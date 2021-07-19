{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat where

import Control.Applicative ((<|>))
import Control.Logger.Simple
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (drgNew)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Controller
import Simplex.Chat.Help
import Simplex.Chat.Input
import Simplex.Chat.Notification
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.Chat.Protocol
import Simplex.Chat.Store
import Simplex.Chat.Styled (plain)
import Simplex.Chat.Terminal
import Simplex.Chat.Types
import Simplex.Chat.View
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..))
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (raceAny_)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async (race_)
import UnliftIO.STM

data ChatCommand
  = ChatHelp
  | MarkdownHelp
  | AddContact
  | Connect SMPQueueInfo
  | DeleteContact ContactName
  | SendMessage ContactName ByteString
  | NewGroup GroupProfile
  | AddMember GroupName ContactName GroupMemberRole
  | JoinGroup GroupName
  | RemoveMember GroupName ContactName
  | MemberRole GroupName ContactName GroupMemberRole
  | LeaveGroup GroupName
  | DeleteGroup GroupName
  | ListMembers GroupName
  | SendGroupMessage GroupName ByteString
  deriving (Show)

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- agent does not listen to TCP
      smpServers = undefined, -- filled in from options
      rsaKeySize = 2048 `div` 8,
      connIdBytes = 12,
      tbqSize = 16,
      dbFile = undefined, -- filled in from options
      dbPoolSize = 4,
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

simplexChat :: WithTerminal t => ChatOpts -> t -> IO ()
simplexChat opts t =
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $ do
  initializeNotifications
    >>= newChatController opts t
    >>= runSimplexChat

newChatController :: WithTerminal t => ChatOpts -> t -> (Notification -> IO ()) -> IO ChatController
newChatController ChatOpts {dbFile, smpServers} t sendNotification = do
  chatStore <- createStore (dbFile <> ".chat.db") 4
  currentUser <- getCreateActiveUser chatStore
  chatTerminal <- newChatTerminal t
  smpAgent <- getSMPAgentClient cfg {dbFile = dbFile <> ".agent.db", smpServers}
  idsDrg <- newTVarIO =<< drgNew
  inputQ <- newTBQueueIO $ tbqSize cfg
  notifyQ <- newTBQueueIO $ tbqSize cfg
  pure ChatController {currentUser, smpAgent, chatTerminal, chatStore, idsDrg, inputQ, notifyQ, sendNotification}

runSimplexChat :: ChatController -> IO ()
runSimplexChat = runReaderT (race_ runTerminalInput runChatController)

runChatController :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
runChatController =
  raceAny_
    [ inputSubscriber,
      agentSubscriber,
      notificationSubscriber
    ]

inputSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
inputSubscriber = do
  q <- asks inputQ
  forever $
    atomically (readTBQueue q) >>= \case
      InputControl _ -> pure ()
      InputCommand s ->
        case parseAll chatCommandP . encodeUtf8 $ T.pack s of
          Left e -> printToView [plain s, "invalid input: " <> plain e]
          Right cmd -> do
            case cmd of
              SendMessage c msg -> showSentMessage c msg
              SendGroupMessage g msg -> showSentGroupMessage g msg
              _ -> printToView [plain s]
            user <- asks currentUser
            void . runExceptT $ processChatCommand user cmd `catchError` showChatError

processChatCommand :: ChatMonad m => User -> ChatCommand -> m ()
processChatCommand user@User {userId, profile} = \case
  ChatHelp -> printToView chatHelpInfo
  MarkdownHelp -> printToView markdownInfo
  AddContact -> do
    (connId, qInfo) <- withAgent createConnection
    withStore $ \st -> createDirectConnection st userId connId
    showInvitation qInfo
  Connect qInfo -> do
    connId <- withAgent $ \a -> joinConnection a qInfo . directMessage $ XInfo profile
    withStore $ \st -> createDirectConnection st userId connId
  DeleteContact cName -> do
    conns <- withStore $ \st -> getContactConnections st userId cName
    withAgent $ \a -> forM_ conns $ \Connection {agentConnId} ->
      deleteConnection a agentConnId `catchError` \(_ :: AgentErrorType) -> pure ()
    withStore $ \st -> deleteContact st userId cName
    unsetActive $ ActiveC cName
    showContactDeleted cName
  SendMessage cName msg -> do
    contact <- withStore $ \st -> getContact st userId cName
    let msgEvent = XMsgNew $ MsgContent MTText [] [MsgContentBody {contentType = SimplexContentType XCText, contentData = msg}]
    sendDirectMessage (contactConnId contact) msgEvent
    setActive $ ActiveC cName
  NewGroup gProfile -> do
    gVar <- asks idsDrg
    group <- withStore $ \st -> createNewGroup st gVar user gProfile
    showGroupCreated group
  AddMember gName cName memRole -> do
    (group, contact) <- withStore $ \st -> (,) <$> getGroup st user gName <*> getContact st userId cName
    let Group {groupId, groupProfile, membership, members} = group
        userRole = memberRole membership
        userMemberId = memberId membership
    when (userRole < GRAdmin || userRole < memRole) $ throwError $ ChatError CEGroupUserRole
    when (isMember contact members) . throwError . ChatError $ CEGroupDuplicateMember cName
    when (memberStatus membership == GSMemInvited) . throwError . ChatError $ CEGroupNotJoined gName
    unless (memberActive membership) . throwError . ChatError $ CEGroupMemberNotActive
    gVar <- asks idsDrg
    (agentConnId, qInfo) <- withAgent createConnection
    GroupMember {memberId} <- withStore $ \st -> createContactGroupMember st gVar user groupId contact memRole agentConnId
    let msg = XGrpInv $ GroupInvitation (userMemberId, userRole) (memberId, memRole) qInfo groupProfile
    sendDirectMessage (contactConnId contact) msg
    showSentGroupInvitation group cName
    setActive $ ActiveG gName
  JoinGroup gName -> do
    ReceivedGroupInvitation {fromMember, userMember, queueInfo} <- withStore $ \st -> getGroupInvitation st user gName
    agentConnId <- withAgent $ \a -> joinConnection a queueInfo . directMessage . XGrpAcpt $ memberId userMember
    withStore $ \st -> do
      createMemberConnection st userId (groupMemberId fromMember) agentConnId
      updateGroupMemberStatus st userId (groupMemberId fromMember) GSMemAccepted
      updateGroupMemberStatus st userId (groupMemberId userMember) GSMemAccepted
  MemberRole _gName _cName _mRole -> pure ()
  RemoveMember _gName _cName -> pure ()
  LeaveGroup _gName -> pure ()
  DeleteGroup _gName -> pure ()
  ListMembers _gName -> pure ()
  SendGroupMessage gName msg -> do
    -- TODO save sent messages
    -- TODO save pending message delivery for members without connections
    Group {members} <- withStore $ \st -> getGroup st user gName
    let msgEvent = XMsgNew $ MsgContent MTText [] [MsgContentBody {contentType = SimplexContentType XCText, contentData = msg}]
    sendGroupMessage members msgEvent
    setActive $ ActiveG gName
  where
    isMember :: Contact -> [GroupMember] -> Bool
    isMember Contact {contactId} members = isJust $ find ((== Just contactId) . memberContactId) members

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
agentSubscriber = do
  q <- asks $ subQ . smpAgent
  forever $ do
    (_, connId, msg) <- atomically $ readTBQueue q
    user <- asks currentUser
    -- TODO handle errors properly
    void . runExceptT $ processAgentMessage user connId msg `catchError` (liftIO . print)

processAgentMessage :: forall m. ChatMonad m => User -> ConnId -> ACommand 'Agent -> m ()
processAgentMessage user@User {userId, profile} agentConnId agentMessage = do
  chatDirection <- withStore $ \st -> getConnectionChatDirection st user agentConnId
  case chatDirection of
    ReceivedDMContact ct@Contact {localDisplayName = c, activeConn} ->
      case agentMessage of
        MSG meta msgBody -> do
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage msgBody
          case chatMsgEvent of
            XMsgNew (MsgContent MTText [] body) -> newTextMessage c meta $ find (isSimplexContentType XCText) body
            XInfo _ -> pure () -- TODO profile update
            XGrpInv gInv -> processGroupInvitation ct gInv
            _ -> pure ()
        CONF confId connInfo -> do
          -- confirming direct connection with a member
          withStore $ \st -> updateConnectionStatus st activeConn ConnRequested
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage connInfo
          case chatMsgEvent of
            XGrpMemInfo _memId _memProfile -> do
              -- TODO check member ID
              -- TODO update member profile
              acceptAgentConnection activeConn confId XOk
            _ -> messageError "CONF from member must have x.grp.mem.info"
        INFO connInfo -> do
          withStore $ \st -> updateConnectionStatus st activeConn ConnSndReady
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage connInfo
          case chatMsgEvent of
            XGrpMemInfo _memId _memProfile -> do
              -- TODO check member ID
              -- TODO update member profile
              pure ()
            XOk -> pure ()
            _ -> messageError "INFO from member must have x.grp.mem.info or x.ok"
        CON -> do
          withStore $ \st -> updateConnectionStatus st activeConn ConnReady
          showContactConnected ct
          showToast (c <> "> ") "connected"
          setActive $ ActiveC c
        END -> do
          showContactDisconnected c
          showToast (c <> "> ") "disconnected"
          unsetActive $ ActiveC c
        _ -> messageError $ "unexpected agent event: " <> T.pack (show agentMessage)
    ReceivedDMConnection conn ->
      case agentMessage of
        CONF confId connInfo -> do
          withStore $ \st -> updateConnectionStatus st conn ConnRequested
          saveConnInfo conn connInfo
          acceptAgentConnection conn confId $ XInfo profile
        INFO connInfo -> do
          withStore $ \st -> updateConnectionStatus st conn ConnSndReady
          saveConnInfo conn connInfo
        CON ->
          withStore $ \st -> updateConnectionStatus st conn ConnReady
        _ -> messageError $ "unsupported agent event: " <> T.pack (show agentMessage)
    ReceivedGroupMessage conn gName m ->
      case agentMessage of
        CONF confId connInfo -> do
          withStore $ \st -> updateConnectionStatus st conn ConnRequested
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage connInfo
          case memberCategory m of
            GCInviteeMember ->
              case chatMsgEvent of
                XGrpAcpt memId
                  | memId == memberId m -> do
                    withStore $ \st -> updateGroupMemberStatus st userId (groupMemberId m) GSMemAccepted
                    acceptAgentConnection conn confId XOk
                  | otherwise -> messageError "x.grp.acpt: memberId is different from expected"
                _ -> messageError "CONF from invited member must have x.grp.acpt"
            _ ->
              case chatMsgEvent of
                XGrpMemInfo memId _memProfile
                  | memId == memberId m -> do
                    -- TODO update member profile
                    Group {membership} <- withStore $ \st -> getGroup st user gName
                    acceptAgentConnection conn confId $ XGrpMemInfo (memberId membership) profile
                  | otherwise -> messageError "x.grp.mem.info: memberId is different from expected"
                _ -> messageError "CONF from member must have x.grp.mem.info"
        INFO connInfo -> do
          withStore $ \st -> updateConnectionStatus st conn ConnSndReady
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage connInfo
          case chatMsgEvent of
            XGrpMemInfo memId _memProfile
              | memId == memberId m -> do
                -- TODO update member profile
                pure ()
              | otherwise -> messageError "x.grp.mem.info: memberId is different from expected"
            XOk -> pure ()
            _ -> messageError "INFO from member must have x.grp.mem.info"
          pure ()
        CON -> do
          group@Group {members, membership} <- withStore $ \st -> getGroup st user gName
          withStore $ \st -> do
            updateConnectionStatus st conn ConnReady
            updateGroupMemberStatus st userId (groupMemberId m) GSMemConnected
            updateGroupMemberStatus st userId (groupMemberId membership) GSMemConnected
          -- TODO forward any pending (GMIntroInvReceived) introductions
          case memberCategory m of
            GCHostMember -> do
              showUserJoinedGroup gName
              setActive $ ActiveG gName
            GCInviteeMember -> do
              showJoinedGroupMember gName m
              intros <- withStore $ \st -> createIntroductions st group m
              sendGroupMessage members . XGrpMemNew $ memberInfo m
              forM_ intros $ \intro -> do
                sendDirectMessage agentConnId . XGrpMemIntro . memberInfo $ reMember intro
                withStore $ \st -> updateIntroStatus st intro GMIntroSent
            GCPreMember -> do
              -- TODO send probe and decide whether to use existing contact connection or the new contact connection
              -- TODO notify member who forwarded introduction - question - where it is stored? There is via_contact but probably there should be via_member in group_members table
              showConnectedToGroupMember gName m
            GCPostMember -> do
              -- TODO notify member who forwarded introduction - question - where it is stored? There is via_contact but probably there should be via_member in group_members table
              showConnectedToGroupMember gName m
            GCUserMember ->
              messageError "implementation error - CON received from User member"
        MSG meta msgBody -> do
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage msgBody
          case chatMsgEvent of
            XMsgNew (MsgContent MTText [] body) ->
              newGroupTextMessage gName m meta $ find (isSimplexContentType XCText) body
            XGrpMemNew memInfo@(MemberInfo memId _ _) -> do
              group@Group {membership} <- withStore $ \st -> getGroup st user gName
              when (memberId membership /= memId) $
                if isMember memId group
                  then messageError "x.grp.mem.new error: member already exists"
                  else do
                    newMember <- withStore $ \st -> createNewGroupMember st user group memInfo GCPostMember GSMemAnnounced
                    showJoinedGroupMemberConnecting gName m newMember
            XGrpMemIntro memInfo@(MemberInfo memId _ _) ->
              case memberCategory m of
                GCHostMember -> do
                  group <- withStore $ \st -> getGroup st user gName
                  if isMember memId group
                    then messageWarning "x.grp.mem.intro ignored: member already exists"
                    else do
                      (groupConnId, groupQInfo) <- withAgent createConnection
                      (directConnId, directQInfo) <- withAgent createConnection
                      newMember <- withStore $ \st -> createIntroReMember st user group m memInfo groupConnId directConnId
                      let msg = XGrpMemInv memId IntroInvitation {groupQInfo, directQInfo}
                      sendDirectMessage agentConnId msg
                      withStore $ \st -> updateGroupMemberStatus st userId (groupMemberId newMember) GSMemIntroInvited
                _ -> messageError "x.grp.mem.intro can be only sent by host member"
            XGrpMemInv memId introInv ->
              case memberCategory m of
                GCInviteeMember -> do
                  group <- withStore $ \st -> getGroup st user gName
                  case find ((== memId) . memberId) $ members group of
                    Nothing -> messageError "x.grp.mem.inv error: referenced member does not exists"
                    Just reMember -> do
                      intro <- withStore $ \st -> saveIntroInvitation st reMember m introInv
                      case activeConn (reMember :: GroupMember) of
                        Nothing -> pure () -- this is not an error, introduction will be forwarded once the member is connected
                        Just Connection {agentConnId = reAgentConnId} -> do
                          sendDirectMessage reAgentConnId $ XGrpMemFwd (memberInfo m) introInv
                          withStore $ \st -> updateIntroStatus st intro GMIntroInvForwarded
                _ -> messageError "x.grp.mem.inv can be only sent by invitee member"
            XGrpMemFwd memInfo@(MemberInfo memId _ _) introInv@IntroInvitation {groupQInfo, directQInfo} -> do
              group@Group {membership} <- withStore $ \st -> getGroup st user gName
              toMember <- case find ((== memId) . memberId) $ members group of
                -- TODO if the missed messages are correctly sent as soon as there is connection before anything else is sent
                -- the situation when member does not exist is an error
                -- member receiving x.grp.mem.fwd should have also received x.grp.mem.new prior to that.
                -- For now, this branch compensates for the lack of delayed message delivery.
                Nothing -> withStore $ \st -> createNewGroupMember st user group memInfo GCPostMember GSMemAnnounced
                Just m' -> pure m'
              withStore $ \st -> saveMemberInvitation st toMember introInv
              let msg = XGrpMemInfo (memberId membership) profile
              groupConnId <- withAgent $ \a -> joinConnection a groupQInfo $ directMessage msg
              directConnId <- withAgent $ \a -> joinConnection a directQInfo $ directMessage msg
              withStore $ \st -> createIntroToMemberContact st userId m toMember groupConnId directConnId
            _ -> messageError $ "unsupported message: " <> T.pack (show chatMsgEvent)
        _ -> messageError $ "unsupported agent event: " <> T.pack (show agentMessage)
  where
    isMember :: MemberId -> Group -> Bool
    isMember memId Group {membership, members} =
      memberId membership == memId || isJust (find ((== memId) . memberId) members)

    messageWarning :: Text -> m ()
    messageWarning = liftIO . print

    messageError :: Text -> m ()
    messageError = liftIO . print

    newTextMessage :: ContactName -> MsgMeta -> Maybe MsgContentBody -> m ()
    newTextMessage c meta = \case
      Just MsgContentBody {contentData = bs} -> do
        let text = safeDecodeUtf8 bs
        showReceivedMessage c (snd $ broker meta) text (integrity meta)
        showToast (c <> "> ") text
        setActive $ ActiveC c
      _ -> messageError "x.msg.new: no expected message body"

    newGroupTextMessage :: GroupName -> GroupMember -> MsgMeta -> Maybe MsgContentBody -> m ()
    newGroupTextMessage gName GroupMember {localDisplayName = c} meta = \case
      Just MsgContentBody {contentData = bs} -> do
        let text = safeDecodeUtf8 bs
        showReceivedGroupMessage gName c (snd $ broker meta) text (integrity meta)
        showToast ("#" <> gName <> " " <> c <> "> ") text
        setActive $ ActiveG gName
      _ -> messageError "x.msg.new: no expected message body"

    processGroupInvitation :: Contact -> GroupInvitation -> m ()
    processGroupInvitation ct@Contact {localDisplayName} inv@(GroupInvitation (fromMemId, fromRole) (memId, memRole) _ _) = do
      when (fromRole < GRAdmin || fromRole < memRole) . throwError . ChatError $ CEGroupContactRole localDisplayName
      when (fromMemId == memId) $ throwError $ ChatError CEGroupDuplicateMemberId
      group <- withStore $ \st -> createGroupInvitation st user ct inv
      showReceivedGroupInvitation group localDisplayName memRole

    parseChatMessage :: ByteString -> Either ChatError ChatMessage
    parseChatMessage msgBody = first ChatErrorMessage (parseAll rawChatMessageP msgBody >>= toChatMessage)

    saveConnInfo :: Connection -> ConnInfo -> m ()
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage connInfo
      case chatMsgEvent of
        XInfo p ->
          withStore $ \st -> createDirectContact st userId activeConn p
        _ -> pure () -- TODO show/log error, other events in SMP confirmation

sendDirectMessage :: ChatMonad m => ConnId -> ChatMsgEvent -> m ()
sendDirectMessage agentConnId chatMsgEvent =
  void . withAgent $ \a -> sendMessage a agentConnId $ directMessage chatMsgEvent

directMessage :: ChatMsgEvent -> ByteString
directMessage chatMsgEvent =
  serializeRawChatMessage $
    rawChatMessage ChatMessage {chatMsgId = Nothing, chatMsgEvent, chatDAG = Nothing}

sendGroupMessage :: ChatMonad m => [GroupMember] -> ChatMsgEvent -> m ()
sendGroupMessage members chatMsgEvent = do
  let msg = directMessage chatMsgEvent
  -- TODO once scheduled delivery is implemented memberActive should be changed to memberCurrent
  withAgent $ \a ->
    forM_ (filter memberActive members) $
      traverse (\connId -> sendMessage a connId msg) . memberConnId

acceptAgentConnection :: ChatMonad m => Connection -> ConfirmationId -> ChatMsgEvent -> m ()
acceptAgentConnection conn@Connection {agentConnId} confId msg = do
  withAgent $ \a -> allowConnection a agentConnId confId $ directMessage msg
  withStore $ \st -> updateConnectionStatus st conn ConnAccepted

getCreateActiveUser :: SQLiteStore -> IO User
getCreateActiveUser st = do
  user <-
    getUsers st >>= \case
      [] -> newUser
      users -> maybe (selectUser users) pure (find activeUser users)
  putStrLn $ "Current user: " <> userStr user
  pure user
  where
    newUser :: IO User
    newUser = do
      putStrLn
        "No user profiles found, it will be created now.\n\
        \Please choose your display name and your full name.\n\
        \They will be sent to your contacts when you connect.\n\
        \They are only stored on your device and you can change them later."
      loop
      where
        loop = do
          displayName <- getContactName
          fullName <- T.pack <$> getWithPrompt "full name (optional)"
          liftIO (runExceptT $ createUser st Profile {displayName, fullName} True) >>= \case
            Left SEDuplicateName -> do
              putStrLn "chosen display name is already used by another profile on this device, choose another one"
              loop
            Left e -> putStrLn ("database error " <> show e) >> exitFailure
            Right user -> pure user
    selectUser :: [User] -> IO User
    selectUser [user] = do
      liftIO $ setActiveUser st (userId user)
      pure user
    selectUser users = do
      putStrLn "Select user profile:"
      forM_ (zip [1 ..] users) $ \(n :: Int, user) -> putStrLn $ show n <> " - " <> userStr user
      loop
      where
        loop = do
          nStr <- getWithPrompt $ "user profile number (1 .. " <> show (length users) <> ")"
          case readMaybe nStr :: Maybe Int of
            Nothing -> putStrLn "invalid user number" >> loop
            Just n
              | n <= 0 || n > length users -> putStrLn "invalid user number" >> loop
              | otherwise -> do
                let user = users !! (n - 1)
                liftIO $ setActiveUser st (userId user)
                pure user
    userStr :: User -> String
    userStr User {localDisplayName, profile = Profile {fullName}} =
      T.unpack $ localDisplayName <> if T.null fullName then "" else " (" <> fullName <> ")"
    getContactName :: IO ContactName
    getContactName = do
      displayName <- getWithPrompt "display name (no spaces)"
      if null displayName || isJust (find (== ' ') displayName)
        then putStrLn "display name has space(s), choose another one" >> getContactName
        else pure $ T.pack displayName
    getWithPrompt :: String -> IO String
    getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

showToast :: (MonadUnliftIO m, MonadReader ChatController m) => Text -> Text -> m ()
showToast title text = atomically . (`writeTBQueue` Notification {title, text}) =<< asks notifyQ

notificationSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
notificationSubscriber = do
  ChatController {notifyQ, sendNotification} <- ask
  forever $ atomically (readTBQueue notifyQ) >>= liftIO . sendNotification

withAgent :: ChatMonad m => (AgentClient -> ExceptT AgentErrorType m a) -> m a
withAgent action =
  asks smpAgent
    >>= runExceptT . action
    >>= liftEither . first ChatErrorAgent

withStore ::
  ChatMonad m =>
  (forall m'. (MonadUnliftIO m', MonadError StoreError m') => SQLiteStore -> m' a) ->
  m a
withStore action =
  asks chatStore
    >>= runExceptT . action
    >>= liftEither . first ChatErrorStore

chatCommandP :: Parser ChatCommand
chatCommandP =
  ("/help" <|> "/h") $> ChatHelp
    <|> ("/group #" <|> "/group " <|> "/g #" <|> "/g ") *> (NewGroup <$> groupProfile)
    <|> ("/add #" <|> "/add " <|> "/a #" <|> "/a ") *> (AddMember <$> displayName <* A.space <*> displayName <*> memberRole)
    <|> ("/join #" <|> "/join " <|> "/j #" <|> "/j ") *> (JoinGroup <$> displayName)
    <|> ("/remove #" <|> "/rm #") *> (RemoveMember <$> displayName <* A.space <*> displayName)
    <|> ("/delete #" <|> "/d #") *> (DeleteGroup <$> displayName)
    <|> ("/members #" <|> "/ms #") *> (ListMembers <$> displayName)
    <|> A.char '#' *> (SendGroupMessage <$> displayName <* A.space <*> A.takeByteString)
    <|> ("/add" <|> "/a") $> AddContact
    <|> ("/connect " <|> "/c ") *> (Connect <$> smpQueueInfoP)
    <|> ("/delete @" <|> "/delete " <|> "/d @" <|> "/d ") *> (DeleteContact <$> displayName)
    <|> A.char '@' *> (SendMessage <$> displayName <*> (A.space *> A.takeByteString))
    <|> ("/markdown" <|> "/m") $> MarkdownHelp
  where
    displayName = safeDecodeUtf8 <$> (B.cons <$> A.satisfy refChar <*> A.takeTill (== ' '))
    refChar c = c > ' ' && c /= '#' && c /= '@'
    groupProfile = do
      gName <- displayName
      fullName' <- safeDecodeUtf8 <$> (A.space *> A.takeByteString) <|> pure ""
      pure GroupProfile {displayName = gName, fullName = if T.null fullName' then gName else fullName'}
    memberRole =
      (" owner" $> GROwner)
        <|> (" admin" $> GRAdmin)
        <|> (" normal" $> GRMember)
        <|> pure GRAdmin
