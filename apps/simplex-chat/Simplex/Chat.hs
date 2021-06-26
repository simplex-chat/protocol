{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Controller
import Simplex.Chat.Styled (plain)
import Simplex.Help
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (raceAny_)
import Simplex.Notification
import Simplex.Terminal
import Simplex.View
import Types
import UnliftIO.STM

data ChatCommand
  = ChatHelp
  | MarkdownHelp
  | AddContact Contact
  | Connect Contact SMPQueueInfo
  | DeleteContact Contact
  | SendMessage Contact ByteString
  deriving (Show)

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
              _ -> printToView [plain s]
            runExceptT (processChatCommand cmd) >>= \case
              Left (ChatErrorAgent c e) -> showAgentError c e
              _ -> pure ()

processChatCommand :: ChatMonad m => ChatCommand -> m ()
processChatCommand = \case
  ChatHelp -> printToView chatHelpInfo
  MarkdownHelp -> printToView markdownInfo
  AddContact c -> do
    (_, qInfo) <- withAgent c (`createConnection` Just (fromContact c))
    showInvitation c qInfo
  Connect c qInfo ->
    void . withAgent c $ \smp -> joinConnection smp (Just $ fromContact c) qInfo
  DeleteContact c -> do
    withAgent c (`deleteConnection` fromContact c)
    showContactDeleted c
    unsetActive' $ ActiveC c
  SendMessage c msg -> do
    void . withAgent c $ \smp -> sendMessage smp (fromContact c) msg
    setActive' $ ActiveC c

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
agentSubscriber = do
  q <- asks $ subQ . smpAgent
  nQ <- asks notifyQ
  forever $ do
    (_, a, resp) <- atomically (readTBQueue q)
    let notify = \text -> atomically $ writeTBQueue nQ Notification {title = "@" <> a, text}
    case resp of
      CON -> do
        showContactConnected $ Contact a
        setActive' $ ActiveC $ Contact a
      END -> do
        showContactDisconnected $ Contact a
        notify "disconnected"
        unsetActive' $ ActiveC $ Contact a
      MSG {brokerMeta, msgBody, msgIntegrity} -> do
        -- ReceivedMessage contact (snd brokerMeta) msgBody msgIntegrity
        showReceivedMessage (Contact a) (snd brokerMeta) msgBody msgIntegrity
        notify msgBody
        setActive' $ ActiveC $ Contact a
      _ -> pure ()

notificationSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
notificationSubscriber = do
  ChatController {notifyQ, sendNotification} <- ask
  forever $ atomically (readTBQueue notifyQ) >>= liftIO . sendNotification

withAgent :: ChatMonad m => Contact -> (AgentClient -> ExceptT AgentErrorType m a) -> m a
withAgent c action =
  asks smpAgent
    >>= runExceptT . action
    >>= liftEither . first (ChatErrorAgent c)

chatCommandP :: Parser ChatCommand
chatCommandP =
  ("/help" <|> "/h") $> ChatHelp
    <|> ("/add " <|> "/a ") *> (AddContact <$> contact)
    <|> ("/connect " <|> "/c ") *> (Connect <$> contact <* A.space <*> smpQueueInfoP)
    <|> ("/delete " <|> "/d ") *> (DeleteContact <$> contact)
    <|> A.char '@' *> (SendMessage <$> contact <* A.space <*> A.takeByteString)
    <|> ("/markdown" <|> "/m") $> MarkdownHelp
  where
    contact = Contact <$> A.takeTill (== ' ')
