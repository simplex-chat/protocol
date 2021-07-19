{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Protocol where

import Control.Applicative (optional)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.List (find)
import Data.Text (Text)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (bshow)

data ChatDirection (p :: AParty) where
  ReceivedDMConnection :: Connection -> ChatDirection 'Agent
  ReceivedDMContact :: Contact -> ChatDirection 'Agent
  SentDirectMessage :: Contact -> ChatDirection 'Client
  ReceivedGroupMessage :: Connection -> GroupName -> GroupMember -> ChatDirection 'Agent
  SentGroupMessage :: GroupName -> ChatDirection 'Client

deriving instance Eq (ChatDirection p)

deriving instance Show (ChatDirection p)

data ChatMsgEvent
  = XMsgNew MsgContent
  | XInfo Profile
  | XGrpInv GroupInvitation
  | XGrpAcpt MemberId
  | XGrpMemNew MemberInfo
  | XGrpMemIntro MemberInfo
  | XGrpMemInv MemberId IntroInvitation
  | XGrpMemFwd MemberInfo IntroInvitation
  | XGrpMemInfo MemberId Profile
  | XGrpMemCon MemberId
  | XGrpMemConAll MemberId
  | XInfoProbe ByteString
  | XInfoProbeCheck MemberId ByteString
  | XInfoProbeOk MemberId ByteString
  | XOk
  deriving (Eq, Show)

data MessageType = MTText | MTImage deriving (Eq, Show)

data MsgContent = MsgContent
  { messageType :: MessageType,
    files :: [(ContentType, Int)],
    content :: [MsgContentBody]
  }
  deriving (Eq, Show)

toMsgType :: ByteString -> Either String MessageType
toMsgType = \case
  "c.text" -> Right MTText
  "c.image" -> Right MTImage
  t -> Left $ "invalid message type " <> B.unpack t

rawMsgType :: MessageType -> ByteString
rawMsgType = \case
  MTText -> "c.text"
  MTImage -> "c.image"

data ChatMessage = ChatMessage
  { chatMsgId :: Maybe Int64,
    chatMsgEvent :: ChatMsgEvent,
    chatDAG :: Maybe ByteString
  }
  deriving (Eq, Show)

toChatMessage :: RawChatMessage -> Either String ChatMessage
toChatMessage RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody} = do
  (chatDAG, body) <- getDAG <$> mapM toMsgBodyContent chatMsgBody
  let chatMsg msg = pure ChatMessage {chatMsgId, chatMsgEvent = msg, chatDAG}
  case (chatMsgEvent, chatMsgParams) of
    ("x.msg.new", mt : rawFiles) -> do
      t <- toMsgType mt
      files <- mapM (toContentInfo <=< parseAll contentInfoP) rawFiles
      chatMsg . XMsgNew $ MsgContent {messageType = t, files, content = body}
    ("x.info", []) -> do
      profile <- getJSON body
      chatMsg $ XInfo profile
    ("x.grp.inv", [fromMemId, fromRole, memId, role, qInfo]) -> do
      fromMem <- (,) <$> B64.decode fromMemId <*> toMemberRole fromRole
      invitedMem <- (,) <$> B64.decode memId <*> toMemberRole role
      groupQInfo <- parseAll smpQueueInfoP qInfo
      profile <- getJSON body
      chatMsg . XGrpInv $ GroupInvitation fromMem invitedMem groupQInfo profile
    ("x.grp.acpt", [memId]) ->
      chatMsg . XGrpAcpt =<< B64.decode memId
    ("x.grp.mem.new", [memId, role]) -> do
      chatMsg . XGrpMemNew =<< toMemberInfo memId role body
    ("x.grp.mem.intro", [memId, role]) ->
      chatMsg . XGrpMemIntro =<< toMemberInfo memId role body
    ("x.grp.mem.inv", [memId, groupQInfo, directQInfo]) ->
      chatMsg =<< (XGrpMemInv <$> B64.decode memId <*> toIntroInv groupQInfo directQInfo)
    ("x.grp.mem.fwd", [memId, role, groupQInfo, directQInfo]) -> do
      chatMsg =<< (XGrpMemFwd <$> toMemberInfo memId role body <*> toIntroInv groupQInfo directQInfo)
    ("x.grp.mem.info", [memId]) ->
      chatMsg =<< (XGrpMemInfo <$> B64.decode memId <*> getJSON body)
    ("x.grp.mem.con", [memId]) ->
      chatMsg . XGrpMemCon =<< B64.decode memId
    ("x.grp.mem.con.all", [memId]) ->
      chatMsg . XGrpMemConAll =<< B64.decode memId
    ("x.info.probe", [probe]) -> do
      chatMsg . XInfoProbe =<< B64.decode probe
    ("x.info.probe.check", [memId, probeHash]) -> do
      chatMsg =<< (XInfoProbeCheck <$> B64.decode memId <*> B64.decode probeHash)
    ("x.info.probe.ok", [memId, probe]) -> do
      chatMsg =<< (XInfoProbeOk <$> B64.decode memId <*> B64.decode probe)
    ("x.ok", []) ->
      chatMsg XOk
    _ -> Left $ "bad syntax or unsupported event " <> B.unpack chatMsgEvent
  where
    getDAG :: [MsgContentBody] -> (Maybe ByteString, [MsgContentBody])
    getDAG body = case break (isContentType SimplexDAG) body of
      (b, MsgContentBody SimplexDAG dag : a) -> (Just dag, b <> a)
      _ -> (Nothing, body)
    toMemberInfo :: ByteString -> ByteString -> [MsgContentBody] -> Either String MemberInfo
    toMemberInfo memId role body = MemberInfo <$> B64.decode memId <*> toMemberRole role <*> getJSON body
    toIntroInv :: ByteString -> ByteString -> Either String IntroInvitation
    toIntroInv groupQInfo directQInfo = IntroInvitation <$> parseAll smpQueueInfoP groupQInfo <*> parseAll smpQueueInfoP directQInfo
    toContentInfo :: (RawContentType, Int) -> Either String (ContentType, Int)
    toContentInfo (rawType, size) = (,size) <$> toContentType rawType
    getJSON :: FromJSON a => [MsgContentBody] -> Either String a
    getJSON = J.eitherDecodeStrict' <=< getSimplexContentType XCJson

isContentType :: ContentType -> MsgContentBody -> Bool
isContentType t MsgContentBody {contentType = t'} = t == t'

isSimplexContentType :: XContentType -> MsgContentBody -> Bool
isSimplexContentType = isContentType . SimplexContentType

getContentType :: ContentType -> [MsgContentBody] -> Either String ByteString
getContentType t body = case find (isContentType t) body of
  Just MsgContentBody {contentData} -> Right contentData
  Nothing -> Left "no required content type"

getSimplexContentType :: XContentType -> [MsgContentBody] -> Either String ByteString
getSimplexContentType = getContentType . SimplexContentType

rawChatMessage :: ChatMessage -> RawChatMessage
rawChatMessage ChatMessage {chatMsgId, chatMsgEvent, chatDAG} =
  case chatMsgEvent of
    XMsgNew MsgContent {messageType = t, files, content} ->
      let rawFiles = map (serializeContentInfo . rawContentInfo) files
       in rawMsg "x.msg.new" (rawMsgType t : rawFiles) content
    XInfo profile ->
      rawMsg "x.info" [] [jsonBody profile]
    XGrpInv (GroupInvitation (fromMemId, fromRole) (memId, role) qInfo groupProfile) ->
      let params =
            [ B64.encode fromMemId,
              serializeMemberRole fromRole,
              B64.encode memId,
              serializeMemberRole role,
              serializeSmpQueueInfo qInfo
            ]
       in rawMsg "x.grp.inv" params [jsonBody groupProfile]
    XGrpAcpt memId ->
      rawMsg "x.grp.acpt" [B64.encode memId] []
    XGrpMemNew (MemberInfo memId role profile) ->
      let params = [B64.encode memId, serializeMemberRole role]
       in rawMsg "x.grp.mem.new" params [jsonBody profile]
    XGrpMemIntro (MemberInfo memId role profile) ->
      rawMsg "x.grp.mem.intro" [B64.encode memId, serializeMemberRole role] [jsonBody profile]
    XGrpMemInv memId IntroInvitation {groupQInfo, directQInfo} ->
      let params = [B64.encode memId, serializeSmpQueueInfo groupQInfo, serializeSmpQueueInfo directQInfo]
       in rawMsg "x.grp.mem.inv" params []
    XGrpMemFwd (MemberInfo memId role profile) IntroInvitation {groupQInfo, directQInfo} ->
      let params =
            [ B64.encode memId,
              serializeMemberRole role,
              serializeSmpQueueInfo groupQInfo,
              serializeSmpQueueInfo directQInfo
            ]
       in rawMsg "x.grp.mem.fwd" params [jsonBody profile]
    XGrpMemInfo memId profile ->
      rawMsg "x.grp.mem.info" [B64.encode memId] [jsonBody profile]
    XGrpMemCon memId ->
      rawMsg "x.grp.mem.con" [B64.encode memId] []
    XGrpMemConAll memId ->
      rawMsg "x.grp.mem.con.all" [B64.encode memId] []
    XInfoProbe probe ->
      rawMsg "x.info.probe" [B64.encode probe] []
    XInfoProbeCheck memId probeHash ->
      rawMsg "x.info.probe.check" [B64.encode memId, B64.encode probeHash] []
    XInfoProbeOk memId probe ->
      rawMsg "x.info.probe.ok" [B64.encode memId, B64.encode probe] []
    XOk ->
      rawMsg "x.ok" [] []
  where
    rawMsg :: ByteString -> [ByteString] -> [MsgContentBody] -> RawChatMessage
    rawMsg event chatMsgParams body =
      RawChatMessage {chatMsgId, chatMsgEvent = event, chatMsgParams, chatMsgBody = rawWithDAG body}
    rawContentInfo :: (ContentType, Int) -> (RawContentType, Int)
    rawContentInfo (t, size) = (rawContentType t, size)
    jsonBody :: ToJSON a => a -> MsgContentBody
    jsonBody x =
      let json = LB.toStrict $ J.encode x
       in MsgContentBody {contentType = SimplexContentType XCJson, contentData = json}
    rawWithDAG :: [MsgContentBody] -> [RawMsgBodyContent]
    rawWithDAG body = map rawMsgBodyContent $ case chatDAG of
      Nothing -> body
      Just dag -> MsgContentBody {contentType = SimplexDAG, contentData = dag} : body

toMsgBodyContent :: RawMsgBodyContent -> Either String MsgContentBody
toMsgBodyContent RawMsgBodyContent {contentType, contentData} = do
  cType <- toContentType contentType
  pure MsgContentBody {contentType = cType, contentData}

rawMsgBodyContent :: MsgContentBody -> RawMsgBodyContent
rawMsgBodyContent MsgContentBody {contentType = t, contentData} =
  RawMsgBodyContent {contentType = rawContentType t, contentData}

data MsgContentBody = MsgContentBody
  { contentType :: ContentType,
    contentData :: ByteString
  }
  deriving (Eq, Show)

data ContentType
  = SimplexContentType XContentType
  | MimeContentType MContentType
  | SimplexDAG
  deriving (Eq, Show)

data XContentType = XCText | XCImage | XCJson deriving (Eq, Show)

data MContentType = MCImageJPG | MCImagePNG deriving (Eq, Show)

toContentType :: RawContentType -> Either String ContentType
toContentType (RawContentType ns cType) = case ns of
  "x" -> case cType of
    "text" -> Right $ SimplexContentType XCText
    "image" -> Right $ SimplexContentType XCImage
    "json" -> Right $ SimplexContentType XCJson
    "dag" -> Right SimplexDAG
    _ -> err
  "m" -> case cType of
    "image/jpg" -> Right $ MimeContentType MCImageJPG
    "image/png" -> Right $ MimeContentType MCImagePNG
    _ -> err
  _ -> err
  where
    err = Left . B.unpack $ "invalid content type " <> ns <> "." <> cType

rawContentType :: ContentType -> RawContentType
rawContentType t = case t of
  SimplexContentType t' -> RawContentType "x" $ case t' of
    XCText -> "text"
    XCImage -> "image"
    XCJson -> "json"
  MimeContentType t' -> RawContentType "m" $ case t' of
    MCImageJPG -> "image/jpg"
    MCImagePNG -> "image/png"
  SimplexDAG -> RawContentType "x" "dag"

newtype ContentMsg = NewContentMsg ContentData

newtype ContentData = ContentText Text

data RawChatMessage = RawChatMessage
  { chatMsgId :: Maybe Int64,
    chatMsgEvent :: ByteString,
    chatMsgParams :: [ByteString],
    chatMsgBody :: [RawMsgBodyContent]
  }
  deriving (Eq, Show)

data RawMsgBodyContent = RawMsgBodyContent
  { contentType :: RawContentType,
    contentData :: ByteString
  }
  deriving (Eq, Show)

data RawContentType = RawContentType NameSpace ByteString
  deriving (Eq, Show)

type NameSpace = ByteString

newtype MsgData = MsgData ByteString
  deriving (Eq, Show)

class DataLength a where
  dataLength :: a -> Int

rawChatMessageP :: Parser RawChatMessage
rawChatMessageP = do
  chatMsgId <- optional A.decimal <* A.space
  chatMsgEvent <- B.intercalate "." <$> identifierP `A.sepBy1'` A.char '.' <* A.space
  chatMsgParams <- A.takeWhile1 (not . A.inClass ", ") `A.sepBy'` A.char ',' <* A.space
  chatMsgBody <- msgBodyContent =<< contentInfoP `A.sepBy'` A.char ',' <* A.space
  pure RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody}
  where
    msgBodyContent :: [(RawContentType, Int)] -> Parser [RawMsgBodyContent]
    msgBodyContent [] = pure []
    msgBodyContent ((contentType, size) : ps) = do
      contentData <- A.take size <* A.space
      ((RawMsgBodyContent {contentType, contentData}) :) <$> msgBodyContent ps

contentInfoP :: Parser (RawContentType, Int)
contentInfoP = do
  contentType <- RawContentType <$> identifierP <* A.char '.' <*> A.takeTill (A.inClass ":, ")
  size <- A.char ':' *> A.decimal
  pure (contentType, size)

identifierP :: Parser ByteString
identifierP = B.cons <$> A.letter_ascii <*> A.takeWhile (\c -> A.isAlpha_ascii c || A.isDigit c)

serializeRawChatMessage :: RawChatMessage -> ByteString
serializeRawChatMessage RawChatMessage {chatMsgId, chatMsgEvent, chatMsgParams, chatMsgBody} =
  B.unwords
    [ maybe "" bshow chatMsgId,
      chatMsgEvent,
      B.intercalate "," chatMsgParams,
      B.unwords $ map serializeBodyContentInfo chatMsgBody,
      B.unwords $ map msgContentData chatMsgBody
    ]

serializeBodyContentInfo :: RawMsgBodyContent -> ByteString
serializeBodyContentInfo RawMsgBodyContent {contentType = t, contentData} =
  serializeContentInfo (t, B.length contentData)

serializeContentInfo :: (RawContentType, Int) -> ByteString
serializeContentInfo (RawContentType ns cType, size) = ns <> "." <> cType <> ":" <> bshow size

msgContentData :: RawMsgBodyContent -> ByteString
msgContentData RawMsgBodyContent {contentData} = contentData <> " "
