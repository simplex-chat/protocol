{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.View
  ( printToView,
    showInvitation,
    showAgentError,
    showContactDeleted,
    showContactConnected,
    showContactDisconnected,
    showReceivedMessage,
    showSentMessage,
    ttyContact,
    ttyFromContact,
    ttyGroup,
    ttyFromGroup,
    safeDecodeUtf8,
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Composition ((.:))
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, ZonedTime, getCurrentTimeZone, getZonedTime, localDay, localTimeOfDay, timeOfDayToTime, utcToLocalTime, zonedTimeToLocalTime)
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import Simplex.Messaging.Agent.Protocol
import Simplex.Terminal (printToTerminal)
import Simplex.Util (safeDecodeUtf8)
import System.Console.ANSI.Types
import Types

type ChatReader m = (MonadUnliftIO m, MonadReader ChatController m)

showInvitation :: ChatReader m => Contact -> SMPQueueInfo -> m ()
showInvitation = printToView .: invitation

showAgentError :: ChatReader m => Contact -> AgentErrorType -> m ()
showAgentError = printToView .: agentError

showContactDeleted :: ChatReader m => Contact -> m ()
showContactDeleted = printToView . contactDeleted

showContactConnected :: ChatReader m => Contact -> m ()
showContactConnected = printToView . contactConnected

showContactDisconnected :: ChatReader m => Contact -> m ()
showContactDisconnected = printToView . contactDisconnected

showReceivedMessage :: ChatReader m => Contact -> UTCTime -> ByteString -> MsgIntegrity -> m ()
showReceivedMessage c utcTime msg mOk = printToView =<< liftIO (receivedMessage c utcTime msg mOk)

showSentMessage :: ChatReader m => Contact -> ByteString -> m ()
showSentMessage c msg = printToView =<< liftIO (sentMessage c msg)

invitation :: Contact -> SMPQueueInfo -> [StyledString]
invitation c qInfo =
  [ "pass this invitation to your contact " <> ttyContact c <> " (via any channel): ",
    "",
    (bPlain . serializeSmpQueueInfo) qInfo,
    "",
    "and ask them to connect: /c <name_for_you> <invitation_above>"
  ]

contactDeleted :: Contact -> [StyledString]
contactDeleted c = [ttyContact c <> " is deleted"]

contactConnected :: Contact -> [StyledString]
contactConnected c = [ttyContact c <> " is connected"]

contactDisconnected :: Contact -> [StyledString]
contactDisconnected c = ["disconnected from " <> ttyContact c <> " - restart chat"]

receivedMessage :: Contact -> UTCTime -> ByteString -> MsgIntegrity -> IO [StyledString]
receivedMessage c utcTime msg mOk = do
  t <- formatUTCTime <$> getCurrentTimeZone <*> getZonedTime
  pure $ prependFirst (t <> " " <> ttyFromContact c) (msgPlain msg) ++ showIntegrity mOk
  where
    formatUTCTime :: TimeZone -> ZonedTime -> StyledString
    formatUTCTime localTz currentTime =
      let localTime = utcToLocalTime localTz utcTime
          format =
            if (localDay localTime < localDay (zonedTimeToLocalTime currentTime))
              && (timeOfDayToTime (localTimeOfDay localTime) > (6 * 60 * 60 :: DiffTime))
              then "%m-%d" -- if message is from yesterday or before and 6 hours has passed since midnight
              else "%H:%M"
       in styleTime $ formatTime defaultTimeLocale format localTime
    showIntegrity :: MsgIntegrity -> [StyledString]
    showIntegrity MsgOk = []
    showIntegrity (MsgError err) = msgError $ case err of
      MsgSkipped fromId toId ->
        "skipped message ID " <> show fromId
          <> if fromId == toId then "" else ".." <> show toId
      MsgBadId msgId -> "unexpected message ID " <> show msgId
      MsgBadHash -> "incorrect message hash"
      MsgDuplicate -> "duplicate message ID"
    msgError :: String -> [StyledString]
    msgError s = [styled (Colored Red) s]

sentMessage :: Contact -> ByteString -> IO [StyledString]
sentMessage c msg = do
  time <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  pure $ prependFirst (styleTime time <> " " <> ttyToContact c) (msgPlain msg)

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: ByteString -> [StyledString]
msgPlain = map styleMarkdownText . T.lines . safeDecodeUtf8

agentError :: Contact -> AgentErrorType -> [StyledString]
agentError c = \case
  CONN e -> case e of
    NOT_FOUND -> ["no contact " <> ttyContact c]
    DUPLICATE -> ["contact " <> ttyContact c <> " already exists"]
    SIMPLEX -> ["contact " <> ttyContact c <> " did not accept invitation yet"]
  e -> ["chat error: " <> plain (show e)]

printToView :: (MonadUnliftIO m, MonadReader ChatController m) => [StyledString] -> m ()
printToView s = asks chatTerminal >>= liftIO . (`printToTerminal` s)

ttyContact :: Contact -> StyledString
ttyContact (Contact a) = styled (Colored Green) a

ttyToContact :: Contact -> StyledString
ttyToContact (Contact a) = styled (Colored Cyan) $ a <> " "

ttyFromContact :: Contact -> StyledString
ttyFromContact (Contact a) = styled (Colored Yellow) $ a <> "> "

ttyGroup :: Group -> StyledString
ttyGroup (Group g) = styled (Colored Blue) $ "#" <> g

ttyFromGroup :: Group -> Contact -> StyledString
ttyFromGroup (Group g) (Contact a) = styled (Colored Yellow) $ "#" <> g <> " " <> a <> "> "

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]
