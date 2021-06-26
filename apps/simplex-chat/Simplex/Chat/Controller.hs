{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Controller where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Numeric.Natural
import Simplex.Messaging.Agent (AgentClient)
import Simplex.Messaging.Agent.Protocol (AgentErrorType)
import Simplex.Notification
import Simplex.Terminal
import Types
import UnliftIO.STM

data ChatController = ChatController
  { smpAgent :: AgentClient,
    chatTerminal :: ChatTerminal,
    inputQ :: TBQueue InputEvent,
    notifyQ :: TBQueue Notification,
    sendNotification :: Notification -> IO ()
  }

data InputEvent = InputCommand String | InputControl Char

data ChatError = ChatErrorAgent Contact AgentErrorType
  deriving (Show, Exception)

type ChatMonad m = (MonadUnliftIO m, MonadReader ChatController m, MonadError ChatError m)

newChatController :: AgentClient -> ChatTerminal -> (Notification -> IO ()) -> Natural -> STM ChatController
newChatController smpAgent chatTerminal sendNotification qSize = do
  inputQ <- newTBQueue qSize
  notifyQ <- newTBQueue qSize
  pure ChatController {smpAgent, chatTerminal, inputQ, notifyQ, sendNotification}

setActive' :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
setActive' to = asks (activeTo . chatTerminal) >>= atomically . (`writeTVar` to)

unsetActive' :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
unsetActive' a = asks (activeTo . chatTerminal) >>= atomically . (`modifyTVar` unset)
  where
    unset a' = if a == a' then ActiveNone else a'
