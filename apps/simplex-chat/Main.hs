{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ChatOptions
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Input
import Simplex.Messaging.Agent (getSMPAgentClient)
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Notification
import Simplex.Store (createStore)
import Simplex.Terminal
import System.Directory (getAppUserDataDirectory)
import UnliftIO.Async (race_)

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- agent does not listen to TCP
      smpServers = undefined, -- filled in from options
      rsaKeySize = 2048 `div` 8,
      connIdBytes = 12,
      tbqSize = 16,
      dbFile = "smp-chat.db",
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

main :: IO ()
main = do
  ChatOpts {dbFile, smpServers} <- welcomeGetOpts
  void $ createStore "simplex-chat.db" 4
  ct <- newChatTerminal
  a <- getSMPAgentClient cfg {dbFile, smpServers}
  notify <- initializeNotifications
  cc <- atomically $ newChatController a ct notify $ tbqSize cfg
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $ do
  runReaderT simplexChat cc

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFile} <- getChatOpts appDir
  putStrLn "SimpleX chat prototype v0.3.1"
  putStrLn $ "db: " <> dbFile
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts

simplexChat :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
simplexChat = race_ runTerminalInput runChatController

-- defaultSettings :: C.Size -> C.VirtualTerminalSettings
-- defaultSettings size =
--   C.VirtualTerminalSettings
--     { C.virtualType = "xterm",
--       C.virtualWindowSize = pure size,
--       C.virtualEvent = retry,
--       C.virtualInterrupt = retry
--     }

-- main :: IO ()
-- main = do
--   void $ createStore "simplex-chat.db" 4

--   hFlush stdout
--   -- ChatTerminal {termSize} <- newChatTerminal
--   -- pos <- C.withVirtualTerminal (defaultSettings termSize) $
--   --   \t -> runTerminalT (C.setAlternateScreenBuffer True >> C.putString "a" >> C.flush >> C.getCursorPosition) t
--   -- print pos
--   -- race_ (printEvents t) (updateTerminal t)
--   void . withTerminal . runTerminalT $ chatLayoutDemo >> C.flush >> C.awaitEvent

-- printEvents :: C.VirtualTerminal -> IO ()
-- printEvents t = forever $ do
--   event <- withTerminal . runTerminalT $ C.flush >> C.awaitEvent
--   runTerminalT (putStringLn $ show event) t

-- updateTerminal :: C.VirtualTerminal -> IO ()
-- updateTerminal t = forever $ do
--   threadDelay 10000
--   win <- readTVarIO $ C.virtualWindow t
--   withTerminal . runTerminalT $ mapM_ C.putStringLn win >> C.flush
