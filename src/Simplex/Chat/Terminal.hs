{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Terminal where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Simplex.Chat.Styled
import Simplex.Chat.Types
import System.Console.ANSI.Types
import System.Terminal
import System.Terminal.Internal (LocalTerminal, Terminal, VirtualTerminal)
import UnliftIO.STM

data ActiveTo = ActiveNone | ActiveC ContactName | ActiveG GroupName
  deriving (Eq)

data ChatTerminal = ChatTerminal
  { activeTo :: TVar ActiveTo,
    termDevice :: TerminalDevice,
    termState :: TVar TerminalState,
    termSize :: Size,
    nextMessageRow :: TVar Int,
    termLock :: TMVar ()
  }

data TerminalState = TerminalState
  { inputPrompt :: String,
    inputString :: String,
    inputPosition :: Int,
    previousInput :: String
  }

class Terminal t => WithTerminal t where
  withTerm :: (MonadIO m, MonadMask m) => t -> (t -> m a) -> m a

data TerminalDevice = forall t. WithTerminal t => TerminalDevice t

instance WithTerminal LocalTerminal where
  withTerm _ = withTerminal

instance WithTerminal VirtualTerminal where
  withTerm t = ($ t)

withChatTerm :: (MonadIO m, MonadMask m) => ChatTerminal -> (forall t. WithTerminal t => TerminalT t m a) -> m a
withChatTerm ChatTerminal {termDevice = TerminalDevice t} action = withTerm t $ runTerminalT action

newChatTerminal :: WithTerminal t => t -> IO ChatTerminal
newChatTerminal t = do
  activeTo <- newTVarIO ActiveNone
  termSize <- withTerm t . runTerminalT $ getWindowSize
  let lastRow = height termSize - 1
  termState <- newTVarIO newTermState
  termLock <- newTMVarIO ()
  nextMessageRow <- newTVarIO lastRow
  -- threadDelay 500000 -- this delay is the same as timeout in getTerminalSize
  return ChatTerminal {activeTo, termDevice = TerminalDevice t, termState, termSize, nextMessageRow, termLock}

newTermState :: TerminalState
newTermState =
  TerminalState
    { inputString = "",
      inputPosition = 0,
      inputPrompt = "> ",
      previousInput = ""
    }

withTermLock :: MonadTerminal m => ChatTerminal -> m () -> m ()
withTermLock ChatTerminal {termLock} action = do
  _ <- atomically $ takeTMVar termLock
  action
  atomically $ putTMVar termLock ()

printToTerminal :: ChatTerminal -> [StyledString] -> IO ()
printToTerminal ct s =
  withChatTerm ct $
    withTermLock ct $ do
      printMessage ct s
      updateInput ct

updateInput :: forall m. MonadTerminal m => ChatTerminal -> m ()
updateInput ChatTerminal {termSize = Size {height, width}, termState, nextMessageRow} = do
  hideCursor
  ts <- readTVarIO termState
  nmr <- readTVarIO nextMessageRow
  let ih = inputHeight ts
      iStart = height - ih
      prompt = inputPrompt ts
      Position {row, col} = positionRowColumn width $ length prompt + inputPosition ts
  if nmr >= iStart
    then atomically $ writeTVar nextMessageRow iStart
    else clearLines nmr iStart
  setCursorPosition $ Position {row = max nmr iStart, col = 0}
  putString $ prompt <> inputString ts <> " "
  eraseInLine EraseForward
  setCursorPosition $ Position {row = iStart + row, col}
  showCursor
  flush
  where
    clearLines :: Int -> Int -> m ()
    clearLines from till
      | from >= till = return ()
      | otherwise = do
        setCursorPosition $ Position {row = from, col = 0}
        eraseInLine EraseForward
        clearLines (from + 1) till
    inputHeight :: TerminalState -> Int
    inputHeight ts = length (inputPrompt ts <> inputString ts) `div` width + 1
    positionRowColumn :: Int -> Int -> Position
    positionRowColumn wid pos =
      let row = pos `div` wid
          col = pos - row * wid
       in Position {row, col}

printMessage :: forall m. MonadTerminal m => ChatTerminal -> [StyledString] -> m ()
printMessage ChatTerminal {termSize = Size {height, width}, nextMessageRow} msg = do
  nmr <- readTVarIO nextMessageRow
  setCursorPosition $ Position {row = nmr, col = 0}
  mapM_ printStyled msg
  flush
  let lc = sum $ map lineCount msg
  atomically . writeTVar nextMessageRow $ min (height - 1) (nmr + lc)
  where
    lineCount :: StyledString -> Int
    lineCount s = sLength s `div` width + 1
    printStyled :: StyledString -> m ()
    printStyled s = do
      putStyled s
      eraseInLine EraseForward
      putLn

-- Currently it is assumed that the message does not have internal line breaks.
-- Previous implementation "kind of" supported them,
-- but it was not determining the number of printed lines correctly
-- because of accounting for control sequences in length
putStyled :: MonadTerminal m => StyledString -> m ()
putStyled (s1 :<>: s2) = putStyled s1 >> putStyled s2
putStyled (Styled [] s) = putString s
putStyled (Styled sgr s) = setSGR sgr >> putString s >> resetAttributes

setSGR :: MonadTerminal m => [SGR] -> m ()
setSGR = mapM_ $ \case
  Reset -> resetAttributes
  SetConsoleIntensity BoldIntensity -> setAttribute bold
  SetConsoleIntensity _ -> resetAttribute bold
  SetItalicized True -> setAttribute italic
  SetItalicized _ -> resetAttribute italic
  SetUnderlining NoUnderline -> resetAttribute underlined
  SetUnderlining _ -> setAttribute underlined
  SetSwapForegroundBackground True -> setAttribute inverted
  SetSwapForegroundBackground _ -> resetAttribute inverted
  SetColor l i c -> setAttribute . layer l . intensity i $ color c
  SetBlinkSpeed _ -> pure ()
  SetVisible _ -> pure ()
  SetRGBColor _ _ -> pure ()
  SetPaletteColor _ _ -> pure ()
  SetDefaultColor _ -> pure ()
  where
    layer = \case
      Foreground -> foreground
      Background -> background
    intensity = \case
      Dull -> id
      Vivid -> bright
    color = \case
      Black -> black
      Red -> red
      Green -> green
      Yellow -> yellow
      Blue -> blue
      Magenta -> magenta
      Cyan -> cyan
      White -> white
