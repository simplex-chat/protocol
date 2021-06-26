{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Demo where

import Simplex.Chat.Styled
import System.Console.ANSI.Types
import System.Terminal

someViewUpdate :: Monad m => m ()
someViewUpdate = pure ()

chatLayoutDemo :: MonadTerminal m => m ()
chatLayoutDemo =
  mapM_
    putStyledLn
    [ "  search " <> Styled gray "(ctrl-s)   " <> lineV <> Styled toContact " @bob " <> "Bob Roberts                                    " <> Styled greenColor "@john" <> "",
      "                    " <> lineV <> Styled gray " 14:15 online                             profile (ctrl-p)",
      lineH 20 <> crossover <> lineH 59,
      "* " <> Styled [SetConsoleIntensity BoldIntensity] "all chats   " <> "      " <> lineV <> "",
      Styled gray "  (ctrl-a)          " <> lineV <> "",
      "*" <> Styled toContact " @alice      " <> Styled darkGray "14:37 " <> lineV <> "",
      Styled gray "  Hello there!  ... " <> lineV <> "",
      Styled selected " " <> Styled (toContact <> selected) " @bob        " <> Styled (selected <> gray) "12:35 " <> lineV <> "",
      Styled selected "  All good, John... " <> lineV <> "",
      "*" <> Styled group " #team       " <> Styled darkGray "10:55 " <> lineV <> "",
      Styled gray "  What's up ther... " <> lineV <> "",
      " " <> Styled toContact " @tom          " <> Styled darkGray "Wed " <> lineV <> "",
      Styled gray "  Have you seen ... " <> lineV <> "",
      "                    " <> lineV,
      "                    " <> lineV,
      "                    " <> lineV,
      "                    " <> lineV,
      "                    " <> lineV,
      "                    " <> lineV <> Styled greenColor " ✔︎" <> Styled darkGray " 12:30" <> Styled toContact " @bob" <> " hey bob - how is it going?",
      "                    " <> lineV <> Styled greenColor " ✔︎" <> Styled darkGray "      " <> Styled toContact "     " <> " let's meet soon!",
      "                    " <> lineV <> " *" <> Styled darkGray " 12:35" <> Styled contact " bob>" <> " All good, John! How are you?",
      "                    " <> teeL <> lineH 59,
      "                    " <> lineV <> " > " <> Styled toContact "@bob" <> " 😀 This is the message that will be sent to @bob"
    ]
    >> putStyled (Styled ctrlKeys " help (ctrl-h)  new contact (ctrl-n)  choose chat (ctrl-↓↑)  new group (ctrl-g) ")

contact :: [SGR]
contact = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]

toContact :: [SGR]
toContact = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]

group :: [SGR]
group = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]

selected :: [SGR]
selected = [SetColor Background Vivid Black]

ctrlKeys :: [SGR]
ctrlKeys = [SetColor Background Dull White, SetColor Foreground Dull Black]

gray :: [SGR]
gray = [SetColor Foreground Dull White]

darkGray :: [SGR]
darkGray = [SetColor Foreground Vivid Black]

greenColor :: [SGR]
greenColor = [SetColor Foreground Vivid Green]

lineV :: StyledString
lineV = Styled selected " " -- "\x2502"

lineH :: Int -> StyledString
lineH n = Styled darkGray $ replicate n '\x2500'

teeL :: StyledString
teeL = Styled selected " " -- "\x251C"

crossover :: StyledString
crossover = Styled selected " " -- "\x253C"

putStyledLn :: MonadTerminal m => StyledString -> m ()
putStyledLn s = putStyled s >> putLn

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
