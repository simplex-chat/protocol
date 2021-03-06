{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Styled
  ( StyledString (..),
    bPlain,
    plain,
    styleMarkdown,
    styleMarkdownText,
    styled,
    sLength,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Markdown
import System.Console.ANSI.Types

data StyledString = Styled [SGR] String | StyledString :<>: StyledString

instance Semigroup StyledString where (<>) = (:<>:)

instance Monoid StyledString where mempty = plain ""

instance IsString StyledString where fromString = plain

plain :: String -> StyledString
plain = Styled []

bPlain :: ByteString -> StyledString
bPlain = Styled [] . B.unpack

styleMarkdownText :: Text -> StyledString
styleMarkdownText = styleMarkdown . parseMarkdown

styleMarkdown :: Markdown -> StyledString
styleMarkdown (s1 :|: s2) = styleMarkdown s1 <> styleMarkdown s2
styleMarkdown (Markdown Snippet s) = '`' `wrap` styled Snippet s
styleMarkdown (Markdown Secret s) = '#' `wrap` styled Secret s
styleMarkdown (Markdown f s) = styled f s

wrap :: Char -> StyledString -> StyledString
wrap c s = plain [c] <> s <> plain [c]

class StyledFormat a where
  styled :: Format -> a -> StyledString

instance StyledFormat String where styled = Styled . sgr

instance StyledFormat ByteString where styled f = styled f . B.unpack

instance StyledFormat Text where styled f = styled f . T.unpack

sgr :: Format -> [SGR]
sgr = \case
  Bold -> [SetConsoleIntensity BoldIntensity]
  Italic -> [SetUnderlining SingleUnderline, SetItalicized True]
  Underline -> [SetUnderlining SingleUnderline]
  StrikeThrough -> [SetSwapForegroundBackground True]
  Colored c -> [SetColor Foreground Vivid c]
  Secret -> [SetColor Foreground Dull Black, SetColor Background Dull Black]
  Snippet -> []
  NoFormat -> []

sLength :: StyledString -> Int
sLength (Styled _ s) = length s
sLength (s1 :<>: s2) = sLength s1 + sLength s2
