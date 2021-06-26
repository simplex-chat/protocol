module Simplex.Util where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = decodeUtf8With onError
  where
    onError _ _ = Just '?'
