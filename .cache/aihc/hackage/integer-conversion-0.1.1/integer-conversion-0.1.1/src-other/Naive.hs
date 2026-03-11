module Naive (
    textToInteger,
    byteStringToInteger,
    stringToInteger,
) where

import Data.Char (ord)

import qualified Data.ByteString as BS
import qualified Data.List       as L
import qualified Data.Text       as T

textToInteger :: T.Text -> Integer
textToInteger = T.foldl' (\acc c -> acc * 10 + toInteger (ord c - 48)) 0

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger = BS.foldl' (\acc c -> acc * 10 + toInteger c - 48) 0

stringToInteger :: String -> Integer
stringToInteger = L.foldl' (\acc c -> acc * 10 + toInteger (ord c - 48)) 0
