{-# LANGUAGE OverloadedStrings #-}

module Data.Attoparsec.ByteString.Base64 where

import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
-- import Data.Char (isAlphaNum)
import Data.Word8 (isAlphaNum, _equal, _plus, _slash)
import Data.Monoid ((<>))


base64 :: Atto.Parser ByteString
base64 = do
  init <- Atto.takeWhile1 (\c -> isAlphaNum c || c == _plus || c == _slash) Atto.<?> "content"
  padding <- Atto.takeWhile (== _equal) Atto.<?> "padding"
  pure (init <> padding)
