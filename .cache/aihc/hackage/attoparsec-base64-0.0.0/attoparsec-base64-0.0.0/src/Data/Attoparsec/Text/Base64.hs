{-# LANGUAGE OverloadedStrings #-}

module Data.Attoparsec.Text.Base64 where

import qualified Data.Attoparsec.Text as Atto
import Data.Text (Text)
import Data.Char (isAlphaNum)
import Data.Monoid ((<>))


base64 :: Atto.Parser Text
base64 = do
  init <- Atto.takeWhile1 (\c -> isAlphaNum c || c == '+' || c == '/') Atto.<?> "content"
  padding <- Atto.takeWhile (== '=') Atto.<?> "padding"
  pure (init <> padding)
