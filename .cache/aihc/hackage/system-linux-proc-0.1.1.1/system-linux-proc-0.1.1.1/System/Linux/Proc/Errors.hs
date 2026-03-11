{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.Errors
  ( ProcError (..)
  , renderProcError
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text


data ProcError
  = ProcReadError !FilePath !Text
  | ProcParseError !FilePath !Text
  | ProcMemInfoKeyError !Text
  deriving (Eq, Show)

renderProcError :: ProcError -> Text
renderProcError = \case
  ProcReadError fp msg -> mconcat
    [ "Error reading '", Text.pack fp, "': ", msg ]
  ProcParseError fp msg -> mconcat
    [ "Parser error on file '", Text.pack fp, ": ", msg ]
  ProcMemInfoKeyError key -> mconcat
    [ "MemInfo: Key not found: '", key, "'" ]


