{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Attoparsec.Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO
import qualified System.Environment

import qualified Nix.NarInfo

main :: IO ()
main = System.Environment.getArgs >>= \case
  [filename] -> do
    txt <- Data.Text.IO.readFile filename
    case
      Data.Attoparsec.Text.parseOnly
        Nix.NarInfo.parseNarInfo
        txt
      of
        Left e -> error e
        Right ni ->
          Data.Text.Lazy.IO.putStr
          $ Data.Text.Lazy.Builder.toLazyText
          $ Nix.NarInfo.buildNarInfo ni
  _ -> error "No input narinfo file"
