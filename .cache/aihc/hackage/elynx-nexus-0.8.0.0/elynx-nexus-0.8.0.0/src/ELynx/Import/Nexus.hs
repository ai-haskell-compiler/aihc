{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Import.Nexus
-- Description :  Nexus types and classes
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 17:10:05 2020.
module ELynx.Import.Nexus
  ( Block (..),
    nexusBlock,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS

-- | A Nexus block has a name (e.g., TREES), and parser for the entry.
data Block a = Block
  { name :: BS.ByteString,
    parser :: Parser a
  }

-- This has to be refined. Like this, only one block can be parsed, and the
-- block type has to be known beforehand.

-- | Parse a given 'Block' in a Nexus file.
--
-- The Nexus file can contain other blocks.
nexusBlock :: Block a -> Parser a
nexusBlock b = do
  start
  _ <- manyTill anyChar (lookAhead $ beginB b) <?> "nexusBlockSkipUntilBlock"
  r <- block b <?> "nexusBlock"
  _ <- many anyChar <?> "nexusBlockSkipUntilEnd"
  _ <- endOfInput
  return r

start :: Parser ()
start = do
  _ <- stringCI "#nexus" <?> "nexusStart"
  skipWhile isSpace
  return ()

block :: Block a -> Parser a
block b = do
  beginB b
  r <- parser b <?> "blockParser"
  endB
  return r

beginB :: Block a -> Parser ()
beginB (Block n _) = do
  _ <- stringCI "begin" <?> "blockBegin"
  skipWhile isSpace
  _ <- stringCI n <?> "blockName"
  _ <- char ';' <?> "blockEnd"
  skipWhile isSpace
  return ()

endB :: Parser ()
endB = do
  _ <- stringCI "end;" <?> "nexusEnd"
  skipWhile isSpace
  return ()
