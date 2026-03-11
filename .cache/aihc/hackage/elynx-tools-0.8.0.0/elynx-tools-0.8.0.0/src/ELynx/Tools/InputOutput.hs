{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  ELynx.Tools.InputOutput
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 14 13:30:37 2019.
--
-- Tools involving input, output, and parsing.
module ELynx.Tools.InputOutput
  ( -- * Execution Mode
    ExecutionMode (..),
    openFileWithExecutionMode,

    -- * Input, output
    readGZFile,
    writeGZFile,

    -- * Parsing
    runParserOnFile,
    parseFileWith,
    parseIOWith,
    parseFileOrIOWith,
    parseStringWith,
    parseByteStringWith,
  )
where

import Codec.Compression.GZip
import Data.Aeson hiding (String)
import Data.Attoparsec.ByteString.Lazy hiding (Fail)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isSuffixOf)
import GHC.Generics
import System.Directory
import System.IO

-- | Overwrite existing output files or fail if output files exist.
data ExecutionMode = Overwrite | Fail
  deriving (Eq, Show, Generic)

instance FromJSON ExecutionMode

instance ToJSON ExecutionMode

checkFile :: ExecutionMode -> FilePath -> IO ()
checkFile Overwrite _ = return ()
checkFile Fail fp =
  doesFileExist fp >>= \case
    True ->
      error $
        "File exists: "
          <> fp
          <> "."
          <> "\n"
          <> "Please use --force to overwrite results of a previous analysis."
    False -> return ()

-- | Open existing files only if 'ExecutionMode' is 'Overwrite'.
openFileWithExecutionMode :: ExecutionMode -> FilePath -> IO Handle
openFileWithExecutionMode em fp = checkFile em fp >> openFile fp WriteMode

-- | Read file. If file path ends with ".gz", assume gzipped file and decompress
-- before read.
readGZFile :: FilePath -> IO BL.ByteString
readGZFile f
  | ".gz" `isSuffixOf` f = decompress <$> BL.readFile f
  | otherwise = BL.readFile f

-- | Write file. If file path ends with ".gz", assume gzipped file and compress
-- before write.
writeGZFile :: ExecutionMode -> FilePath -> BL.ByteString -> IO ()
writeGZFile frc f r
  | ".gz" `isSuffixOf` f = checkFile frc f >> BL.writeFile f (compress r)
  | otherwise = checkFile frc f >> BL.writeFile f r

-- | Parse a possibly gzipped file.
runParserOnFile :: Parser a -> FilePath -> IO (Either String a)
runParserOnFile p f = eitherResult . parse p <$> readGZFile f

-- | Parse a possibly gzipped file and extract the result.
parseFileWith :: Parser a -> FilePath -> IO a
parseFileWith p f = parseFileOrIOWith p (Just f)

-- | Parse standard input.
parseIOWith :: Parser a -> IO a
parseIOWith p = parseFileOrIOWith p Nothing

-- | Parse a possibly gzipped file, or standard input, and extract the result.
parseFileOrIOWith :: Parser a -> Maybe FilePath -> IO a
parseFileOrIOWith p mf = do
  s <- maybe BL.getContents readGZFile mf
  return $! parseByteStringWith p s

-- | Parse a 'String' and extract the result.
parseStringWith :: Parser a -> String -> a
parseStringWith p x = parseByteStringWith p (BL.pack x)

-- | Parse a 'BL.ByteString' and extract the result.
parseByteStringWith :: Parser a -> BL.ByteString -> a
parseByteStringWith p x = case eitherResult $ parse p x of
  Left err -> error err
  Right val -> val
