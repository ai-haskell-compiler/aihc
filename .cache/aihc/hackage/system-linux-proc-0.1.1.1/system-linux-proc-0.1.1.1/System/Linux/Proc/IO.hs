{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.IO
  ( listProcDirectory
  , readProcFile
  ) where

import           Control.Error (ExceptT, handleExceptT)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Data.Text (Text)
import qualified Data.Text as T

import           System.IO (IOMode (..), withFile)

import           System.Directory (getDirectoryContents)
import           System.Linux.Proc.Errors


readProcFile :: FilePath -> ExceptT ProcError IO ByteString
readProcFile fpath =
  handleExceptT (ProcReadError fpath . ioErrorToText) $
    -- BS.readFile won't work here because it tries to get the file
    -- length before reading the file and files in the /proc filesystem
    -- are reported as having zero length.
    withFile fpath ReadMode BS.hGetContents

listProcDirectory :: FilePath -> ExceptT ProcError IO [FilePath]
listProcDirectory fpath =
  handleExceptT (ProcReadError fpath . ioErrorToText) $ getDirectoryContents fpath

ioErrorToText :: IOError -> Text
ioErrorToText = T.pack . show
