{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.Process
  ( ProcessId (..)
  , getProcProcessIds
  ) where

import           Control.Error (runExceptT)

import           Data.Maybe (mapMaybe)

import           System.Linux.Proc.IO
import           System.Linux.Proc.Errors

import           Text.Read (readMaybe)

newtype ProcessId
  = ProcessId { unProcessId :: Int }
  deriving (Eq, Show)

-- | Get the current list of `ProcessId`s.
getProcProcessIds :: IO (Either ProcError [ProcessId])
getProcProcessIds =
  runExceptT $
    mapMaybe (fmap ProcessId . readMaybe) <$> listProcDirectory "/proc"
