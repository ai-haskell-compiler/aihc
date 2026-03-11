{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Options
-- Description :  Options for elynx validation and redo sub commands
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Apr 23 19:17:17 2020.
module Options
  ( AllReproductions (..),
    getELynxProgName,
    getELynxArgs,
    ValidateArguments (..),
    RedoArguments (..),
    CommandArguments (..),
    commandArguments,
  )
where

import ELynx.Tools.InputOutput
import ELynx.Tools.Options
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import qualified SLynx.Options as S
import qualified TLynx.Options as T

-- | Collect all reproductions in one data type.
data AllReproductions
  = S (Reproduction (Arguments S.CommandArguments))
  | T (Reproduction (Arguments T.CommandArguments))

-- | Get program name irrespective of which program has been run.
getELynxProgName :: AllReproductions -> String
getELynxProgName (S x) = progName x
getELynxProgName (T x) = progName x

-- | Get arguments irrespective of which program has been run.
getELynxArgs :: AllReproductions -> [String]
getELynxArgs (S x) = argsStr x
getELynxArgs (T x) = argsStr x

newtype ValidateArguments = ValidateArguments
  {vElynxFile :: FilePath}
  deriving (Eq, Show, Generic)

validateArguments :: Parser ValidateArguments
validateArguments = ValidateArguments <$> inFileArg

validateDsc :: [String]
validateDsc = ["Validate an ELynx analysis"]

data RedoArguments = RedoArguments
  { rElynxFile :: FilePath,
    rExecutionMode :: ExecutionMode
  }
  deriving (Eq, Show, Generic)

redoArguments :: Parser RedoArguments
redoArguments = RedoArguments <$> inFileArg <*> executionModeOpt

redoDsc :: [String]
redoDsc = ["Redo an ELynx analysis"]

inFileArg :: Parser FilePath
inFileArg = strArgument $ metavar "ELYNX-FILE"

data CommandArguments
  = Validate ValidateArguments
  | Redo RedoArguments

validateCommand :: Mod CommandFields CommandArguments
validateCommand =
  createCommand "validate" validateDsc [] validateArguments Validate

redoCommand :: Mod CommandFields CommandArguments
redoCommand = createCommand "redo" redoDsc [] redoArguments Redo

commandArguments :: ParserInfo CommandArguments
commandArguments =
  elynxParserInfo desc [] $ hsubparser $ validateCommand <> redoCommand

desc :: [String]
desc = ["Validate and redo past ELynx analyses"]
