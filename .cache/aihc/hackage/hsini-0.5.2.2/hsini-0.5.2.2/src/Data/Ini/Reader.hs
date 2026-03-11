{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module    : Data.Ini.Reader
Copyright : 2011-2014 Magnus Therning
License   : BSD3

Parser for configurations.
-}
module Data.Ini.Reader (
    parse,
    IniReaderError (..),
    IniParseResult,
) where

import Control.Monad.Except (throwError)
import Text.ParserCombinators.Parsec qualified as P

import Data.Ini.Reader.Internals (IniParseResult, IniReaderError (..), buildConfig, iniParser)
import Data.Ini.Types (Config)

-- | Parser for a configuration contained in a 'String'.
parse :: String -> IniParseResult Config
parse s = case P.parse iniParser "ini" s of
    Left e -> throwError . IniParserError $ show e
    Right is -> buildConfig is
