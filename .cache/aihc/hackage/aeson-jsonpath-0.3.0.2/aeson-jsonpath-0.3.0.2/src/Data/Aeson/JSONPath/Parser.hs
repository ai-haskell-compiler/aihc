{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{- |
Module      : Data.Aeson.JSONPath.Parser
Description : JSONPath Query Parser
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable

This module is responsible for parsing the JSONPath query
-}
module Data.Aeson.JSONPath.Parser
  ( pQuery )
  where

import qualified Text.ParserCombinators.Parsec  as P

import Data.Aeson.JSONPath.Parser.Query (pRootQuery)
import Data.Aeson.JSONPath.Types

import Prelude

-- | Query parser
pQuery :: P.Parser Query
pQuery = pRootQuery <* P.eof
