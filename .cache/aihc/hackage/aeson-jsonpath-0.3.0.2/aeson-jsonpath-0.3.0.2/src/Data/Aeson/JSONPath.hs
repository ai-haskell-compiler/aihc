{- |
Module      : Data.Aeson.JSONPath
Description : Run JSONPath queries on Data.Aeson
Copyright   : (c) 2024-2025 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <mtaimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable

Run JSONPath queries on Aeson Values using methods exported in this module.
-}
module Data.Aeson.JSONPath
  (
  -- * Using this library
  -- $use

  -- * API
    query
  , queryQQ
  , queryLocated
  , queryLocatedQQ

  -- * QuasiQuoter
  , jsonPath
  )
  where

import qualified Text.ParserCombinators.Parsec as P

import Data.Aeson                  (Value)
import Data.Vector                 (Vector)
import Language.Haskell.TH.Quote   (QuasiQuoter (..))
import Language.Haskell.TH.Syntax  (lift)

import Data.Aeson.JSONPath.Parser  (pQuery)
import Data.Aeson.JSONPath.Query   (qQuery, qQueryLocated)

import Data.Aeson.JSONPath.Types

import Prelude

-- |
-- A 'QuasiQuoter' for checking valid JSONPath syntax at compile time
--
-- @
-- path :: Query
-- path = [jsonPath|$.store.records[0,1]|]
-- @
jsonPath :: QuasiQuoter
jsonPath = QuasiQuoter
  { quoteExp = \q -> case P.parse pQuery ("failed to parse query: " <> q) q of
      Left err -> fail $ show err
      Right ex -> lift ex
  , quotePat = error "Error: quotePat"
  , quoteType = error "Error: quoteType"
  , quoteDec = error "Error: quoteDec"
  }

-- |
-- Use when query string is not known at compile time
--
-- @
-- >>> query "$.artist" json
-- Right [String "David Bowie"]
--
-- >>> query "$.art[ist" json
-- Left "failed to parse query: $.art[ist" (line 1, column 7)
-- @
-- For detailed usage examples, see: <https://github.com/taimoorzaeem/aeson-jsonpath?tab=readme-ov-file#aeson-jsonpath>
query :: String -> Value -> Either P.ParseError (Vector Value)
query q root = do
  parsedQuery <- P.parse pQuery ("failed to parse query: " <> q) q
  return $ queryQQ parsedQuery root


-- |
-- Use when query string is known at compile time
--
-- @
-- artist = queryQQ [jsonPath|$.artist|] json -- successfully compiles
--
-- >>> artist
-- [String "David Bowie"]
-- @
-- @
-- artist = queryQQ [jsonPath|$.art[ist|] json -- fails at compilation time
-- @
queryQQ :: Query -> Value -> Vector Value
queryQQ q root = qQuery q QueryState {
    rootVal = root
  , curVal = root
  , executeQuery = qQuery
}

-- |
-- Get the location of the returned nodes along with the node
--
-- @
-- >>> queryLocated "$.title" json
-- Right [("$[\'title\']",String "Space Oddity")]
-- @
queryLocated :: String -> Value -> Either P.ParseError (Vector (String, Value))
queryLocated q root = do
  parsedQuery <- P.parse pQuery ("failed to parse query: " <> q) q
  return $ queryLocatedQQ parsedQuery root

-- |
-- Same as 'queryLocated' but allows QuasiQuoter
--
-- @
-- artist = queryLocatedQQ [jsonPath|$.*|] json -- successfully compiles
--
-- >>> artist
-- [("$[\'artist\']",String "David Bowie"),
--  ("$[\'title\']",String "Space Oddity")]
-- @
queryLocatedQQ :: Query -> Value -> Vector (String, Value)
queryLocatedQQ q root = qQueryLocated q QueryState {
    rootVal = root
  , curVal = root
  , executeQuery = qQuery
} "$"

-- $use
--
-- To use this package, I would suggest that you import this module like:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import qualified Data.Aeson.JSONPath as JSONPath
-- > import           Data.Aeson.JSONPath (jsonPath)
--
-- For this module, consider this json for the example queries
--
-- > { "artist": "David Bowie", "title": "Space Oddity" }
