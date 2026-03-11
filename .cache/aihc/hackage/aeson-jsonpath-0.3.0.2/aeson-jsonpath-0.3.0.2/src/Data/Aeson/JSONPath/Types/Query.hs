{-# LANGUAGE DeriveLift #-}
module Data.Aeson.JSONPath.Types.Query
  ( Query (..)
  , QueryType (..)
  , QueryState (..)
  )
  where

import Data.Aeson  (Value)
import Data.Vector (Vector)

import Data.Aeson.JSONPath.Types.Segment (QuerySegment (..))
import Language.Haskell.TH.Syntax        (Lift)

import Prelude

-- |
data QueryState = QueryState
  { rootVal      :: Value
  , curVal       :: Value
  , executeQuery :: Query -> QueryState -> Vector Value
  }

-- |
data QueryType 
  = Root 
  | Current
  deriving (Eq, Show, Lift)

-- |
data Query = Query
  { queryType     :: QueryType
  , querySegments :: [QuerySegment Query]
  } deriving (Eq, Show, Lift)
