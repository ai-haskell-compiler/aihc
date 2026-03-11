{-# LANGUAGE DeriveLift #-}
module Data.Aeson.JSONPath.Types.Segment
  (Segment (..)
  , QuerySegment (..)
  , SegmentType (..)
  )
  where

import Data.Aeson.JSONPath.Types.Selector (Selector (..))
import Data.Text                          (Text)
import Language.Haskell.TH.Syntax         (Lift)

import Prelude

-- |
data SegmentType
  = Child
  | Descendant
  deriving (Eq, Show, Lift)

-- |
data QuerySegment a = QuerySegment
  { segmentType :: SegmentType
  , segment     :: Segment a
  } deriving (Eq, Show, Lift)

-- |
data Segment a
  = Bracketed [Selector a]
  | Dotted Text
  | WildcardSegment
  deriving (Eq, Show, Lift)
