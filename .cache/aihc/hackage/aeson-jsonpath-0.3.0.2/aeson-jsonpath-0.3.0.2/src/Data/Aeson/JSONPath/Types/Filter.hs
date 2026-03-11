{-# LANGUAGE DeriveLift #-}
module Data.Aeson.JSONPath.Types.Filter
  (LogicalOrExpr (..)
  , LogicalAndExpr (..)
  , BasicExpr (..)
  , TestExpr (..)
  , ComparisonExpr (..)
  , ComparisonOp (..)
  , Comparable (..)
  , Literal (..)
  , SingularQueryType (..)
  , SingularQuery (..)
  , SingularQuerySegment (..)
  )
  where

import Data.Text                   (Text)
import Data.Scientific             (Scientific)

import Language.Haskell.TH.Syntax  (Lift)

import Prelude

-- |
newtype LogicalOrExpr a
  = LogicalOr [LogicalAndExpr a]
  deriving (Eq, Show, Lift)

-- |
newtype LogicalAndExpr a
  = LogicalAnd [BasicExpr a]
  deriving (Eq, Show, Lift)

-- |
data BasicExpr a
  = Paren (LogicalOrExpr a) -- ( expr )
  | NotParen (LogicalOrExpr a) -- not (expr)
  | Test (TestExpr a) -- query
  | NotTest (TestExpr a) -- not query
  | Comparison ComparisonExpr
  deriving (Eq, Show, Lift)

-- |
newtype TestExpr a
  = FilterQuery a
  deriving (Eq, Show, Lift)

-- |
data ComparisonExpr
  = Comp Comparable ComparisonOp Comparable
  deriving (Eq, Show, Lift)

-- |
data ComparisonOp
  = Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | Equal
  | NotEqual
  deriving (Eq, Show, Lift)

-- |
data Comparable
  = CompLit Literal
  | CompSQ SingularQuery
  deriving (Eq, Show, Lift)

-- |
data Literal
  = LitString Text
  | LitNum Scientific
  | LitBool Bool
  | LitNull
  deriving (Eq, Show, Lift)

-- |
data SingularQueryType = RootSQ | CurrentSQ
  deriving (Eq, Show, Lift)

-- |
data SingularQuery = SingularQuery
  { singularQueryType     :: SingularQueryType
  , singularQuerySegments :: [SingularQuerySegment]
  } deriving (Eq, Show, Lift)

-- |
data SingularQuerySegment
  = NameSQSeg Text
  | IndexSQSeg Int
  deriving (Eq, Show, Lift)
