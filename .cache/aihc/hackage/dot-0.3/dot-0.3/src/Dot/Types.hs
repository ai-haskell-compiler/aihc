{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dot.Types where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.String (IsString(fromString))

data Strictness = Strict | NonStrict
  deriving (Show,Read)

data Directionality = Directed | Undirected
  deriving (Show,Read)

data CardinalDirection
  = North
  | East
  | South
  | West
  | Northeast
  | Northwest
  | Southeast
  | Southwest
  deriving (Show,Read)

data Element = Graph | Node | Edge
  deriving (Show,Read)

data EdgeElement = EdgeSubgraph Subgraph | EdgeNode NodeId
  deriving (Show,Read)

instance IsString EdgeElement where
  fromString str = EdgeNode (fromString str)

newtype Id = Id Text
  deriving (Show,Read,IsString)

data NodeId = NodeId Id (Maybe Port)
  deriving (Show,Read)

instance IsString NodeId where
  fromString str = NodeId (fromString str) Nothing

data ListTwo a = ListTwo
  { listTwoFirst :: a
  , listTwoSecond :: a
  , listTwoOther :: [a]
  } deriving (Show,Read)

data Port = Port
  { portId :: Id
  , portCompass :: Maybe CardinalDirection
  } deriving (Show,Read)

data DotGraph = DotGraph Strictness Directionality (Maybe Id) [Statement]
  deriving (Show,Read)

data Statement
  = StatementAttribute AttributeStatement
  | StatementNode NodeStatement
  | StatementEdge EdgeStatement
  | StatementSubgraph Subgraph
  | StatementEquality Id Id
  deriving (Show,Read)

data AttributeStatement = AttributeStatement Element [Attribute]
  deriving (Show,Read)

data Attribute = Attribute Id Id
  deriving (Show,Read)

data NodeStatement = NodeStatement NodeId [Attribute]
  deriving (Show,Read)

data EdgeStatement = EdgeStatement (ListTwo EdgeElement) [Attribute]
  deriving (Show,Read)

data Subgraph = Subgraph
  { subgraphId :: Maybe Id
  , subgraphStatements :: [Statement]
  } deriving (Show,Read)

