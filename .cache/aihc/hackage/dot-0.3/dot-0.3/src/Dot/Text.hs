{-# LANGUAGE OverloadedStrings #-}

module Dot.Text
  ( encode
  , encodeLazy
  , builder
  , encodeToFile
  ) where

import Data.Monoid
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Dot.Types
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder

levelSpaces :: Int
levelSpaces = 2

indentationBuilder :: Builder
indentationBuilder = "  "

encode :: DotGraph -> Text
encode = LText.toStrict . encodeLazy

encodeLazy :: DotGraph -> LText.Text
encodeLazy = Builder.toLazyText . builder

encodeToFile :: FilePath -> DotGraph -> IO ()
encodeToFile fp dg = TIO.writeFile fp (encode dg)

builder :: DotGraph -> Builder
builder (DotGraph strictness directionality mid statements) = mempty
  <> encodeStrictness strictness
  <> encodeGraphDirectionality directionality
  <> encodeMaybeId mid
  <> "{\n"
  <> foldr (\statement builder -> encodeStatement directionality indentationBuilder statement <> builder) mempty statements
  <> "}"

encodeId :: Id -> Builder
encodeId (Id theId) = case Text.uncons theId of
  Just (c,_) -> if not (c >= '0' && c <= '9') && Text.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_') theId
    then Builder.fromText theId
    else "\""
      <> Builder.fromText
         ( Text.replace "\"" "\\\""
         $ Text.replace "\n" "\\n"
         $ Text.replace "\\" "\\\\"
         $ theId
         )
      <> "\""
  Nothing -> "\"\""

encodeNodeId :: NodeId -> Builder
encodeNodeId (NodeId theId mport) =
  encodeId theId <> maybe mempty encodePort mport

encodePort :: Port -> Builder
encodePort (Port theId mcompass) =
  ":" <> encodeId theId <> maybe mempty encodeCompass mcompass

encodeCompass :: CardinalDirection -> Builder
encodeCompass x = case x of
  North -> "n"
  East -> "e"
  South -> "s"
  West -> "w"
  Northeast -> "ne"
  Northwest -> "nw"
  Southeast -> "se"
  Southwest -> "sw"

encodeMaybeId :: Maybe Id -> Builder
encodeMaybeId x = case x of
  Just theId -> encodeId theId <> " "
  Nothing -> mempty

encodeStrictness :: Strictness -> Builder
encodeStrictness x = case x of
  Strict -> "strict "
  NonStrict -> mempty

encodeElement :: Element -> Builder
encodeElement x = case x of
  Graph -> "graph "
  Node -> "node "
  Edge -> "edge "

encodeSubgraph :: Subgraph -> Builder
encodeSubgraph = error "encodeSubgraph: have not written this function yet"

encodeStatement :: Directionality -> Builder -> Statement -> Builder
encodeStatement directionality indentation x = case x of
  StatementAttribute (AttributeStatement element attrs) -> indentation
    <> encodeElement element
    <> encodeAttributes attrs
  StatementNode (NodeStatement theNodeId attrs) -> indentation
    <> encodeNodeId theNodeId
    <> encodeAttributes attrs
  StatementSubgraph subgraph -> encodeSubgraph subgraph
  StatementEdge (EdgeStatement elements attrs) -> indentation
    <> encodeEdgeElements directionality elements
    <> encodeAttributes attrs
  StatementEquality a b -> indentation
    <> encodeId a <> " = " <> encodeId b <> "\n"
  where nextIndentation = indentationBuilder <> indentation

encodeEdgeOp :: Directionality -> Builder
encodeEdgeOp x = case x of
  Undirected -> " -- "
  Directed -> " -> "

encodeEdgeElements :: Directionality -> ListTwo EdgeElement -> Builder
encodeEdgeElements edgeOp (ListTwo a b xs) =
  encodeEdgeElement a <> edgeOpBuilder <> encodeEdgeElement b
  <> foldr (\e builder -> edgeOpBuilder <> encodeEdgeElement e <> builder) mempty xs
  where edgeOpBuilder = encodeEdgeOp edgeOp

encodeEdgeElement :: EdgeElement -> Builder
encodeEdgeElement x = case x of
  EdgeSubgraph subgraph -> encodeSubgraph subgraph
  EdgeNode theNodeId -> encodeNodeId theNodeId

encodeAttributes :: [Attribute] -> Builder
encodeAttributes (x : xs) = " ["
  <> foldr (\attr builder -> encodeAttribute attr <> "," <> builder) (encodeAttribute x) xs
  <> "];\n"
encodeAttributes [] = " [];\n"

encodeAttribute :: Attribute -> Builder
encodeAttribute (Attribute attrId valId) = encodeId attrId <> "=" <> encodeId valId

encodeGraphDirectionality :: Directionality -> Builder
encodeGraphDirectionality x = case x of
  Directed -> "digraph "
  Undirected -> "graph "

example :: DotGraph
example = DotGraph Strict Directed (Just "foobar")
  [ StatementNode $ NodeStatement "a1"
    [ Attribute "color" "blue"
    , Attribute "shape" "box"
    ]
  , StatementNode $ NodeStatement "a2" []
  , StatementEdge $ EdgeStatement (ListTwo "a1" "a2" ["a3"])
    [ Attribute "color" "red"
    ]
  ]
