{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A stable, non-evaluating view of a GRIN result and its reachable heap.
module Aihc.Grin.Snapshot
  ( HeapSnapshot (..),
    SnapshotValue (..),
    SnapshotCell (..),
    renderSnapshotReturn,
    renderSnapshotHeap,
    renderHeapSnapshot,
  )
where

import Aihc.Grin.Syntax
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T

-- | Result values plus the heap cells reachable from them. Location numbers
-- are assigned in graph-discovery order, so they do not expose interpreter or
-- native allocator addresses.
data HeapSnapshot = HeapSnapshot
  { snapshotReturnValues :: ![SnapshotValue],
    snapshotHeap :: !(IntMap SnapshotCell)
  }
  deriving (Eq, Show)

data SnapshotValue
  = SnapshotLiteral !GrinLiteral
  | SnapshotAddress
  | SnapshotNode !GrinNodeTag ![SnapshotValue]
  | SnapshotLocation !Int
  | SnapshotMutVar
  | SnapshotStateToken
  deriving (Eq, Show)

data SnapshotCell
  = SnapshotSuspended !FunctionName ![SnapshotValue]
  | SnapshotIndirection !Int
  | SnapshotValue !SnapshotValue
  | SnapshotRaised !SnapshotValue
  | SnapshotBlackhole
  deriving (Eq, Show)

renderSnapshotReturn :: HeapSnapshot -> Text
renderSnapshotReturn snapshot =
  case snapshotReturnValues snapshot of
    [] -> "()"
    [value] -> renderValue False value
    values -> "(" <> T.intercalate ", " (map (renderValue False) values) <> ")"

renderSnapshotHeap :: HeapSnapshot -> Text
renderSnapshotHeap snapshot =
  T.intercalate
    "\n"
    [ "@" <> tshow location <> " = " <> renderCell cell
    | (location, cell) <- IntMap.toAscList (snapshotHeap snapshot)
    ]

renderHeapSnapshot :: HeapSnapshot -> Text
renderHeapSnapshot snapshot =
  "return: "
    <> renderSnapshotReturn snapshot
    <> "\nheap:"
    <> case renderSnapshotHeap snapshot of
      "" -> " []"
      heap -> "\n" <> indent heap

renderCell :: SnapshotCell -> Text
renderCell cell =
  case cell of
    SnapshotSuspended functionName fields -> renderNode False (GrinThunk functionName) fields
    SnapshotIndirection location -> "Indirection @" <> tshow location
    SnapshotValue value -> renderValue False value
    SnapshotRaised exception -> "<raised " <> renderValue False exception <> ">"
    SnapshotBlackhole -> "<blackhole>"

renderValue :: Bool -> SnapshotValue -> Text
renderValue nested value =
  case value of
    SnapshotLiteral literal -> renderLiteral literal
    SnapshotAddress -> "<addr>"
    SnapshotNode tag fields -> renderNode nested tag fields
    SnapshotLocation location -> "@" <> tshow location
    SnapshotMutVar -> "<mutvar>"
    SnapshotStateToken -> "<state>"

renderNode :: Bool -> GrinNodeTag -> [SnapshotValue] -> Text
renderNode nested tag fields =
  parenthesize (nested && not (null fields)) $
    T.unwords (renderNodeTag tag : map (renderValue True) fields)

renderNodeTag :: GrinNodeTag -> Text
renderNodeTag tag =
  case tag of
    GrinConstructor name remaining ->
      "C" <> name <> if remaining == 0 then "" else "/" <> tshow remaining
    GrinClosure functionName argumentLayouts ->
      "P" <> unFunctionName functionName <> "/" <> tshow (length argumentLayouts)
    GrinThunk functionName -> "F" <> unFunctionName functionName

renderLiteral :: GrinLiteral -> Text
renderLiteral literal =
  case literal of
    GrinLitInt _ value -> tshow value
    GrinLitChar _ value -> T.pack (show value) <> "#"
    GrinLitString value -> T.pack (show (T.unpack value))
    GrinLitAddr value -> T.pack (show (map (chr . fromIntegral) (BS.unpack value))) <> "#"

parenthesize :: Bool -> Text -> Text
parenthesize shouldParenthesize value
  | shouldParenthesize = "(" <> value <> ")"
  | otherwise = value

indent :: Text -> Text
indent = T.unlines . map ("  " <>) . T.lines

tshow :: (Show value) => value -> Text
tshow = T.pack . show
