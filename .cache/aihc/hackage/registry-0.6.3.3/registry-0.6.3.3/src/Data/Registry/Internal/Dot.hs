-- |
--  Nested datatype to track the resolution algorithm
--
--  From this data type we can draw a graph of the full
--  instantiation of a value
module Data.Registry.Internal.Dot where

import Data.Hashable
import Data.List (elemIndex)
import Data.Map.Strict hiding (adjust)
import Data.Registry.Internal.Statistics
import Data.Registry.Internal.Types
import Data.Text as T
import Protolude as P
import Type.Reflection

-- | Make a list of graph edges from the list of function applications
makeEdges :: Operations -> [(Value, Value)]
makeEdges [] = []
makeEdges (AppliedFunction out ins : rest) = ((out,) <$> ins) <> makeEdges rest

-- * DOT GRAPH

-- | A DOT graph
newtype Dot = Dot
  { unDot :: Text
  }
  deriving (Eq, Show)

-- | Use a State type to get the current index of a value
-- when there are values of the same type and different
-- hash values
type DotState = State ValuesByType

-- | List of value hashes by value type
type ValuesByType = Map SomeTypeRep ValueHashes

-- | Type alias for a Hash
type Hash = Int

-- | Type alias for a ValueId
type ValueId = Int

-- | Type alias for a list of hashes
type ValueHashes = [Hash]

-- | Type alias for a list of an edge in the graph
type Edge = (Value, Value)

-- | Type alias for a list of edges
type Edges = [Edge]

-- | Type alias for associating a number to a value
type ValueCounter = Maybe Int

-- | Make a DOT graph out of all the function applications
toDot :: Operations -> Dot
toDot op =
  let edges = makeEdges op
      allValues = edges >>= (\(v1, v2) -> [v1, v2])
      valueTypes = execState (traverse countValueTypes allValues) mempty
   in Dot $
        T.unlines $
          [ "strict digraph {",
            "  node [shape=record]"
          ]
            <> (toDotEdge valueTypes <$> edges)
            <> ["}"]

-- | Update a map classifying values by type
countValueTypes :: Value -> DotState ()
countValueTypes value = do
  maps <- get
  let key = valueDynTypeRep value
  let valueHash = hashOf value

  case lookup key maps of
    -- there were no values for that type, create a list with the value hash
    Nothing -> put $ insert key [valueHash] maps
    -- there is a list of hashes for that type
    Just hashes ->
      case elemIndex valueHash hashes of
        -- that value hasn't been seen before
        Nothing -> do
          let newHashes = hashes <> [valueHash]
          put $ insert key newHashes maps

        -- the value has been seen before
        Just _ -> pure ()

-- | A DOT edge representing the dependency between 2 values
toDotEdge :: ValuesByType -> (Value, Value) -> Text
toDotEdge valuesByType (value1, value2) =
  let v1 = toDotVertex valuesByType value1
      v2 = toDotVertex valuesByType value2
   in v1 <> " -> " <> v2 <> ";"

-- | Represent a value as a vertex in a dot graph
--   we use some state to keep track of values of the
--   same type
--   The values are numbered starting from 1 when there are
--   several of them for the same type
toDotVertex :: ValuesByType -> Value -> Text
toDotVertex valuesByType value =
  let key = valueDynTypeRep value
      valueHash = hashOf value

      valueCounter =
        case lookup key valuesByType of
          Nothing -> Nothing -- this case should not happen given how the map is built
          Just hashes ->
            case hashes of
              [_] -> Nothing
              _ -> (+ 1) <$> elemIndex valueHash hashes
   in adjust (nodeDescription (valDescription value) valueCounter)

-- | Return the hash of a value based on its dependencies
hashOf :: Value -> Int
hashOf value =
  hash
    (unDependencies . valueDependencies $ value, valDescription value)

-- | Description of a Value in the DOT graph
nodeDescription :: ValueDescription -> ValueCounter -> Text
nodeDescription (ValueDescription t Nothing) n =
  t <> showValueCounter n
nodeDescription (ValueDescription t (Just v)) n =
  nodeDescription (ValueDescription t Nothing) n <> "\n" <> v

-- | Don't show the counter if there
showValueCounter :: ValueCounter -> Text
showValueCounter Nothing = ""
showValueCounter (Just n) = "-" <> P.show n

-- | We need to process the node descriptions
--     - we add quotes arountd the text
--     - we remove quotes (") inside the text
--     - we escape newlines
adjust :: Text -> Text
adjust node = "\"" <> (escapeNewlines . removeQuotes) node <> "\""

-- | Remove quotes from a textual description to avoid breaking the DOT format
removeQuotes :: Text -> Text
removeQuotes = T.replace "\"" ""

-- | Replace \n with \\n so that newlines are kept in
--   node descriptions
escapeNewlines :: Text -> Text
escapeNewlines = T.replace "\n" "\\n"
