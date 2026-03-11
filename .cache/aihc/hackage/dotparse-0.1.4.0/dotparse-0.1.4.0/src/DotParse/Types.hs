{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Abstract Grammar for the dot language.
-- http://www.graphviz.org/doc/info/lang.html
module DotParse.Types
  ( DotConfig (..),
    defaultDotConfig,
    DotParse (..),
    testDotParser,
    runDotParser,
    Error (..),
    prettyError,
    Graph (..),
    gattL,
    attL,
    defaultGraph,
    processDotWith,
    processDot,
    processGraph,
    processGraphWith,

    -- * components
    Strict (..),
    defStrict,
    Directed (..),
    defDirected,
    ID (..),
    label,
    Compass (..),
    Port (..),
    AttributeType (..),
    AttributeStatement (..),
    NodeStatement (..),
    EdgeID (..),
    EdgeOp (..),
    fromDirected,
    EdgeStatement (..),
    edgeID,
    edgeIDs,
    edgeIDsNamed,
    Statement (..),
    addStatement,
    addStatements,
    SubGraphStatement (..),
    GlobalAttributeStatement (..),

    -- * Graph Extraction
    bbL,
    nodesPortL,
    nodesL,
    edgesL,
    nodesA,
    edgesA,
    nodePos,
    nodeWidth,
    edgeSpline,
    edgeWidth,
    NodeInfo (..),
    nodeInfo,
    EdgeInfo (..),
    edgeInfo,
    splinePath,

    -- * Conversion
    graphToChartWith,
    graphToChart,
    ChartConfig (..),
    defaultChartConfig,
    toStatements,
    toDotGraph,
    toDotGraphWith,
  )
where

import Algebra.Graph qualified as G
import Chart
import Control.Monad
import Data.Bool
import Data.ByteString hiding (any, empty, filter, head, length, map, zip, zipWith)
import Data.ByteString.Char8 qualified as B
import Data.List.NonEmpty hiding (filter, head, length, map, zip, zipWith, (!!))
import Data.Map.Merge.Strict
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These
import DotParse.FlatParse
import FlatParse.Basic hiding (cut)
import GHC.Generics
import Optics.Core
import System.Exit
import System.Process.ByteString
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> import qualified Data.Map as Map
-- >>> import qualified FlatParse.Basic as FP
-- >>> import qualified Data.ByteString as BS
-- >>> import FlatParse.Basic (runParser, Result)
-- >>> :set -XOverloadedStrings

-- | printing options, for separators.
data DotConfig = DotConfig
  { topLevelSep :: ByteString,
    statementSep :: ByteString,
    attSep :: ByteString,
    subGraphSep :: ByteString
  }
  deriving (Eq, Show, Generic)

-- | default separators
defaultDotConfig :: DotConfig
defaultDotConfig = DotConfig " " "\n    " ";" ";"

-- | A parser & printer class for a graphviz graph and components of its dot language
class DotParse a where
  dotPrint :: DotConfig -> a -> ByteString
  dotParse :: Parser Error a

-- | dotParse and then dotPrint:
--
-- - pretty printing error on failure.
--
-- - This is not an exact parser/printer, so the test re-parses the dotPrint, which should be idempotent
testDotParser :: forall a. (DotParse a) => Proxy a -> DotConfig -> ByteString -> IO ()
testDotParser _ cfg b =
  case runParser dotParse b :: Result Error a of
    Err e -> B.putStrLn $ prettyError b e
    OK a left -> do
      when (left /= "") (B.putStrLn $ "parsed with leftovers: " <> left)
      case runParser dotParse (dotPrint cfg a) :: Result Error a of
        Err e -> B.putStrLn $ "round trip error: " <> prettyError (dotPrint cfg a) e
        Fail -> B.putStrLn "uncaught round trip parse error"
        OK _ left' -> do
          when (left' /= "") (B.putStrLn $ "round trip parse with left overs" <> left)
    Fail -> B.putStrLn "uncaught parse error"

-- | run a dotParse erroring on leftovers, Fail or Err
runDotParser :: (DotParse a) => ByteString -> a
runDotParser = runParser_ dotParse

-- | Representation of a full graphviz graph, as per the dot language specification
data Graph = Graph
  { strict :: Last Strict,
    directed :: Last Directed,
    graphid :: Last ID,
    nodeAttributes :: Map.Map ID ID,
    graphAttributes :: Map.Map ID ID,
    edgeAttributes :: Map.Map ID ID,
    globalAttributes :: Map.Map ID ID,
    nodes :: [NodeStatement],
    edges :: [EdgeStatement],
    subgraphs :: [SubGraphStatement]
  }
  deriving (Eq, Show, Generic)

instance Semigroup Graph where
  (Graph m d i na ga ea gs ns es ss) <> (Graph m' d' i' na' ga' ea' gs' ns' es' ss') =
    Graph (m <> m') (d <> d') (i <> i') (na <> na') (ga <> ga') (ea <> ea') (gs <> gs') (ns <> ns') (es <> es') (ss <> ss')

instance Monoid Graph where
  mempty = Graph mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | 'Directed' graph of size 1.
--
-- >>> BS.putStr $ dotPrint defaultDotConfig defaultGraph <> "\n"
-- digraph {
--     node [height=0.5;shape=circle]
--     graph [overlap=false;size="1!";splines=spline]
--     edge [arrowsize=0.5]
--     rankdir="TB"
--     }
defaultGraph :: Graph
defaultGraph =
  mempty
    & set (attL NodeType (ID "height")) (Just $ IDDouble 0.5)
    & set (attL NodeType (ID "shape")) (Just $ ID "circle")
    & set (attL GraphType (ID "overlap")) (Just $ ID "false")
    & set (attL GraphType (ID "size")) (Just $ IDQuoted "1!")
    & set (attL GraphType (ID "splines")) (Just $ ID "spline")
    & set (attL EdgeType (ID "arrowsize")) (Just $ IDDouble 0.5)
    & set #directed (Last (Just Directed))
    & set (gattL (ID "rankdir")) (Just (IDQuoted "TB"))

-- | global attributes lens
gattL :: ID -> Lens' Graph (Maybe ID)
gattL k = #globalAttributes % at k

-- | attributes lens
attL :: AttributeType -> ID -> Lens' Graph (Maybe ID)
attL GraphType k = #graphAttributes % at k
attL NodeType k = #nodeAttributes % at k
attL EdgeType k = #edgeAttributes % at k

outercalate :: ByteString -> [ByteString] -> ByteString
outercalate _ [] = mempty
outercalate a xs = a <> intercalate a xs <> a

instance DotParse Graph where
  dotPrint cfg (Graph me d i na ga ea gs ns es ss) =
    intercalate (cfg ^. #topLevelSep) $
      bool [] ["strict"] (me == Last (Just MergeEdges))
        <> bool ["digraph"] ["graph"] (d == Last (Just UnDirected))
        <> maybe [] ((: []) . dotPrint cfg) (getLast i)
        <> [ wrapCurlyPrint $
               outercalate
                 (cfg ^. #statementSep)
                 ( [dotPrint cfg (AttributeStatement NodeType na)]
                     <> [dotPrint cfg (AttributeStatement GraphType ga)]
                     <> [dotPrint cfg (AttributeStatement EdgeType ea)]
                     <> (dotPrint cfg . GlobalAttributeStatement <$> Map.toList gs)
                     <> (dotPrint cfg <$> ns)
                     <> (dotPrint cfg <$> es)
                     <> (dotPrint cfg <$> ss)
                 )
           ]

  dotParse = token $ do
    me <- dotParse
    d <- dotParse
    i <- optional dotParse
    ss <- wrapCurlyP (many dotParse)
    let g =
          (mempty :: Graph)
            & #strict
            .~ Last (Just me)
            & #directed
            .~ Last (Just d)
            & #graphid
            .~ Last i
    pure $ addStatements ss g

-- * Dot Grammar

-- | MergeEdges (strict)
data Strict = MergeEdges | NoMergeEdges deriving (Eq, Show, Generic)

instance DotParse Strict where
  dotPrint _ MergeEdges = "strict"
  dotPrint _ NoMergeEdges = ""

  dotParse = token $ withOption ($(keyword "strict")) (const $ pure MergeEdges) (pure NoMergeEdges)

-- | Default Strict is NoMergeEdges
defStrict :: Last Strict -> Strict
defStrict (Last Nothing) = NoMergeEdges
defStrict (Last (Just x)) = x

-- | Directed (digraph | graph)
data Directed = Directed | UnDirected deriving (Eq, Show, Generic)

instance DotParse Directed where
  dotPrint _ Directed = "digraph"
  dotPrint _ UnDirected = "graph"

  dotParse =
    token $
      (Directed <$ $(keyword "digraph"))
        <|> (UnDirected <$ $(keyword "graph"))

-- | Default Directed is Directed
defDirected :: Last Directed -> Directed
defDirected (Last Nothing) = Directed
defDirected (Last (Just x)) = x

-- | A dot statement as per the dot language specification.
data Statement = StatementNode NodeStatement | StatementEdge EdgeStatement | StatementGlobalAttribute GlobalAttributeStatement | StatementAttribute AttributeStatement | StatementSubGraph SubGraphStatement deriving (Eq, Show, Generic)

instance DotParse Statement where
  dotPrint cfg (StatementNode x) = dotPrint cfg x
  dotPrint cfg (StatementEdge x) = dotPrint cfg x
  dotPrint cfg (StatementAttribute x) = dotPrint cfg x
  dotPrint cfg (StatementGlobalAttribute x) = dotPrint cfg x
  dotPrint cfg (StatementSubGraph x) = dotPrint cfg x

  dotParse =
    token $
      -- Order is important
      (StatementEdge <$> dotParse)
        <|> (StatementAttribute <$> dotParse)
        <|> (StatementGlobalAttribute <$> dotParse)
        <|> (StatementSubGraph <$> dotParse)
        <|> (StatementNode <$> dotParse)

-- | Identifier as per the dot language specifications.
--
-- >>> runDotParser "0" :: ID
-- IDInt 0
--
-- >>> runDotParser "-.123" :: ID
-- IDDouble (-0.123)
--
-- >>> runParser dotParse "apple_1'" :: Result Error ID
-- OK (ID "apple_1") "'"
--
-- >>> :set -XQuasiQuotes
-- >>> runParser dotParse "\"hello\"" :: Result Error ID
-- OK (IDQuoted "hello") ""
--
-- >>> runDotParser "<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>" :: ID
-- IDHtml "<The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>"
--
-- >>> runDotParser "shape=diamond" :: (ID,ID)
-- (ID "shape",ID "diamond")
--
-- >>> runDotParser "fontname=\"Arial\"" :: (ID,ID)
-- (ID "fontname",IDQuoted "Arial")
--
-- >>> runDotParser "[shape=diamond; color=blue] [label=label]" :: Map.Map ID ID
-- fromList [(ID "color",ID "blue"),(ID "label",ID "label"),(ID "shape",ID "diamond")]
data ID = ID ByteString | IDInt Int | IDDouble Double | IDQuoted ByteString | IDHtml ByteString deriving (Eq, Show, Generic, Ord)

instance DotParse ID where
  dotPrint _ (ID s) = s
  dotPrint _ (IDInt i) = strToUtf8 (show i)
  dotPrint _ (IDDouble x) = strToUtf8 (show x)
  dotPrint _ (IDQuoted x) =
    wrapQuotePrint x
  dotPrint _ (IDHtml s) = s

  -- order matters
  dotParse =
    (ID <$> ident)
      <|> (IDInt <$> (signed int `notFollowedBy` $(char '.')))
      <|> (IDDouble <$> signed double)
      <|> (IDQuoted . strToUtf8 <$> quoted)
      <|> (IDHtml . strToUtf8 <$> htmlLike)

-- | ID as the equivalent plain String
--
-- note that the dot language identifier equivalence law is:
--
-- > x == y if label x == label y
label :: ID -> String
label (ID s) = utf8ToStr s
label (IDInt i) = show i
label (IDDouble d) = show d
label (IDQuoted q) = utf8ToStr q
label (IDHtml h) = utf8ToStr h

-- | Attribute key-value pair of identifiers
instance DotParse (ID, ID) where
  dotPrint cfg (x0, x1) = dotPrint cfg x0 <> "=" <> dotPrint cfg x1

  dotParse = token $
    do
      x0 <- token dotParse
      _ <- token $(symbol "=")
      x1 <- dotParse
      pure (x0, x1)

-- | Attribute collections
--
-- A given entity can have multiple attribute lists. For simplicity, these are mconcat'ed on parsing.
instance DotParse (Map.Map ID ID) where
  dotPrint cfg as =
    bool
      (wrapSquarePrint (intercalate (cfg ^. #attSep) $ dotPrint cfg <$> Map.toList as))
      mempty
      (as == Map.empty)

  dotParse =
    Map.fromList . mconcat . fmap toList
      <$> token (many (wrapSquareP (nonEmptyP dotParse sepP)) <|> ([] <$ wrapSquareP ws))

-- | Compass instructions which are optionally associated with an identifier
data Compass = CompassN | CompassNE | CompassE | CompassSE | CompassS | CompassSW | CompassW | CompassNW | CompassC | Compass_ deriving (Eq, Show, Generic)

instance DotParse Compass where
  dotPrint _ CompassN = "n"
  dotPrint _ CompassNE = "ne"
  dotPrint _ CompassE = "e"
  dotPrint _ CompassSE = "se"
  dotPrint _ CompassS = "s"
  dotPrint _ CompassSW = "sw"
  dotPrint _ CompassW = "w"
  dotPrint _ CompassNW = "nw"
  dotPrint _ CompassC = "c"
  dotPrint _ Compass_ = "_"

  dotParse =
    token
      $( switch
           [|
             case _ of
               "n" -> pure CompassN
               "ne" -> pure CompassNE
               "e" -> pure CompassE
               "se" -> pure CompassSE
               "s" -> pure CompassS
               "sw" -> pure CompassSW
               "w" -> pure CompassW
               "nw" -> pure CompassNW
               "c" -> pure CompassC
               "_" -> pure Compass_
             |]
       )

-- | Port instructions which are optionally associated with an identifier
newtype Port = Port {portID :: These ID Compass} deriving (Eq, Show, Generic)

instance DotParse Port where
  dotPrint cfg (Port (This i)) = ": " <> dotPrint cfg i
  dotPrint cfg (Port (That c)) = ": " <> dotPrint cfg c
  dotPrint cfg (Port (These i c)) = ": " <> dotPrint cfg i <> " : " <> dotPrint cfg c

  dotParse =
    token $
      ((\x0 x1 -> Port (These x0 x1)) <$> ($(symbol ":") *> dotParse) <*> ($(symbol ":") *> dotParse))
        <|> (Port . This <$> ($(symbol ":") *> dotParse))
        <|> (Port . That <$> ($(symbol ":") *> dotParse))

-- | A top-level attribute
--
-- >>> runDotParser "rankdir=\"BT\"" :: Statement
-- StatementGlobalAttribute (GlobalAttributeStatement {globalAttributeStatement = (ID "rankdir",IDQuoted "BT")})
newtype GlobalAttributeStatement = GlobalAttributeStatement {globalAttributeStatement :: (ID, ID)} deriving (Eq, Show, Generic)

instance DotParse GlobalAttributeStatement where
  dotPrint cfg (GlobalAttributeStatement s) = dotPrint cfg s
  dotParse = GlobalAttributeStatement <$> dotParse

-- | Category of attribute
data AttributeType = GraphType | NodeType | EdgeType deriving (Eq, Show, Generic)

instance DotParse AttributeType where
  dotPrint _ GraphType = "graph"
  dotPrint _ NodeType = "node"
  dotPrint _ EdgeType = "edge"

  dotParse =
    token
      (GraphType <$ $(keyword "graph"))
      <|> (NodeType <$ $(keyword "node"))
      <|> (EdgeType <$ $(keyword "edge"))

-- | Top-level attribute statement
--
-- >>> runDotParser "graph [overlap=false, splines=spline, size=\"1!\"];" :: Statement
-- StatementAttribute (AttributeStatement {attributeType = GraphType, attributes = fromList [(ID "overlap",ID "false"),(ID "size",IDQuoted "1!"),(ID "splines",ID "spline")]})
data AttributeStatement = AttributeStatement {attributeType :: AttributeType, attributes :: Map.Map ID ID} deriving (Eq, Show, Generic)

instance DotParse AttributeStatement where
  dotPrint cfg (AttributeStatement t as) =
    bool
      ( intercalate
          " "
          [dotPrint cfg t, dotPrint cfg as]
      )
      mempty
      (mempty == as)

  dotParse = AttributeStatement <$> dotParse <*> dotParse

-- | Node statement
--
-- >>> runDotParser "A [shape=diamond; color=blue]" :: Statement
-- StatementNode (NodeStatement {nodeID = ID "A", port = Nothing, nodeAttrs = fromList [(ID "color",ID "blue"),(ID "shape",ID "diamond")]})
data NodeStatement = NodeStatement {nodeID :: ID, port :: Maybe Port, nodeAttrs :: Map.Map ID ID} deriving (Eq, Show, Generic)

instance DotParse NodeStatement where
  dotPrint cfg (NodeStatement i p as) =
    intercalate " " $
      [dotPrint cfg i]
        <> (dotPrint cfg <$> maybeToList p)
        <> [dotPrint cfg as]

  dotParse = NodeStatement <$> dotParse <*> optional dotParse <*> dotParse

-- | An edge can be specified in as a NodeID or as a SubGraph
data EdgeID
  = EdgeID ID (Maybe Port)
  | EdgeSubGraph SubGraphStatement
  deriving (Eq, Show, Generic)

instance DotParse EdgeID where
  dotPrint cfg (EdgeID e p) =
    mconcat $ [dotPrint cfg e] <> (dotPrint cfg <$> maybeToList p)
  dotPrint cfg (EdgeSubGraph s) = dotPrint cfg s

  dotParse =
    (EdgeID <$> dotParse <*> optional dotParse)
      <|> (EdgeSubGraph <$> dotParse)

-- | An edgeop is -> in directed graphs and -- in undirected graphs.
data EdgeOp = EdgeDirected | EdgeUndirected deriving (Eq, Show, Generic)

instance DotParse EdgeOp where
  dotPrint _ EdgeDirected = "->"
  dotPrint _ EdgeUndirected = "--"

  dotParse =
    token
      $( switch
           [|
             case _ of
               "->" -> pure EdgeDirected
               "--" -> pure EdgeUndirected
             |]
       )

-- | generate an EdgeOp given the type of graph.
fromDirected :: Directed -> EdgeOp
fromDirected Directed = EdgeDirected
fromDirected UnDirected = EdgeUndirected

-- | Edge statement
--
-- >>> runDotParser "A -> B [style=dashed, color=grey]" :: Statement
-- StatementEdge (EdgeStatement {edgeOp = EdgeDirected, leftEdge = EdgeID (ID "A") Nothing, rightEdges = EdgeID (ID "B") Nothing :| [], edgeAttrs = fromList [(ID "color",ID "grey"),(ID "style",ID "dashed")]})
data EdgeStatement = EdgeStatement {edgeOp :: EdgeOp, leftEdge :: EdgeID, rightEdges :: NonEmpty EdgeID, edgeAttrs :: Map.Map ID ID} deriving (Eq, Show, Generic)

instance DotParse EdgeStatement where
  dotPrint cfg (EdgeStatement l rs xs as) =
    intercalate
      " "
      ( [intercalate (" " <> dotPrint cfg l <> " ") (dotPrint cfg <$> (rs : toList xs))]
          <> [dotPrint cfg as]
      )

  dotParse = token $ do
    l <- dotParse
    o0 <- dotParse
    r0 <- dotParse
    ors <- many ((,) <$> dotParse <*> dotParse)
    as <- dotParse
    bool
      (pure (EdgeStatement o0 l (r0 :| (snd <$> ors)) as))
      empty
      (any ((/= o0) . fst) ors)

-- | The edge ID or subgraph ID (if any)
edgeID :: EdgeID -> Maybe ID
edgeID (EdgeID i _) = Just i
edgeID (EdgeSubGraph (SubGraphStatement i _)) = i

-- | edge IDs
edgeIDsNamed :: EdgeStatement -> [(ID, ID)]
edgeIDsNamed e = [(x, y) | (Just x, Just y) <- edgeIDs e]

-- | list of edges in a given EdgeStatement, including anonymous SugGraphs
edgeIDs :: EdgeStatement -> [(Maybe ID, Maybe ID)]
edgeIDs e = zip (id0 : id1) id1
  where
    id0 = edgeID (view #leftEdge e)
    id1 = edgeID <$> toList (view #rightEdges e)

-- | A subgraph statement.
--
-- Note: each subgraph must have a unique name
--
-- >>> runDotParser "subgraph A {A, B, C}" :: Statement
-- StatementSubGraph (SubGraphStatement {subgraphID = Just (ID "A"), subgraphStatements = [StatementNode (NodeStatement {nodeID = ID "A", port = Nothing, nodeAttrs = fromList []}),StatementNode (NodeStatement {nodeID = ID "B", port = Nothing, nodeAttrs = fromList []}),StatementNode (NodeStatement {nodeID = ID "C", port = Nothing, nodeAttrs = fromList []})]})
data SubGraphStatement = SubGraphStatement {subgraphID :: Maybe ID, subgraphStatements :: [Statement]} deriving (Eq, Show, Generic)

instance DotParse SubGraphStatement where
  dotPrint cfg (SubGraphStatement x xs) =
    intercalate " " $
      maybe
        []
        (\x' -> [intercalate " " ["subgraph", dotPrint cfg x']])
        x
        <> (: []) (wrapCurlyPrint (intercalate (cfg ^. #subGraphSep) $ dotPrint cfg <$> xs))

  dotParse = token $ do
    x <- optional ($(keyword "subgraph") *> dotParse)
    pure (SubGraphStatement x) <*> wrapCurlyP (many (optional sepP *> dotParse))

-- | add a graphviz statement to a 'Graph'
addStatement :: Statement -> Graph -> Graph
addStatement (StatementNode n) g = g & #nodes %~ (n :)
addStatement (StatementEdge e) g = g & #edges %~ (e :)
addStatement (StatementSubGraph s) g = g & #subgraphs %~ (s :)
addStatement (StatementAttribute (AttributeStatement GraphType as)) g = g & #graphAttributes %~ (<> as)
addStatement (StatementAttribute (AttributeStatement NodeType as)) g = g & #nodeAttributes %~ (<> as)
addStatement (StatementAttribute (AttributeStatement EdgeType as)) g = g & #edgeAttributes %~ (<> as)
addStatement (StatementGlobalAttribute (GlobalAttributeStatement s)) g = g & #globalAttributes %~ (<> Map.fromList [s])

-- | add a list of graphviz statements to a 'Graph'
addStatements :: [Statement] -> Graph -> Graph
addStatements ss g = Prelude.foldr addStatement g (Prelude.reverse ss)

-- | run a dot string through graphviz, supplying arguments and collecting stdout
processDotWith :: Directed -> [String] -> ByteString -> IO ByteString
processDotWith d args i = do
  let cmd = case d of
        Directed -> "dot"
        UnDirected -> "neato"
  (r, input, e) <- readProcessWithExitCode cmd args i
  bool
    (error $ utf8ToStr e)
    (pure input)
    (r == ExitSuccess)

-- | run a dot string through graphviz, collecting the augmented dot string output
processDot :: Directed -> ByteString -> IO ByteString
processDot d = processDotWith d ["-Tdot"]

-- | Augment a Graph via the graphviz process
processGraphWith :: DotConfig -> Graph -> IO Graph
processGraphWith cfg g =
  runDotParser <$> processDot (defDirected $ view #directed g) (dotPrint cfg g)

-- | Augment a Graph via the graphviz process
processGraph :: Graph -> IO Graph
processGraph g =
  runDotParser <$> processDot (defDirected $ view #directed g) (dotPrint defaultDotConfig g)

instance DotParse (Point Double) where
  dotPrint _ (Point x y) =
    intercalate "," $
      strToUtf8 . show <$> [x, y]

  dotParse = token pointP

pointI :: Iso' ID (Point Double)
pointI =
  iso
    (runParser_ pointP . strToUtf8 . label)
    (IDQuoted . dotPrint defaultDotConfig)

instance DotParse (Rect Double) where
  dotPrint _ (Rect x z y w) =
    intercalate "," $
      strToUtf8 . show <$> [x, y, z, w]

  dotParse = token rectP

rectI :: Iso' ID (Rect Double)
rectI =
  iso
    (runParser_ rectP . strToUtf8 . label)
    (IDQuoted . dotPrint defaultDotConfig)

-- | Bounding box ID lens
bb_ :: Lens' Graph (Maybe ID)
bb_ = #graphAttributes % at (ID "bb")

-- | Bounding Box lens as a 'Rect'
bbL :: Lens' Graph (Maybe (Rect Double))
bbL = lens (preview (bb_ % _Just % rectI)) (\g r -> set bb_ (review rectI <$> r) g)

-- | nodes lens
nodesPortL :: Lens' Graph (Map.Map ID (Maybe Port, Map.Map ID ID))
nodesPortL =
  lens
    ( \g ->
        g
          & view #nodes
          & fmap (\x -> (view #nodeID x, (view #port x, view #nodeAttrs x)))
          & Map.fromList
    )
    (\g m -> g & #nodes .~ ((\(n, (p, a)) -> NodeStatement n p a) <$> Map.toList m))

-- | nodes lens ignoring/forgetting port information
nodesL :: Lens' Graph (Map.Map ID (Map.Map ID ID))
nodesL =
  lens
    ( \g ->
        g
          & view #nodes
          & fmap (\x -> (view #nodeID x, view #nodeAttrs x))
          & Map.fromList
    )
    (\g m -> g & #nodes .~ ((\(n, a) -> NodeStatement n Nothing a) <$> Map.toList m))

-- | edges lens ignoring/forgetting port information
edgesL :: Lens' Graph (Map.Map (ID, ID) (Map.Map ID ID))
edgesL =
  lens getEdges_ setEdges_

-- | edge & attribute map
-- ignores anonymous subgraphs
getEdges_ :: Graph -> Map.Map (ID, ID) (Map.Map ID ID)
getEdges_ g =
  Map.fromList $
    mconcat $
      fmap
        (\(xs, a) -> (,a) <$> xs)
        [(edgeIDsNamed e, view #edgeAttrs e) | e <- view #edges g]

setEdges_ :: Graph -> Map.Map (ID, ID) (Map.Map ID ID) -> Graph
setEdges_ g m =
  g
    & #edges
    .~ ( ( \((x0, x1), as) ->
             EdgeStatement
               (fromDirected (defDirected $ view #directed g))
               (EdgeID x0 Nothing)
               (EdgeID x1 Nothing :| [])
               as
         )
           <$> Map.toList m
       )

-- | A specific attribute for all nodes in a graph
nodesA :: ID -> Graph -> Map.Map ID (Maybe ID)
nodesA a g = fmap (Map.lookup a) (view nodesL g)

-- | node position (as a Point)
nodePos :: Graph -> Map.Map ID (Maybe (Point Double))
nodePos = fmap (fmap (view pointI)) . nodesA (ID "pos")

--

-- | Specific attribute for all edges
edgesA :: Graph -> ID -> Map.Map (ID, ID) (Maybe ID)
edgesA g a = fmap (Map.lookup a) (view edgesL g)

-- | node width attributes
nodeWidth :: Graph -> Map.Map ID (Maybe Double)
nodeWidth g =
  fmap
    ( \case
        Just (IDDouble x') -> Just x'
        _ -> Nothing
    )
    $ nodesA (ID "width") g

-- | edge width attributes
edgeWidth :: Graph -> Map.Map (ID, ID) (Maybe Double)
edgeWidth g =
  fmap
    ( \case
        Just (IDDouble x') -> Just x'
        _ -> Nothing
    )
    $ edgesA g (ID "width")

-- | edge path attributes
edgeSpline :: Graph -> Map.Map (ID, ID) (Maybe Spline)
edgeSpline g =
  fmap
    ( \case
        Just (IDQuoted x') -> Just (runParser_ splineP x')
        _ -> Nothing
    )
    $ edgesA g (ID "pos")

-- | typical node information after processing a dot bytestring.
data NodeInfo = NodeInfo {nlabel :: ID, nwidth :: Double, pos :: Point Double} deriving (Eq, Show, Generic)

-- | Create a list of NodeInfo from a graph.
nodeInfo :: Graph -> Double -> [NodeInfo]
nodeInfo g w = [NodeInfo x (fromMaybe w (join w')) p | (x, (Just p, w')) <- xs]
  where
    xs =
      Map.toList $
        merge
          (mapMissing (\_ v -> (v, Nothing)))
          dropMissing
          (zipWithMatched (\_ x y -> (x, Just y)))
          (nodePos g)
          (nodeWidth g)

-- | typical edge information after processing a dot bytestring.
data EdgeInfo = EdgeInfo {elabel :: (ID, ID), ewidth :: Double, curve :: [PathData Double]} deriving (Eq, Show, Generic)

-- | Create a list of EdgeInfo from a graph
edgeInfo :: Graph -> Double -> [EdgeInfo]
edgeInfo g w = [EdgeInfo (x, y) (fromMaybe w (join w')) (splinePath p) | ((x, y), (Just p, w')) <- xs]
  where
    xs =
      Map.toList $
        merge
          (mapMissing (\_ v -> (v, Nothing)))
          dropMissing
          (zipWithMatched (\_ x y -> (x, Just y)))
          (edgeSpline g)
          (edgeWidth g)

-- |
--
-- https://graphviz.org/docs/attr-types/splineType/
-- format of the example is end point point and then triples (5,8,11 lengths are 1, 2 and 3 cubics)
splinePath :: Spline -> [PathData Double]
splinePath sp = s' <> p1' <> cs <> e'
  where
    s' = maybe [] (\s -> [StartP s, LineP $ view #splineP1 sp]) (view #splineStart sp)
    e' = maybe [] (\e -> [LineP e]) (view #splineEnd sp)
    p1' = [StartP (view #splineP1 sp)]
    cs = (\(x, y, z) -> CubicP x y z) <$> view #splineTriples sp

-- | create Statements from a (no edge label) algebraic graph
toStatements :: Directed -> G.Graph ByteString -> [Statement]
toStatements d g =
  ((\x -> StatementNode $ NodeStatement (IDQuoted x) Nothing Map.empty) <$> G.vertexList g)
    <> ( ( \(x, y) ->
             StatementEdge $
               EdgeStatement
                 (fromDirected d)
                 (EdgeID (IDQuoted x) Nothing)
                 (fromList [EdgeID (IDQuoted y) Nothing])
                 Map.empty
         )
           <$> G.edgeList g
       )

-- | Various configutaion parameters for the chart-svg Chart
data ChartConfig = ChartConfig
  { chartHeight :: Double,
    chartScale :: Double,
    edgeSize :: Double,
    nodeBorderSize :: Double,
    chartColor :: Colour,
    chartBackgroundColor :: Colour,
    backupNodeHeight :: Double,
    backupNodeWidth :: Double,
    chartVshift :: Double,
    textSize :: Double,
    escapeText :: EscapeText
  }
  deriving (Generic, Show, Eq)

-- | default parameters
defaultChartConfig :: ChartConfig
defaultChartConfig = ChartConfig 500 72 0.5 1 (grey 0.4 0.8) (grey 0.5 0.2) 0.5 0.5 (-6) 16 NoEscapeText

-- | convert a 'Graph' processed via the graphviz commands to a 'ChartOptions'
graphToChartWith :: ChartConfig -> (ID -> Text) -> Graph -> ChartOptions
graphToChartWith cfg labelf g =
  mempty
    & set #chartTree (named "edges" ps <> named "shapes" c0 <> named "labels" [ts])
    & set (#chartTree % charts' % each % #chartStyle % #scaleP) ScalePArea
    & #markupOptions
    % #markupHeight
    .~ (Just $ cfg ^. #chartHeight)
    & #markupOptions
    % #chartAspect
    .~ ChartAspect
    & #hudOptions
    .~ mempty
  where
    glyphs w = case view (attL NodeType (ID "shape")) g of
      Just (ID "circle") -> defaultGlyphStyle & #glyphShape .~ CircleGlyph & #size .~ (cfg ^. #chartScale) * w & #borderSize .~ (cfg ^. #edgeSize) & #borderColor .~ (cfg ^. #chartColor) & #color .~ (cfg ^. #chartBackgroundColor)
      Just (ID "box") -> defaultGlyphStyle & #glyphShape .~ RectSharpGlyph (h / w) & #size .~ (cfg ^. #chartScale) * w & #borderSize .~ view #nodeBorderSize cfg & #borderColor .~ (cfg ^. #chartColor) & #color .~ (cfg ^. #chartBackgroundColor)
      -- defaults to circle
      _ -> defaultGlyphStyle & #glyphShape .~ CircleGlyph & #size .~ (cfg ^. #chartScale) * w & #borderSize .~ view #nodeBorderSize cfg & #borderColor .~ (cfg ^. #chartColor) & #color .~ (cfg ^. #chartBackgroundColor)
    h = maybe (cfg ^. #backupNodeHeight) (runParser_ double . strToUtf8 . label) (view (attL NodeType (ID "height")) g)
    vshift' = cfg ^. #chartVshift
    -- node information
    ns = nodeInfo g (cfg ^. #backupNodeWidth)
    -- edge information
    es = edgeInfo g (cfg ^. #edgeSize)
    -- paths
    ps = fmap (\(EdgeInfo _ w p) -> PathChart (defaultPathStyle & #borderSize .~ (2 * w) & #borderColor .~ (cfg ^. #chartColor) & #color .~ transparent) p) es
    -- circles
    c0 = fmap (\(NodeInfo _ w p) -> GlyphChart (glyphs w) [p]) ns
    -- labels
    ts =
      TextChart (defaultTextStyle & #escapeText .~ (cfg ^. #escapeText) & #size .~ (cfg ^. #textSize) & #color .~ (cfg ^. #chartColor)) ((\(NodeInfo l _ (Point x y)) -> (labelf l, Point x (vshift' + y))) <$> ns)

-- | convert a 'Graph' processed via the graphviz commands to a 'ChartOptions' using the default ChartConfig.
graphToChart :: Graph -> ChartOptions
graphToChart = graphToChartWith defaultChartConfig (Text.pack . label)

-- | Convert an algebraic graph to a dotparse graph.
toDotGraphWith :: Directed -> Graph -> G.Graph ByteString -> Graph
toDotGraphWith d g gg = g & #directed .~ Last (Just d) & addStatements (toStatements d gg)

-- | Convert an algebraic graph to a dotparse graph, starting with the 'defaultGraph'.
toDotGraph :: G.Graph ByteString -> Graph
toDotGraph = toDotGraphWith Directed defaultGraph
