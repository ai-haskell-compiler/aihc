{-| FGL To Dot is an automatic translation and labeling
    of FGL graphs (see the 'Graph' class) to graphviz
    Dot format that can be written out to a file and
    displayed.

@
    let dot = showDot (fglToDot graph)
    writeFile \"file.dot\" dot
    system(\"dot -Tpng -ofile.png file.dot\")
@
-}

module Data.Graph.Inductive.Dot
  ( fglToDot, fglToDotString, fglToDotUnlabeled, fglToDotGeneric
  , showDot
  ) where

import Control.Monad
import Data.Graph.Inductive
import Text.Dot

-- |Generate a Dot graph using the show instances of the node and edge labels as displayed graph labels
fglToDot :: (Show a, Show b, Graph gr) => gr a b -> Dot ()
fglToDot gr = fglToDotGeneric gr show show id

-- |Generate a Dot graph using the Node and Edge strings as labels
fglToDotString :: Graph gr => gr String String -> Dot ()
fglToDotString gr = fglToDotGeneric gr id id id

-- |Generate a Dot graph without any edge or node labels
fglToDotUnlabeled :: Graph gr => gr a b -> Dot ()
fglToDotUnlabeled gr = fglToDotGeneric gr undefined undefined (const [])

-- |Generate a Dot graph using the provided functions to mutate the node labels, edge labels and list of attributes.
fglToDotGeneric :: Graph gr => gr a b -> (a -> String) -> (b -> String) -> ([(String,String)] -> [(String,String)]) -> Dot ()
fglToDotGeneric gr nodeConv edgeConv attrConv = do
  let es = labEdges gr -- :: [(Int, Int, b)]
      ns = labNodes gr -- :: [(Int, a)]
  mapM_ (\(n,p) -> userNode (userNodeId n) (attrConv [("label", nodeConv p)])) ns
  mapM_ (\(a,b,p) -> edge (userNodeId a) (userNodeId b) (attrConv [("label", edgeConv p)])) es
