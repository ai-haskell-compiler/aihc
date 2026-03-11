module XmlParser.NodeConsumerState
  ( NodeConsumerState,
    new,
    bumpOffset,
    getOffset,
    fetchNode,
    lookupNamespace,
    getNamespaceRegistry,
  )
where

import qualified Text.XML as Xml
import qualified XmlParser.NamespaceRegistry as NamespaceRegistry
import XmlParser.Prelude

data NodeConsumerState
  = NodeConsumerState
      -- | Nodes.
      [Xml.Node]
      -- | Offset.
      Int
      -- | Namespace registry.
      NamespaceRegistry.NamespaceRegistry

new :: [Xml.Node] -> NamespaceRegistry.NamespaceRegistry -> NodeConsumerState
new nodes nsReg = NodeConsumerState nodes 0 nsReg

bumpOffset :: NodeConsumerState -> NodeConsumerState
bumpOffset (NodeConsumerState a b c) = NodeConsumerState a (succ b) c

getOffset :: NodeConsumerState -> Int
getOffset (NodeConsumerState _ x _) = x

fetchNode :: NodeConsumerState -> Maybe (Xml.Node, NodeConsumerState)
fetchNode (NodeConsumerState nodes offset nsReg) =
  case nodes of
    nodesHead : nodesTail -> Just (nodesHead, NodeConsumerState nodesTail offset nsReg)
    _ -> Nothing

lookupNamespace :: Text -> NodeConsumerState -> Maybe Text
lookupNamespace ns (NodeConsumerState _ _ nsReg) =
  NamespaceRegistry.lookup ns nsReg

getNamespaceRegistry :: NodeConsumerState -> NamespaceRegistry.NamespaceRegistry
getNamespaceRegistry (NodeConsumerState _ _ x) = x
