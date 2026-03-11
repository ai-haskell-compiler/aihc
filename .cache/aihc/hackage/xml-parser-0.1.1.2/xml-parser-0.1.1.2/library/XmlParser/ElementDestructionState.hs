module XmlParser.ElementDestructionState
  ( ElementDestructionContext (..),
    ElementDestructionState,
    new,
    resolveAttributeNames,
    resolveChildNames,
  )
where

import qualified Text.XML as Xml
import qualified XmlParser.NameMap as NameMap
import qualified XmlParser.NamespaceRegistry as NamespaceRegistry
import XmlParser.Prelude

-- |
-- Context needed for the reading by functions of this component.
-- They however do not change these values.
--
-- You can use this as a parameter to a reader monad.
data ElementDestructionContext
  = ElementDestructionContext
      -- | Namespace registry as seen from the context of this node.
      NamespaceRegistry.NamespaceRegistry
      -- | The node that we're in the context of.
      Xml.Element

-- |
-- Used for the state of a parser in the context of specifically element node.
--
-- It is basically the state of the context of the parser.
--
-- You can use this as a parameter to the state monad.
data ElementDestructionState
  = ElementDestructionState
      -- | Cached attribute by name lookup map.
      (Maybe (NameMap.NameMap Text))
      -- | Cached child element by name lookup map.
      (Maybe (NameMap.NameMap Xml.Element))

new :: ElementDestructionState
new = ElementDestructionState Nothing Nothing

-- |
-- Cache attribute names once and return them.
resolveAttributeNames :: ElementDestructionContext -> ElementDestructionState -> (NameMap.NameMap Text, ElementDestructionState)
resolveAttributeNames
  (ElementDestructionContext nreg (Xml.Element _ attributes _))
  (ElementDestructionState attributeByNameMap childByNameMap) =
    let resolvedAttributeByNameMap = fromMaybe (NameMap.fromAttributes nreg attributes) attributeByNameMap
     in (resolvedAttributeByNameMap, ElementDestructionState (Just resolvedAttributeByNameMap) childByNameMap)

-- |
-- Cache child names once and return them.
resolveChildNames :: ElementDestructionContext -> ElementDestructionState -> (NameMap.NameMap Xml.Element, ElementDestructionState)
resolveChildNames
  (ElementDestructionContext nreg (Xml.Element _ _ nodes))
  (ElementDestructionState attributeByNameMap childByNameMap) =
    let resolvedChildByNameMap = fromMaybe (NameMap.fromNodes nreg nodes) childByNameMap
     in (resolvedChildByNameMap, ElementDestructionState attributeByNameMap (Just resolvedChildByNameMap))
