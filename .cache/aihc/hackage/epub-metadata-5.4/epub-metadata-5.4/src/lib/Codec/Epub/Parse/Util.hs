{-# LANGUAGE Arrows #-}

-- | Helper functions used by the other parsing modules
module Codec.Epub.Parse.Util
   ( atQTag
   , mbQTagText
   , mbGetAttrValue
   , mbGetQAttrValue
   , notNullA
   , text
   , dcName
   , opfName
   , xmlName
   )
   where

import Control.Arrow.ListArrows ( ArrowList, (>>^), (>>>), constA, deep,
  getChildren, isA, orElse )
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow ( ArrowXml, getAttrValue, getQAttrValue,
  getText, hasQName, isElem )
import Text.XML.HXT.DOM.TypeDefs ( QName, XmlTree, XNode, mkQName )


-- HXT helpers

{- Not used at this time. But may be used someday

atTag :: (ArrowXml a) => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)
-}


{- | Shortcut arrow to drill down to a specific namespaced child
   element
-}
atQTag :: (ArrowXml a) => QName -> a (NTree XNode) XmlTree
atQTag tag = deep (isElem >>> hasQName tag)


-- | Shortcut arrow to gather up the text part of all child nodes
text :: (ArrowXml a) => a (NTree XNode) String
text = getChildren >>> getText


-- | Arrow that succeeds if the input is not the empty list
notNullA :: (ArrowList a) => a [b] [b]
notNullA = isA $ not . null


{- | Shortcut arrow to retrieve the contents of a namespaced element
   as a Maybe String
-}
mbQTagText :: (ArrowXml a) => QName -> a (NTree XNode) (Maybe String)
mbQTagText tag =
   ( atQTag tag >>>
     text >>> notNullA >>^ Just )
   `orElse`
   (constA Nothing)


{- | Shortcut arrow to retrieve an attribute of an element as a
   Maybe String
-}
mbGetAttrValue :: (ArrowXml a) =>
   String -> a XmlTree (Maybe String)
mbGetAttrValue n =
   (getAttrValue n >>> notNullA >>^ Just)
   `orElse` (constA Nothing)


{- | Shortcut arrow to retrieve an attribute of a namespaced element
   as a Maybe String
-}
mbGetQAttrValue :: (ArrowXml a) =>
   QName -> a XmlTree (Maybe String)
mbGetQAttrValue qn =
   (getQAttrValue qn >>> notNullA >>^ Just)
   `orElse` (constA Nothing)


-- | Construct a qualified name in the Dublin Core namespace
dcName :: String -> QName
dcName local = mkQName "dc" local "http://purl.org/dc/elements/1.1/"


-- | Construct a qualified name in the epub OPF namespace
opfName :: String -> QName
opfName local = mkQName "opf" local "http://www.idpf.org/2007/opf"


-- | Construct a qualified name in the XML namespace
xmlName :: String -> QName
xmlName local = mkQName "xml" local "http://www.w3.org/XML/1998/namespace"
