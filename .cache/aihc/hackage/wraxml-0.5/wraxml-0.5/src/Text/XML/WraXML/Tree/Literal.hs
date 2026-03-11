{- |
This module allows construction of XML trees
using String literals instead of names.
-}
module Text.XML.WraXML.Tree.Literal (
   XmlTree.T,
   processing, processingIndex,
   tag, tagAttr, tagIndexAttr,
   XmlTree.literal, XmlTree.literalIndex,
   XmlTree.comment, XmlTree.commentIndex,
   XmlTree.warning, XmlTree.warningIndex,
   XmlTree.cdata,   XmlTree.cdataIndex,
   ) where

import qualified Text.XML.WraXML.Tree as XmlTree

import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.ProcessingInstruction as PI

import Prelude hiding (show, shows)



processing ::
   (Name.Tag name, Name.Attribute name) =>
   String -> PI.T name str -> XmlTree.T i name str
processing =
   XmlTree.processing . Name.fromString

processingIndex ::
   (Name.Tag name, Name.Attribute name) =>
   i -> String -> PI.T name str -> XmlTree.T i name str
processingIndex i =
   XmlTree.processingIndex i . Name.fromString

tag ::
   (Name.Tag name, Name.Attribute name) =>
   String -> [XmlTree.T i name str] -> XmlTree.T i name str
tag = XmlTree.tag . Name.fromString

tagAttr ::
   (Name.Tag name, Name.Attribute name) =>
   String -> [(String,str)] -> [XmlTree.T i name str] -> XmlTree.T i name str
tagAttr name attrs =
   XmlTree.tagAttr (Name.fromString name)
      (map (uncurry Attr.new) attrs)

tagIndexAttr ::
   (Name.Tag name, Name.Attribute name) =>
   i -> String -> [(String,str)] -> [XmlTree.T i name str] -> XmlTree.T i name str
tagIndexAttr i name attrs =
   XmlTree.tagIndexAttr i (Name.fromString name)
      (map (uncurry Attr.new) attrs)
