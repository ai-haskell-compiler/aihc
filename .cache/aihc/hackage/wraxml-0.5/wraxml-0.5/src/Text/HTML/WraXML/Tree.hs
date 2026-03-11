module Text.HTML.WraXML.Tree where

import qualified Data.Tree.BranchLeafLabel as Tree
import qualified Text.XML.WraXML.Tree    as XmlTree
import qualified Text.XML.WraXML.String  as XmlString
import qualified Data.Char               as Char

import qualified Text.XML.Basic.Tag as TagX
import qualified Text.HTML.Basic.Tag as Tag
import qualified Text.HTML.Basic.Character as HtmlChar
import qualified Text.HTML.Basic.String as HtmlStringB
import qualified Text.HTML.WraXML.Element as Elem
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name

import Text.XML.WraXML.Tree (formatLeaf, )
import qualified Text.XML.Basic.Format as Format

import Control.Monad.Trans.State (State, put, get, )
import Control.Applicative (liftA, liftA2, )

import qualified Data.List.Reverse.StrictElement as Rev
import           Data.Tuple.HT (mapFst, )
import           Control.Monad (liftM2, )
import           Data.Maybe (mapMaybe, fromMaybe, )



{- * Character decoding -}

findMetaEncoding ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name String -> Maybe String
findMetaEncoding =
   fmap (map Char.toLower . Rev.takeWhile ('='/=)) .
   lookup "content-type" .
   map (mapFst (map Char.toLower)) .
   getMetaHTTPHeaders

{- |
Extract META tags which contain HTTP-EQUIV attribute
and present these values like HTTP headers.
-}
getMetaHTTPHeaders ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name String -> [(String, String)]
getMetaHTTPHeaders =
   mapMaybe (\attrs ->
      liftM2 (,)
         (Attr.lookupLit "http-equiv" attrs)
         (Attr.lookupLit "content" attrs)) .
   map Elem.attributes_ .
   filter (Elem.checkName (Name.match "meta")) .
   map fst .
   mapMaybe XmlTree.maybeTag .
   concatMap snd .
   XmlTree.filterTagsFlatten (Elem.checkName (Name.match "head"))


{- |
Decode strings in a HTML tree.
Switch decoding on every occurence of a content-type meta-tag.
This must operate on @HtmlString@s, that is before reference resolution,
since after reference resolution
Unicode characters may clash with encoded characters.
-}
decodeAdaptive ::
   (Name.Attribute name, Name.Tag name) =>
   (XmlString.Encoding -> XmlString.Encoded -> String) ->
   XmlTree.T i name [HtmlChar.T] ->
   State (XmlString.Encoded -> String) (XmlTree.T i name String)
decodeAdaptive getDecoder =
   XmlTree.fold
      (liftA . XmlTree.wrap2)
      (\elm subTrees ->
         liftA2 (Tree.Branch . XmlTree.Tag)
            (Elem.decodeAdaptive getDecoder elm)
            (fmap (map XmlTree.unwrap) $ sequence subTrees))
      (liftA Tree.Leaf .
       decodeLeafAdaptive getDecoder)

decodeLeafAdaptive ::
   (Name.Attribute name, Name.Tag name) =>
   (XmlString.Encoding -> XmlString.Encoded -> String) ->
   XmlTree.Leaf name [HtmlChar.T] ->
   State (XmlString.Encoded -> String) (XmlTree.Leaf name String)
decodeLeafAdaptive getDecoder leaf0 =
   do decoder <- get
      let leaf1 =
             maybe
                (fmap (HtmlStringB.decode decoder) leaf0)
                (XmlTree.CData . decoder)
                (XmlTree.maybeCDataLeaf leaf0)
      -- this should not happen in correct XML file
      maybe
         (return ())
         (put . getDecoder) $
         uncurry TagX.maybeXMLEncoding =<<
         XmlTree.maybeProcessingLeaf leaf1
      return leaf1


{-# DEPRECATED decodeSpecialCharsMetaEncoding "This calls findMetaEncoding which is a potential space leak. Better use decodeAdaptive." #-}

{- |
Convert special characters of XmlString into Unicode
according to the encoding given in a META HTTP-EQUIV tag.
-}
decodeSpecialCharsMetaEncoding ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name XmlString.T -> [XmlTree.T i name String]
decodeSpecialCharsMetaEncoding tree =
   let unicodeTree = XmlTree.unescape tree
   in  fromMaybe
          [unicodeTree]
          (flip XmlTree.maybeDecodeSpecialChars tree
               =<< findMetaEncoding unicodeTree)



{- * Formatting -}


{-
show ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name XmlString.T -> String
show leaf = shows leaf ""
-}

formatMany ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   [XmlTree.T i name string] -> ShowS
formatMany = Format.many format

-- cf. src/Text/ML/HXT/DOM/XmlTreeFunctions.hs
format ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   XmlTree.T i name string -> ShowS
format =
   Tree.fold (flip const) formatBranch formatLeaf .
   XmlTree.unwrap

formatBranch ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   XmlTree.Branch name string -> [ShowS] -> ShowS
formatBranch = formatBranchGen False


formatManyXHTML ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   [XmlTree.T i name string] -> ShowS
formatManyXHTML = Format.many formatXHTML

-- cf. src/Text/XML/HXT/DOM/XmlTreeFunctions.hs
formatXHTML ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   XmlTree.T i name string -> ShowS
formatXHTML =
   Tree.fold (flip const) formatBranchXHTML formatLeaf .
   XmlTree.unwrap

formatBranchXHTML ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   XmlTree.Branch name string -> [ShowS] -> ShowS
formatBranchXHTML = formatBranchGen True


{- |
@not xhtml@: show @<br>@
@xhtml@: show @<br/>@
Unfortunately we cannot generally merge @<tag></tag>@ to @<tag/>@
since browsers expect e.g. separated @<div></div>@.
-}
formatBranchGen ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   Bool -> XmlTree.Branch name string -> [ShowS] -> ShowS
formatBranchGen xhtml branch formatSubTrees =
   case branch of
      XmlTree.Tag elm ->
         Elem.format
            (\tagName -> null formatSubTrees && Tag.isEmpty tagName)
            (if xhtml then Format.slash else id)
            elm formatSubTrees
