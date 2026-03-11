module Text.HTML.WraXML.Document (
   XmlDoc.T(..), XmlDoc.lift,
   decodeAdaptive, format,
   ) where

import qualified Text.HTML.WraXML.Tree    as HtmlTree
import qualified Text.XML.WraXML.Document as XmlDoc
import qualified Text.XML.WraXML.String  as XmlString
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Format as Format
import qualified Text.HTML.Basic.String as HtmlStringB
import qualified Text.HTML.Basic.Character as HtmlChar

import Control.Monad.Trans.State (State, put, get, )

import Control.Applicative (liftA2, )
import Data.Traversable (traverse, )


format ::
   (Name.Tag name, Name.Attribute name, Format.C string) =>
   XmlDoc.T i name string -> ShowS
format (XmlDoc.Cons xml dtd trees) =
   let (formatHTML, formatXMLDecl) =
          maybe
             (HtmlTree.formatMany, id)
             (\xmlDecl ->
                 (HtmlTree.formatManyXHTML,
                  XmlDoc.formatXMLDeclaration xmlDecl)) xml
   in  formatXMLDecl .
       maybe id XmlDoc.formatDocType dtd .
       formatHTML trees


decodeAdaptive ::
   (Name.Attribute name, Name.Tag name) =>
   (XmlString.Encoding -> XmlString.Encoded -> String) ->
   XmlDoc.T i name [HtmlChar.T] ->
   State (XmlString.Encoded -> String) (XmlDoc.T i name String)
decodeAdaptive getDecoder (XmlDoc.Cons xml0 dtd trees0) =
   liftA2
      (\xml1 trees1 -> XmlDoc.Cons xml1 dtd trees1)
      (do decoder <- get
          let xml1 = fmap (map (fmap (HtmlStringB.decode decoder))) xml0
          maybe
             (return ())
             (put . getDecoder) $
             Attr.lookup Attr.encodingName =<< xml1
          return xml1)
      (traverse (HtmlTree.decodeAdaptive getDecoder) trees0)
