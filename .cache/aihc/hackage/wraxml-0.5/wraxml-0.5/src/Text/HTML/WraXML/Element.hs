module Text.HTML.WraXML.Element (
   module Text.HTML.WraXML.Element,
   T, name_, attributes_,
   Elem.cons, Elem.name, Elem.attributes,
   Elem.checkName, Elem.format,
   ) where

import Text.XML.WraXML.Element (T, name_, attributes_, )

import qualified Text.XML.WraXML.Element as Elem

import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.WraXML.String as XmlString

import qualified Text.HTML.Basic.Tag as Tag
import qualified Text.HTML.Basic.Character as HtmlChar
import qualified Text.HTML.Basic.String as HtmlString

import Control.Monad.Trans.State (State, put, get, )


-- * decode

decodeAdaptive ::
   (Name.Attribute name, Name.Tag name) =>
   (XmlString.Encoding -> XmlString.Encoded -> String) ->
   T name [HtmlChar.T] ->
   State (HtmlString.Encoded -> String) (T name String)
decodeAdaptive getDecoder elem0 =
   do decoder <- get
      let elem1 =
             fmap (HtmlString.decode decoder) elem0
      maybe
         (return ())
         (put . getDecoder) $
         (Tag.maybeMetaEncoding
             (name_ elem1) (attributes_ elem1))
      return elem1
