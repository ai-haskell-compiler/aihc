module Sound.Audacity.XML where

import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name.MixedCase as Name

import qualified Data.List as List

import Prelude hiding (unlines)


tag ::
   String -> a ->
   [Attr.T Name.T (a -> String)] ->
   [[Tag.T Name.T String]] ->
   [[Tag.T Name.T String]]
tag name x attrs enclosed =
   let tagName = Tag.Name $ Name.Cons name
   in  [Tag.open tagName $ Attr.mapValues ($ x) attrs] :
       enclosed ++
       [Tag.close tagName] :
       []

attr :: String -> a -> Attr.T Name.T a
attr name value  =  Attr.cons (Attr.Name $ Name.Cons name) value

unlines :: [[Tag.T Name.T String]] -> [Tag.T Name.T String]
unlines =
   concat . snd .
   List.mapAccumL
      (\oldIndent tags ->
         let newIndent = oldIndent + sum (map tagIndent tags)
         in  (newIndent,
              Tag.text (replicate (min oldIndent newIndent) '\t') :
              tags ++
              Tag.text "\n" :
              []))
      0

tagIndent :: Tag.T Name.T String -> Int
tagIndent t =
   case t of
      Tag.Open _ _ -> 1
      Tag.Close _ -> -1
      _ -> 0
