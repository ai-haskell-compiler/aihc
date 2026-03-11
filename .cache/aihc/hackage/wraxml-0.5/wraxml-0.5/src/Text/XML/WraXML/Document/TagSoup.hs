module Text.XML.WraXML.Document.TagSoup where

import qualified Text.XML.WraXML.Tree.TagSoup as TreeTagSoup
import qualified Text.XML.WraXML.Document as XmlDoc
-- import qualified Text.XML.WraXML.Tree     as XmlTree

import Text.XML.WraXML.Tree.TagSoup (PosTag, )

import qualified Text.HTML.TagSoup as Tag

import Text.HTML.TagSoup (Tag(..), )

import qualified Text.XML.Basic.Position as Position

import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name.MixedCase as NameMC
import qualified Text.XML.Basic.Name.LowerCase as NameLC
import qualified Text.XML.Basic.Name as Name

import Control.Monad.Trans.State (State, state, evalState, modify, gets, )

import Data.Char (isSpace, )



dropSpace ::[PosTag] -> [PosTag]
dropSpace =
   dropWhile
      (\tag ->
          case snd tag of
             Tag.TagText text -> all isSpace text
             _ -> False)

withoutLeadingSpace ::
   ([PosTag] -> (a, [PosTag])) ->
   State [PosTag] a
withoutLeadingSpace f =
   modify dropSpace >> state f

toXmlDocument ::
   (Name.Tag name, Name.Attribute name) =>
   [Tag String] -> XmlDoc.T Position.T name String
toXmlDocument ts =
   flip evalState
      (TreeTagSoup.removeMetaPos
          (TreeTagSoup.attachPos
              (Tag.canonicalizeTags ts))) $
   do xml <- withoutLeadingSpace $ \ts0 ->
         case ts0 of
            (_, Tag.TagOpen "?xml" attrs):ts1 ->
                 (Just (map (uncurry Attr.new) attrs), ts1)
            _ -> (Nothing, ts0)
      docType <- withoutLeadingSpace $ \ts0 ->
         case ts0 of
            (_, Tag.TagOpen "!DOCTYPE" dtd):ts1 ->
                 (Just (Attr.formatListBlankHead
                         (map (Attr.fromPair :: (String,String) -> Attr.T NameMC.T String) dtd) ""), ts1)
            _ -> (Nothing, ts0)
      gets (XmlDoc.Cons xml docType . TreeTagSoup.toXmlTreesAux)

{-
toXmlDocument =
   Tag.canonicalizeTags .
   XmlDoc.Cons Nothing .
   TreeTagSoup.toXmlTreesAux
-}

toXmlDocumentString ::
   (Name.Tag name, Name.Attribute name) =>
   [Tag String] -> XmlDoc.T Position.T name String
toXmlDocumentString =
   toXmlDocument
{- this would only work for String, because of isSpace
   let cts = Tag.canonicalizePosTags ts
   in  case dropWhile (Match.tagText (all isSpace) . snd) cts of
          (_, Tag.TagSpecial "DOCTYPE" dtd):rest ->
               XmlDoc.Cons (Just dtd) (TreeTagSoup.toXmlTreesAux rest)
          _ -> XmlDoc.Cons Nothing    (TreeTagSoup.toXmlTreesAux cts)
-}


example :: IO ()
example =
  print .
  (toXmlDocumentString :: [Tag String] -> XmlDoc.T Position.T NameLC.T String) .
  Tag.parseTagsOptions TreeTagSoup.parseOptions
    =<< readFile "/home/thielema/public_html/index.html"
