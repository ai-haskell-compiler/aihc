{-# LANGUAGE Arrows #-}

-- | Parsing for the metadata section of the OPF Package XML Document
module Codec.Epub.Parse.Metadata
   ( metadataP
   )
   where

import Control.Applicative ( WrappedArrow (WrapArrow), unwrapArrow )
import Control.Arrow.ListArrows ( (>>.), (>>>), (<<<), catA, constA, listA, returnA )
import Data.List ( isPrefixOf )
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes )
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow ( ArrowXml, hasAttrValue )
import Text.XML.HXT.DOM.TypeDefs ( XNode )

import Codec.Epub.Data.Metadata
import Codec.Epub.Parse.Util


idP :: (ArrowXml a) => a (NTree XNode) Identifier
idP = atQTag (dcName "identifier") >>> ( unwrapArrow $ Identifier
   <$> (WrapArrow $ mbGetAttrValue "id")
   <*> (WrapArrow $ constA Nothing)
   <*> (WrapArrow $ mbGetQAttrValue (opfName "scheme"))  -- An attr in epub2
   <*> (WrapArrow $ text)
   )


titleP :: (ArrowXml a) => a (NTree XNode) (String, Title)
titleP = atQTag (dcName "title") >>>
   proc x -> do
      i <- mbGetAttrValue "id" -< x
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< ((maybe "" id i), Title l Nothing Nothing c)


langP :: (ArrowXml a) => a (NTree XNode) String
langP = atQTag (dcName "language") >>> text


{- Since creators and contributors have the same exact XML structure,
   this arrow is used to get either of them
-}
creatorP :: (ArrowXml a) => String -> a (NTree XNode) (String, Creator)
creatorP tag = atQTag (dcName tag) >>>
   proc x -> do
      i <- mbGetAttrValue "id" -< x
      r <- mbGetQAttrValue (opfName "role") -< x
      f <- mbGetQAttrValue (opfName "file-as") -< x
      t <- text -< x
      returnA -< ((maybe "" id i), Creator r f Nothing t)


dateElemP :: (ArrowXml a) => a (NTree XNode) (Maybe (DateEvent, DateValue))
dateElemP = atQTag (dcName "date") >>>
   proc x -> do
      e <- mbGetQAttrValue (opfName "event") -< x
      c <- text -< x
      returnA -< (, DateValue c) <$> dateEventFromString e


dateMetaP :: (ArrowXml a) => a (NTree XNode) (Maybe (DateEvent, DateValue))
dateMetaP = atQTag (opfName "meta") >>>
   proc x -> do
      e <- mbGetAttrValue "property" <<< hasAttrValue "property" (isPrefixOf "dcterms:") -< x
      c <- text -< x
      returnA -< (, DateValue c) <$> dateEventFromString e


sourceP :: (ArrowXml a) => a (NTree XNode) (String, Source)
sourceP = atQTag (dcName "source") >>>
  proc x -> do
    i <- mbGetAttrValue "id" -< x
    t <- text -< x
    returnA -< ((maybe "" id i), Source Nothing Nothing Nothing t)


typeP :: (ArrowXml a) => a (NTree XNode) (Maybe String)
typeP = mbQTagText $ dcName "type"


coverageP :: (ArrowXml a) => a (NTree XNode) String
coverageP = atQTag (dcName "coverage") >>> text


descriptionP :: (ArrowXml a) => a (NTree XNode) Description
descriptionP = atQTag (dcName "description") >>>
   proc x -> do
      l <- mbGetQAttrValue (xmlName "lang") -< x
      c <- text -< x
      returnA -< Description l c


formatP :: (ArrowXml a) => a (NTree XNode) String
formatP = atQTag (dcName "format") >>> text


publisherP :: (ArrowXml a) => a (NTree XNode) String
publisherP = atQTag (dcName "publisher") >>> text


relationP :: (ArrowXml a) => a (NTree XNode) String
relationP = atQTag (dcName "relation") >>> text


rightsP :: (ArrowXml a) => a (NTree XNode) String
rightsP = atQTag (dcName "rights") >>> text


subjectP :: (ArrowXml a) => a (NTree XNode) String
subjectP = atQTag (dcName "subject") >>> text


metadataP :: (ArrowXml a) => [Refinement] -> a (NTree XNode) Metadata
metadataP refinements =
   atQTag (opfName "metadata") >>> ( unwrapArrow $ Metadata
      <$> (WrapArrow $ listA $ idP >>. map (refineIdentifier refinements))
      <*> (WrapArrow $ listA $ titleP >>. map (refineTitle refinements))
      <*> (WrapArrow $ listA langP)
      <*> (WrapArrow $ listA $ creatorP "contributor" >>.
         map (refineCreator refinements))
      <*> (WrapArrow $ listA $ creatorP "creator" >>.
         map (refineCreator refinements))
      <*> (Map.fromList . catMaybes <$> (WrapArrow $ listA $ catA [dateElemP, dateMetaP]))
      <*> (WrapArrow $ listA $ sourceP >>. map (refineSource refinements))
      <*> (WrapArrow typeP)
      <*> (WrapArrow $ listA coverageP)
      <*> (WrapArrow $ listA descriptionP)
      <*> (WrapArrow $ listA formatP)
      <*> (WrapArrow $ listA publisherP)
      <*> (WrapArrow $ listA relationP)
      <*> (WrapArrow $ listA rightsP)
      <*> (WrapArrow $ listA subjectP)
      )
