{-# LANGUAGE Arrows #-}

-- | Parsing for the manifest section of the OPF Package XML Document
module Codec.Epub.Parse.Manifest
   ( manifestP
   )
   where

import Control.Arrow.ListArrows ( (>>>), listA, returnA )
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow ( ArrowXml, getAttrValue )
import Text.XML.HXT.DOM.TypeDefs ( XNode )

import Codec.Epub.Data.Manifest
import Codec.Epub.Parse.Util


manifestItemP :: (ArrowXml a) => a (NTree XNode) ManifestItem
manifestItemP= atQTag (opfName "item") >>>
   proc x -> do
      i <- getAttrValue "id" -< x
      h <- getAttrValue "href" -< x
      m <- getAttrValue "media-type" -< x
      returnA -< ManifestItem i h m


manifestP :: (ArrowXml a) => a (NTree XNode) Manifest
manifestP = atQTag (opfName "manifest") >>>
   proc x -> do     
      l <- listA manifestItemP -< x
      returnA -< Manifest l
