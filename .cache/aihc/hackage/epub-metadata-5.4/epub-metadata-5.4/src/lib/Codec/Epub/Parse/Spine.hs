{-# LANGUAGE Arrows #-}

-- | Parsing for the spine section of the OPF Package XML Document
module Codec.Epub.Parse.Spine
   ( spineP
   )
   where

import Control.Arrow.ListArrows ( (>>>), listA, returnA )
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow ( ArrowXml, getAttrValue )
import Text.XML.HXT.DOM.TypeDefs ( XNode )

import Codec.Epub.Data.Spine
import Codec.Epub.Parse.Util


spineItemrefP :: (ArrowXml a) => a (NTree XNode) SpineItemref
spineItemrefP = atQTag (opfName "itemref") >>>
   proc x -> do
      i <- getAttrValue "idref" -< x
      ml <- mbGetAttrValue "linear" -< x
      let l = maybe Nothing (\v -> if v == "no" then Just False else Just True) ml 
      returnA -< SpineItemref i l


spineP :: (ArrowXml a) => a (NTree XNode) Spine
spineP = atQTag (opfName "spine") >>>
   proc x -> do
      i <- getAttrValue "toc" -< x
      l <- listA spineItemrefP -< x
      returnA -< (Spine i l)
