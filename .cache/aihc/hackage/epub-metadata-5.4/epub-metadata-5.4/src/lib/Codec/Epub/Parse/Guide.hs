{-# LANGUAGE Arrows #-}

-- | Parsing for the guide section of the OPF Package XML Document
module Codec.Epub.Parse.Guide
   ( guideP
   )
   where

import Control.Arrow.ListArrows ( (>>>), constA, listA, orElse, returnA )
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow ( ArrowXml, getAttrValue )
import Text.XML.HXT.DOM.TypeDefs ( XNode )

import Codec.Epub.Data.Guide
import Codec.Epub.Parse.Util


guideRefP :: (ArrowXml a) => a (NTree XNode) GuideRef
guideRefP = atQTag (opfName "reference") >>>
   proc x -> do
      t <- getAttrValue "type" -< x
      mt <- mbGetAttrValue "title" -< x
      h <- getAttrValue "href" -< x
      returnA -< GuideRef t mt h


guideP :: (ArrowXml a) => a (NTree XNode) [GuideRef]
guideP = (atQTag (opfName "guide") >>> listA guideRefP)
   `orElse` constA []
