{-# LANGUAGE Arrows #-}

-- | Parsing for the package section of the OPF Package XML Document
module Codec.Epub.Parse.Package
   ( packageP
   )
   where

import Control.Arrow.ListArrows ( (>>>), returnA )
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow ( ArrowXml, getAttrValue )
import Text.XML.HXT.DOM.TypeDefs ( XNode )

import Codec.Epub.Data.Package
import Codec.Epub.Parse.Util


packageP :: (ArrowXml a) => a (NTree XNode) Package
packageP = atQTag (opfName "package") >>>
   proc x -> do
      v <- getAttrValue "version" -< x
      u <- getAttrValue "unique-identifier" -< x
      returnA -< (Package v u)
