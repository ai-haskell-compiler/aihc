{-# LANGUAGE Arrows #-}

{- | Parsing for meta tags in the metadata section of the OPF
   Package XML Document
-}
module Codec.Epub.Parse.Refinements
   ( refinementsP
   )
   where

import Control.Applicative (WrappedArrow (WrapArrow), unwrapArrow)
import Control.Arrow.ListArrows ((>>>), (>>^), listA)
import Data.Tree.NTree.TypeDefs ( NTree )
import Text.XML.HXT.Arrow.XmlArrow (ArrowXml)
import Text.XML.HXT.DOM.TypeDefs (XNode)

import Codec.Epub.Data.Metadata (Refinement (..))
import Codec.Epub.Parse.Util


removeHash :: String -> String
removeHash ('#' : cs) = cs
removeHash s          = s


refinementP :: (ArrowXml a) => a (NTree XNode) Refinement
refinementP = atQTag (opfName "meta") >>> ( unwrapArrow $ Refinement
   <$> (WrapArrow $ mbGetAttrValue "refines" >>^ maybe "" removeHash)
   <*> (WrapArrow $ mbGetAttrValue "property" >>^ maybe "" id)
   <*> (WrapArrow $ mbGetAttrValue "scheme" >>^ maybe "" id)
   <*> (WrapArrow $ text)
   )


refinementsP :: (ArrowXml a) => a (NTree XNode) [Refinement]
refinementsP = atQTag (opfName "metadata") >>> listA refinementP
