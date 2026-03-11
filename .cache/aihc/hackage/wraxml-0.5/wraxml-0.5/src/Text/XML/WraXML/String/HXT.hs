module Text.XML.WraXML.String.HXT where

import qualified Text.XML.HXT.DOM.TypeDefs as HXT
import qualified Data.Tree.NTree.TypeDefs  as HXTTree

import qualified Text.XML.WraXML.String  as XmlString
import qualified Text.XML.Basic.Character as XmlChar


toXmlString :: HXT.XmlTrees -> XmlString.T
toXmlString = concatMap toXmlString'

toXmlString' :: HXT.XmlTree -> XmlString.T
toXmlString' (HXTTree.NTree label subTrees) =
   if null subTrees
     then
        case label of
           HXT.XText str -> XmlString.fromString str
           HXT.XCharRef ref -> [XmlChar.fromCharRef ref]
           HXT.XEntityRef ref -> [XmlChar.fromEntityRef ref]
           _ -> error "HXT: illegal part of HXT string"
     else error "HXT: parts of a string must not contain sub-strings"


fromXmlString :: XmlString.T -> HXT.XmlTrees
fromXmlString =
   map (flip HXTTree.NTree []) .
   XmlChar.switchUnicodeRuns
      HXT.XText
      HXT.XCharRef
      HXT.XEntityRef
