module Text.RSS.Utils where

import Prelude.Compat

import Data.Text
import Data.XML.Types as XML
import Text.XML as C

createContent :: Text -> XML.Node
createContent = XML.NodeContent . ContentText

createQName :: Text -> Name
createQName txt = XML.Name {nameLocalName = txt, nameNamespace = Nothing, namePrefix = Nothing}

type Attr = (Name, [Content])

mkNAttr :: Name -> Text -> Attr
mkNAttr k v = (k, [ContentText v])

elementToDoc :: XML.Element -> Maybe C.Document
elementToDoc el =
  either (const Nothing) Just $ fromXMLDocument $ XML.Document (Prologue [] Nothing []) el []
