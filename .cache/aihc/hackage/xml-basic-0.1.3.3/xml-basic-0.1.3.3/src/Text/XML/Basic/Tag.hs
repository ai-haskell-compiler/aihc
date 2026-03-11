{- |
We do not define a tag data type here,
since this is too much bound to the particular use
(e.g. list or tree structure).
However we define a tag name and some special names.
-}
module Text.XML.Basic.Tag (
   Name(..),
   doctype, doctypeName, doctypeString,
   cdata, cdataName, cdataString,
   xmlName, xmlString,
   maybeXMLEncoding,
   ) where

import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.ProcessingInstruction as PI
import qualified Text.XML.Basic.Attribute as Attr
import Control.Monad (guard, )


newtype Name ident = Name {unname :: ident}
   deriving (Eq, Ord)

instance Show ident => Show (Name ident) where
   showsPrec p = showsPrec p . unname

instance Name.Tag ident => Name.C (Name ident) where
   fromString = Name . Name.tagFromString
   toString = Name.tagToString . unname




{-# DEPRECATED doctype "use doctypeName instead" #-}
doctype :: (Name.Tag name) => Name name
doctype = doctypeName

{-# DEPRECATED cdata "use cdataName instead" #-}
cdata :: (Name.Tag name) => Name name
cdata = cdataName


doctypeName :: (Name.Tag name) => Name name
doctypeName = Name.fromString doctypeString

cdataName :: (Name.Tag name) => Name name
cdataName = Name.fromString cdataString

xmlName :: (Name.Tag name) => Name name
xmlName = Name.fromString xmlString


doctypeString :: String
doctypeString = "DOCTYPE"

cdataString :: String
cdataString = "[CDATA["

xmlString :: String
xmlString = "xml"


maybeXMLEncoding ::
   (Name.Tag name, Name.Attribute name) =>
   Name name -> PI.T name string -> Maybe string
maybeXMLEncoding name instr =
   do guard (xmlName == name)
      let (PI.Known attrs) = instr
      Attr.lookup Attr.encodingName attrs
