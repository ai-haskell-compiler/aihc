{- |
We do not define a tag data type here,
since this is too much bound to the particular use
(e.g. list or tree structure).
However we define a tag name and several
-}
module Text.HTML.Basic.Tag (
   Tag.Name(..),
   Tag.doctype, Tag.doctypeName, Tag.doctypeString,
   Tag.cdata,   Tag.cdataName,   Tag.cdataString,
   isEmpty, isSloppy, isInnerOf, closes,
   maybeMetaHTTPHeader, maybeMetaEncoding, maybeMetaCharset,
   encodingFromContentType,
   ) where


import Text.XML.Basic.Tag (Name, )

import qualified Text.XML.Basic.Tag as Tag
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.List.Reverse.StrictElement as ListRev
import qualified Data.Char as Char
import           Data.Tuple.HT (mapFst, )
import           Control.Monad (guard, liftM2, )


{- |
Check whether a HTML tag is empty.
-}
isEmpty :: (Name.Tag name) =>
   Name name -> Bool
isEmpty = flip Set.member emptySet

{- |
Set of empty HTML tags.
-}
emptySet :: (Name.Tag name) =>
   Set.Set (Name name)
emptySet =
   nameSet $
   "area" :
   "base" :
   "br" :
   "col" :
   "frame" :
   "hr" :
   "img" :
   "input" :
   "link" :
   "meta" :
   "param" :
   []


{- |
Some tags, namely those for text styles like FONT, B, I,
are used quite sloppily.
That is, they are not terminated or not terminated in the right order.
We close them implicitly, if another tag closes
and ignore non-matching closing tags.
-}
isSloppy ::  (Name.Tag name) =>
   Name name -> Bool
isSloppy = flip Set.member sloppySet

{-
Example page:
http://extremetracking.com/open;unique?login=crsucks
-}

sloppySet :: (Name.Tag name) =>
   Set.Set (Name name)
sloppySet =
   nameSet $
   "font" :
   "b" :
   "i" :
   "tt" :
   "u" :
   "strike" :
   "s" :
   "big" :
   "small" :
   []


isInnerOf :: (Name.Tag name) =>
   Name name -> Name name -> Bool
isInnerOf outer inner =
   maybe False (Set.member inner) $
   Map.lookup outer innerMap


innerMap :: (Name.Tag name) =>
   Map.Map (Name name) (Set.Set (Name name))
innerMap =
   nameMap $
   ("body",    pSet) :
   ("caption", pSet) :
   ("dd",      pSet) :
   ("div",     pSet) :
   ("dl",      dtdSet) :
   ("dt",      pSet) :
   ("li",      pSet) :
   ("map",     pSet) :
   ("object",  pSet) :
   ("ol",      liSet) :
   ("table",   nameSet ["th","tr","td","thead","tfoot","tbody"]) :
   ("tbody",   thdrSet) :
   ("td",      pSet) :
   ("tfoot",   thdrSet) :
   ("th",      pSet) :
   ("thead",   thdrSet) :
   ("tr",      thdSet) :
   ("ul",      liSet) :
   []


closes :: (Name.Tag name) =>
   Name name -> Name name -> Bool
closes closing opening =
   (not (Name.match "option" closing) && Name.match "select" opening) ||
   (Name.matchAny ["option", "script", "style","textarea","title"] opening) ||
   (maybe False (Set.member opening) $
    Map.lookup closing closesMap)


closesMap :: (Name.Tag name) =>
   Map.Map (Name name) (Set.Set (Name name))
closesMap =
   nameMap $
   ("a"        , nameSingle "a") :
   ("li"       , liSet) :
   ("th"       , thdSet) :
   ("td"       , thdSet) :
   ("tr"       , thdrSet) :
   ("dt"       , dtdSet) :
   ("dd"       , dtdSet) :
   ("hr"       , pSet) :
   ("colgroup" , nameSingle "colgroup") :
   ("form"     , nameSingle "form") :
   ("label"    , nameSingle "label") :
   ("map"      , nameSingle "map") :
   ("object"   , nameSingle "object") :
   ("thead"    , nameSet ["colgroup"]) :
   ("tfoot"    , nameSet ["thead", "colgroup"]) :
   ("tbody"    , nameSet ["tbody", "tfoot", "thead", "colgroup"]) :
   ("h1"       , headingSet) :
   ("h2"       , headingSet) :
   ("h3"       , headingSet) :
   ("h4"       , headingSet) :
   ("h5"       , headingSet) :
   ("h6"       , headingSet) :
   ("dl"       , headingSet) :
   ("ol"       , headingSet) :
   ("ul"       , headingSet) :
   ("table"    , headingSet) :
   ("div"      , headingSet) :
   ("p"        , headingSet) :
   []


nameMap :: (Name.Tag name) => [(String,a)] -> Map.Map (Name name) a
nameMap = Map.fromList . map (mapFst Name.fromString)

nameSet :: (Name.Tag name) => [String] -> Set.Set (Name name)
nameSet = Set.fromList . map Name.fromString

nameSingle :: (Name.Tag name) => String -> Set.Set (Name name)
nameSingle = Set.singleton . Name.fromString

pSet, dtdSet, thdSet, thdrSet, liSet, headingSet ::
   (Name.Tag name) => Set.Set (Name name)
pSet       = nameSet ["p"]
dtdSet     = nameSet ["dt","dd"]
thdSet     = nameSet ["th","td"]
thdrSet    = nameSet ["th","td","tr"]
liSet      = nameSet ["li"]
headingSet = nameSet ["h1","h2","h3","h4","h5","h6","p" {- not "div" -}]




maybeMetaHTTPHeader ::
   (Name.Tag name, Name.Attribute name) =>
   Tag.Name name -> [Attr.T name string] -> Maybe (string, string)
maybeMetaHTTPHeader name attrs =
   do guard (Name.match "meta" name)
      liftM2 (,)
         (Attr.lookupLit "http-equiv" attrs)
         (Attr.lookupLit "content" attrs)


{- |
Extract charset from Content-Type declaration.

> encodingFromContentType "text/html; charset=UTF-8" == "utf-8"

The routine does not perform any syntax check.
-}
encodingFromContentType :: String -> String
encodingFromContentType = map Char.toLower . ListRev.takeWhile ('='/=)

{- |
A simple routine that does not check for valid syntax
of the Content-Type specification.

In future we might use a distinct @Encoding@ type instead of plain String.
-}
maybeMetaEncoding ::
   (Name.Tag name, Name.Attribute name) =>
   Tag.Name name -> [Attr.T name String] -> Maybe String
maybeMetaEncoding name attrs =
   do (headerName, content) <- maybeMetaHTTPHeader name attrs
      guard (("content-type"==) . map Char.toLower $ headerName)
      return $ encodingFromContentType content

maybeMetaCharset ::
   (Name.Tag name, Name.Attribute name) =>
   Tag.Name name -> [Attr.T name string] -> Maybe string
maybeMetaCharset name attrs =
   do guard (Name.match "meta" name)
      Attr.lookupLit "charset" attrs
