module Text.HTML.WraXML.Table where

-- import qualified Text.HTML.WraXML.Tree as HtmlTree
import qualified Text.XML.WraXML.Tree    as XmlTree
import qualified Text.XML.WraXML.Element as Elem

import qualified Text.XML.Basic.Name as Name

import           Data.Maybe(mapMaybe)


type T i name str = [[[XmlTree.T i name str]]]

{- |
Ignores all non-table-row-tags and all non-table-cell-tags.
-}
simpleFromRows ::
   (Name.Tag name) =>
   [XmlTree.T i name str] ->  T i name str
simpleFromRows =
   map (simpleFromRow . snd) .
   filter (Elem.checkName (Name.match "tr") . fst) .
   mapMaybe XmlTree.maybeTag

simpleFromRow ::
   (Name.Tag name) =>
   [XmlTree.T i name str] ->  [[XmlTree.T i name str]]
simpleFromRow =
   map snd .
   filter (Elem.checkName (Name.matchAny ["th","td"]) . fst) .
   mapMaybe XmlTree.maybeTag


{-
data CellType = Heading | Cell

fromRows :: [XmlTree.T i name str] ->  [[(CellType, [XmlTree.T i name str])]]
fromRows =
   map fromRow .
   filter (XmlTree.checkTagName ("tr"==) . fst) .
   mapMaybe XmlTree.maybeTag

fromRow :: [XmlTree.T i name str] ->  [(CellType, [XmlTree.T i name str])]
fromRow =
   mapMaybe (\tag ->
      if XmlTree.checkTagName ("td"==) tag
        then Just) .
   mapMaybe XmlTree.maybeTag
-}
