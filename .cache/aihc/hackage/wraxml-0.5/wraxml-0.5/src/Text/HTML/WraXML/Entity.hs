module Text.HTML.WraXML.Entity
   (Name, HtmlEnt.mapNameToChar, mapCharToName, ) where

import qualified Text.HTML.Basic.Entity as HtmlEnt
import qualified Data.Map as Map

import Data.Tuple.HT (swap, )


type Name = String

mapCharToName :: Map.Map Char Name
mapCharToName =
   Map.fromList $ map swap HtmlEnt.listInternetExploder
