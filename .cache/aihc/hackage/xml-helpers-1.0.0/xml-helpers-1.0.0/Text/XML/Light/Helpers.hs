-- |A set of useful helper functions for dealing with XML data.
module Text.XML.Light.Helpers
 where

import Control.Monad
import Data.Maybe
import Text.XML.Light

-- |Map the given function over the children of the given element with the
-- given name.
mapChildren :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapChildren s e f = sequence $ map f $ findChildren (unqual s) e

-- |Fold the function over the children of the given element with the given
-- name.
foldChildren :: String -> Element -> a -> (a -> Element -> Maybe a) -> Maybe a
foldChildren s e b f = foldM f b $ findChildren (unqual s) e

-- |Map the given function over all subelements of the given element with
-- the given name.
mapElements :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapElements s e f = sequence $ map f $ findElements (unqual s) e

-- |Fold the given function over the children of the given element with
-- the given name.
foldElements :: String -> Element -> a -> (a -> Element -> Maybe a) -> Maybe a
foldElements s e b f = foldM f b $ findElements (unqual s) e

--

-- |Map the given function over the children of the given element that
-- have an attribute "name" matching the given string.
mapChildrenWithAttName :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapChildrenWithAttName s e f = mapM f $ findChildrenWithAttName s e

-- |Map the given function over the subelements of the given element that
-- have an attribute "name" matching the given string.
mapElementsWithAttName :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapElementsWithAttName s e f = mapM f $ findElementsWithAttName s e

-- |Fold the given function over the children of the given element that
-- have an attribute "name" matching the given string.
foldChildrenWithAttName :: String -> Element -> a ->
                           (a -> Element -> Maybe a) ->
                           Maybe a
foldChildrenWithAttName s e b f = foldM f b $ findChildrenWithAttName s e

-- |Fold the given function over the subelements of the given element that
-- have an attribute "name" matching the given string.
foldElementsWithAttName :: String -> Element -> a ->
                           (a -> Element -> Maybe a) ->
                           Maybe a
foldElementsWithAttName s e b f = foldM f b $ findElementsWithAttName s e


--

-- |Get the string contents of the child of the given element with the given
-- name.
getChildData :: String -> Element -> Maybe String
getChildData s x = strContent `fmap` findChild (unqual s) x

-- |Get the string contents of the subelement of the given element with the
-- given name.
getElementData :: String -> Element -> Maybe String
getElementData s x = strContent `fmap` findElement (unqual s) x

--

-- |Find a child of the given element with that has an attribute "name"
-- equal to the given string.
findChildWithAttName :: String -> Element -> Maybe Element
findChildWithAttName s = listToMaybe . findChildrenWithAttName s

-- |Find all the children of the given element that have an attribute "name"
-- equal to the given string.
findChildrenWithAttName :: String -> Element -> [Element]
findChildrenWithAttName s = filterChildren (elementHasNameAttr s)

-- |Find a subelement of the given element that has an attribute "name"
-- equal to the given string.
findElementWithAttName :: String -> Element -> Maybe Element
findElementWithAttName  s = listToMaybe . findElementsWithAttName s

-- |Find all the subelements of the given element that have an attribute
-- "name" equal to the given string.
findElementsWithAttName :: String -> Element -> [Element]
findElementsWithAttName s = filterElements (elementHasNameAttr s)

-- |Returns True iff the given alement has an attribute "name" equal to
-- the given string.
elementHasNameAttr :: String -> Element -> Bool
elementHasNameAttr s e =
  case findAttr (unqual "name") e of
    Nothing -> False
    Just v  -> s == v

-- |Convert a list of rows (subelement with the name "row") into a Haskell
-- datatype using the given function.s 
parseRows :: (Element -> Maybe a) -> Element -> Maybe [a]
parseRows f xml = sequence $ map f $ findElements (unqual "row") xml

