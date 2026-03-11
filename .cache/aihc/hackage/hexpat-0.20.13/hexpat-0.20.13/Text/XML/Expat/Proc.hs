{-# LANGUAGE FlexibleContexts #-}
-- | This module ported from Text.XML.Light.Proc
module Text.XML.Expat.Proc where

import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.SAX

import Control.Monad
import Data.List.Class (filter)
import Data.Maybe(listToMaybe)
import Data.Monoid
import Prelude hiding (filter)


-- | Select only the elements from a list of XML content.
onlyElems          :: NodeClass n c => c (n c tag text) -> c (n c tag text)
onlyElems           = filter isElement

-- | Select only the text from a list of XML content.
onlyText           :: (NodeClass n c, Monoid text) => c (n c tag text) -> c text
onlyText            = fmap getText . filter isText

-- | Find all immediate children with the given name.
findChildren       :: (NodeClass n c, Eq tag, Monoid tag) => tag -> n c tag text -> c (n c tag text)
findChildren q e    = filterChildren ((q ==) . getName) e

-- | Filter all immediate children wrt a given predicate.
filterChildren       :: NodeClass n c => (n c tag text -> Bool) -> n c tag text -> c (n c tag text)
filterChildren p e | isElement e = filter p (onlyElems (getChildren e))
filterChildren _ _               = mzero

-- | Filter all immediate children wrt a given predicate over their names.
filterChildrenName      :: (NodeClass n c, Monoid tag) => (tag -> Bool) -> n c tag text -> c (n c tag text)
filterChildrenName p e | isElement e = filter (p . getName) (onlyElems (getChildren e))
filterChildrenName _ _               = mzero

-- | Find an immediate child with the given name.
findChild          :: (NodeClass n [], GenericXMLString tag) => tag -> n [] tag text -> Maybe (n [] tag text)
findChild q e       = listToMaybe (findChildren q e)

-- | Find an immediate child with the given name.
filterChild          :: NodeClass n [] => (n [] tag text -> Bool) -> n [] tag text -> Maybe (n [] tag text)
filterChild p e       = listToMaybe (filterChildren p e)

-- | Find an immediate child with name matching a predicate.
filterChildName      :: (NodeClass n [], Monoid tag) => (tag -> Bool) -> n [] tag text -> Maybe (n [] tag text)
filterChildName p e   = listToMaybe (filterChildrenName p e)

-- | Find the left-most occurrence of an element matching given name.
findElement        :: (NodeClass n [], Eq tag, Monoid tag) => tag -> n [] tag text -> Maybe (n [] tag text)
findElement q e     = listToMaybe (findElements q e)

-- | Filter the left-most occurrence of an element wrt. given predicate.
filterElement        :: NodeClass n [] => (n [] tag text -> Bool) -> n [] tag text -> Maybe (n [] tag text)
filterElement p e     = listToMaybe (filterElements p e)

-- | Filter the left-most occurrence of an element wrt. given predicate.
filterElementName     :: (NodeClass n [], Monoid tag) => (tag -> Bool) -> n [] tag text -> Maybe (n [] tag text)
filterElementName p e  = listToMaybe (filterElementsName p e)

-- | Find all non-nested occurances of an element.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
findElements       :: (NodeClass n c, Eq tag, Monoid tag) => tag -> n c tag text -> c (n c tag text)
findElements qn e = filterElementsName (qn==) e

-- | Find all non-nested occurrences of an element wrt. given predicate.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
filterElements       :: NodeClass n c => (n c tag text -> Bool) -> n c tag text -> c (n c tag text)
filterElements p e
    | p e         = return e
    | isElement e = join $ fmap (filterElements p) $ onlyElems $ getChildren e
    | otherwise   = mzero

-- | Find all non-nested occurences of an element wrt a predicate over element names.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
filterElementsName       :: (NodeClass n c, Monoid tag) => (tag -> Bool) -> n c tag text -> c (n c tag text)
filterElementsName p e | isElement e = filterElements (p . getName) e
filterElementsName _ _               = mzero

