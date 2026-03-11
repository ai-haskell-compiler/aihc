{- |
Module      : Text.XML.HXT.CSS

Stability   : provisional

Turn a CSS selector into an HXT arrow.
-}

{-# LANGUAGE FlexibleInstances #-}

module Text.XML.HXT.CSS
    ( css
    , cssShallow
    , cssNav
    , cssShallowNav
    , Css

    -- * Supported selectors
    -- $supported_selectors

    -- * Example
    -- $example
    ) 
    where

import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Data.Tree.NavigatableTree.Class
import qualified Data.Tree.NavigatableTree.XPathAxis as T
import Text.XML.HXT.DTDValidation.TypeDefs
import Data.Tree.NTree.Zipper.TypeDefs

import Text.XML.HXT.CSS.TypeDefs
import Text.XML.HXT.CSS.Parser

-- | Select elements from an HTML document with a CSS selector. 
css :: (ArrowXml a, Css s) => s -> a XmlTree XmlTree
css = withNav . cssNav

-- | Like 'css', except that the selector is anchored at the top. For
-- example, @'cssShallow' \"div\"@ will only select @div@ elements that are
-- in the input of the arrow, it will not recursively search for @div@s
-- contained deeper in the document tree. The latter can be selected by
-- @'cssShallow' \"* div\"@ but is recommended to use 'css' for that. In
-- other words, @'cssShallow' \"div\"@ corresponds to the @\"\/div\"@ XPath
-- expression, whereas @'cssShallow' \"* div\"@ corresponds to @\"\/\/div\"@.
cssShallow :: (ArrowXml a, Css s) => s -> a XmlTree XmlTree
cssShallow = withNav . cssShallowNav

-- | Like 'css', except that it operates on navigatable XML trees.
cssNav :: (ArrowXml a, Css s) => s -> a XmlNavTree XmlNavTree
cssNav s = isElemN >>> skipXmlRoot >>> selectDeep s

-- | Like 'cssShallow', except that it operates on navigatable XML trees.
cssShallowNav :: (ArrowXml a, Css s) => s -> a XmlNavTree XmlNavTree
cssShallowNav s = isElemN >>> skipXmlRoot >>> select s

-- | Things that can be used as a CSS selector. The 'String' instance
-- uses 'safeParseCSS' to parse the string.
class Css s where
    selectDeep :: ArrowXml a => s -> a XmlNavTree XmlNavTree
    select :: ArrowXml a => s -> a XmlNavTree XmlNavTree

    selectDeep s = multi (isElemN >>> select s)

instance Css [Char] where
    selectDeep s =
        case safeParseCSS s of
            Right sel -> selectDeep sel
            Left msg -> constA $ XN.mkError c_err msg

    select s =
        case safeParseCSS s of
            Right sel -> select sel
            Left msg -> constA $ XN.mkError c_err msg

instance Css SelectorsGroup where
    select (SelectorsGroup sels) =
        foldr ((<+>) . select) zeroArrow sels

instance Css Selector where
    select (Selector sss) = select sss
    select (Descendant sss sel) =
        select sss >>> getChildren >>> isElemN >>>
            multi (isElemN >>> select sel)
    select (Child sss sel) =
        select sss >>> getChildren >>> isElemN >>> select sel
    select (AdjSibling sss sel) =
        select sss >>> nextSibling >>> select sel
    select (FolSibling sss sel) =
        select sss >>> followingSiblingAxis >>> select sel

instance Css SimpleSelectorSeq where
    select (SimpleSelectorSeq simpSels) =
        foldr ((>>>) . select) this simpSels

instance Css SimpleSelector where
    select UniversalSelector = this
    select (TypeSelector tagName) = withoutNav $ hasName tagName
    select (IdSelector nodeId) =
        withoutNav $ hasAttrValue "id" (== nodeId)
    select (ClassSelector className) =
        withoutNav $ hasAttrValue "class" (hasWord className)
    select (AttrSelector attrb sel) =
        withoutNav $ hasAttrValue attrb p
      where
        p = case sel of
                AttrExists -> const True
                AttrEq val -> (== val)
                AttrContainsSp val -> hasWord val
                AttrBeginHy val -> hypenPrefix val
                AttrPrefix val -> isPrefixOf val
                AttrSuffix val -> isSuffixOf val
                AttrSubstr val -> isInfixOf val
        hypenPrefix s1 s2 =
            case wordsBy (== '-') s2 of
                w : _ | s1 == w -> True
                _ -> False
    select (Pseudo pseudo) = select pseudo
    select (PseudoNth pseudo) = select pseudo
    select (Negation simple) = neg (select simple)

instance Css PseudoClass where
    select PseudoFirstChild = nthChild (== 1)
    select PseudoLastChild  = nthLastChild (== 1)
    select PseudoOnlyChild =
        nthChild (== 1) >>> nthLastChild (== 1)
    select PseudoFirstOfType = nthOfType (== 1)
    select PseudoLastOfType = nthLastOfType (== 1)
    select PseudoOnlyOfType =
        nthOfType (== 1) >>> nthLastOfType (== 1)
    select PseudoEmpty = neg notEmpty
      where
        notEmpty = filterA $ getChildren >>>
            withoutNav (isElem <+> isText <+> isCdata <+> isEntityRef)
    select PseudoRoot = filterA (moveUp' >>> isRootN)

instance Css PseudoNthClass where
    select (PseudoNthChild nth) = nthChild (testNth nth)
    select (PseudoNthLastChild nth) = nthLastChild (testNth nth)
    select (PseudoNthOfType nth) = nthOfType (testNth nth)
    select (PseudoNthLastOfType nth) = nthLastOfType (testNth nth)

--------------------------------------------------------------------------------

-- avoid the ArrowNavigatableTree constraint
moveUp' :: (ArrowList a, NavigatableTree t) => a (t a1) (t a1)
moveUp' = arrL $ maybeToList . mvUp

skipXmlRoot :: ArrowXml a => a XmlNavTree XmlNavTree
skipXmlRoot = ifA isRootN (getChildren >>> isElemN) this

hasParent :: ArrowXml a => a XmlNavTree XmlNavTree
hasParent = filterA $ moveUp' >>> neg isRootN

isRootN :: ArrowXml a => a XmlNavTree XmlNavTree
isRootN = withoutNav isRoot

nextSibling :: ArrowList a => a XmlNavTree XmlNavTree
nextSibling = arrL go
  where
    go x =
        case mvRight x of
            Just x'
                | isElemNodeN x' -> [x']
                | otherwise -> go x'
            Nothing -> []

nthChild :: ArrowXml a => (Int -> Bool) -> a XmlNavTree XmlNavTree
nthChild p = arrL (nthElemFun T.precedingSiblingAxis p) >>> hasParent

nthLastChild :: ArrowXml a => (Int -> Bool) -> a XmlNavTree XmlNavTree
nthLastChild p = arrL (nthElemFun T.followingSiblingAxis p) >>> hasParent

nthOfType :: ArrowXml a => (Int -> Bool) -> a XmlNavTree XmlNavTree
nthOfType p = arrL (nthElemOfTypeFun T.precedingSiblingAxis p) >>> hasParent

nthLastOfType :: ArrowXml a => (Int -> Bool) -> a XmlNavTree XmlNavTree
nthLastOfType p = arrL (nthElemOfTypeFun T.followingSiblingAxis p) >>> hasParent

nthElemOfTypeFun
    :: (XmlNavTree -> [XmlNavTree])
    -> (Int -> Bool) -> XmlNavTree -> [XmlNavTree]
nthElemOfTypeFun axis p x = nthElemFun axis' p x
  where
    axis' = filter ((== xNm) . getNm) . axis
    xNm = getNm x
    getNm = XN.getQualifiedName . (\(XN.NTree n _) -> n) . ntree

nthElemFun
    :: (XmlNavTree -> [XmlNavTree])
    -> (Int -> Bool) -> XmlNavTree -> [XmlNavTree]
nthElemFun axis p x = [x | p n]
  where
    n = 1 + length (filter isElemNodeN $ axis x)

isElemNodeN :: XmlNavTree -> Bool
isElemNodeN = isElemNode . ntree

isElemN :: ArrowXml a => a XmlNavTree XmlNavTree
isElemN = withoutNav isElem

hasWord :: String -> String -> Bool
hasWord w = any (== w) . wordsBy isSpace

{- $supported_selectors
* Element selectors: @*@, @E@, @.class@, @#id@

* Relationship selectors: @E F@, @E > F@, @E + F@, @E ~ F@

* Attribute selectors: @[attr]@, @[attr=\"value\"]@, @[attr~=\"value\"]@,
@[attr|=\"value\"]@, @[attr^=\"value\"]@, @[attr$=\"value\"]@,
@[attr*=\"value\"]@

* Pseudo-classes: @:not(..)@, @:empty@, @:root@, @:first-child@, @:last-child@,
@:only-child@, @:nth-child(N)@, @:nth-last-child(N)@, @:first-of-type@,
@:last-of-type@, @:only-of-type@, @:nth-of-type(N)@, @:nth-last-of-type(N)@

The argument to the @:nth-child()@ family of pseudo-classes can take one of
the following forms: @6@, @2n@, @n+2@, @3n-1@, @-n+6@, @odd@, @even@.
-}

{- $example
> import Text.XML.HXT.Core
> import Text.XML.HXT.CSS
>
> test :: IO [XmlTree]
> test = runX $ doc >>> css "div > span + p:not(:nth-of-type(3n-1))"
>   where
>     doc = readDocument [withParseHTML yes, withWarnings no] path
>     path = "/path/to/document.html"
-}
