{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines operations on html data types.
module Zenacy.HTML.Internal.Oper
  ( htmlNodeIsElem
  , htmlNodeIsText
  , htmlNodeContent
  , htmlNodeContentSet
  , htmlNodeShow
  , htmlNodeFind
  , htmlNodeCount
  , htmlNodeCountM
  , htmlTextSpace
  , htmlTextAppend
  , htmlTextPrepend
  , htmlAttrHasName
  , htmlAttrRename
  , htmlElemAttr
  , htmlElemAttrCount
  , htmlElemAttrFind
  , htmlElemAttrFindName
  , htmlElemAttrApply
  , htmlElemAttrFilter
  , htmlElemAttrMap
  , htmlElemHasAttr
  , htmlElemHasAttrName
  , htmlElemHasAttrVal
  , htmlElemHasAttrValInfix
  , htmlElemAddAttr
  , htmlElemSetAttr
  , htmlElemGetAttr
  , htmlElemAttrRemove
  , htmlElemRemoveAllAttr
  , htmlElemAttrRename
  , htmlElemID
  , htmlElemIDSet
  , htmlElemHasID
  , htmlElemFindID
  , htmlElemClass
  , htmlElemClassSet
  , htmlElemClasses
  , htmlElemClassesSet
  , htmlElemClassesAdd
  , htmlElemClassesRemove
  , htmlElemClassesContains
  , htmlElemStyle
  , htmlElemStyles
  , htmlElemStyleParseURL
  , htmlElemContent
  , htmlElemContentSet
  , htmlElemHasContent
  , htmlElemNodeFirst
  , htmlElemNodeLast
  , htmlElemNodeCount
  , htmlElemName
  , htmlElemHasName
  , htmlElemRename
  , htmlElemFindElem
  , htmlElemFindElemNamed
  , htmlElemHasElem
  , htmlElemHasElemNamed
  , htmlElemContentApply
  , htmlElemContentMap
  , htmlElemContentFilter
  , htmlElemSearch
  , htmlElemText
  , htmlDocHtml
  , htmlDocBody
  , htmlDocHead
  , htmlDocTitle
  , htmlMapElem
  , htmlMapElemM
  , htmlElemCollapse
  , htmlElemCollapseM
  ) where

import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.HTML
import Control.Monad
  ( (>=>)
  )
import Control.Monad.Extra as X
  ( whenJust
  , whenJustM
  , concatMapM
  , ifM
  )
-- import Control.Monad.Identity
--   ( runIdentity
--   )
import Data.Char
  ( isSpace
  )
import Data.Functor.Identity
  ( runIdentity
  )
import Data.List
  ( find
  )
import Data.List.Extra
  ( firstJust
  )
import Data.Map
  ( Map
  )
import qualified Data.Map as Map
  ( empty
  , fromList
  , lookup
  )
import Data.Maybe
  ( listToMaybe
  , isJust
  )
import Data.Monoid
  ( (<>)
  )
import Data.Set
  ( Set
  )
import qualified Data.Set as Set
  ( delete
  , empty
  , fromList
  , insert
  , member
  , notMember
  , toList
  , union
  , unions
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( all
  , append
  , breakOn
  , concat
  , drop
  , dropAround
  , empty
  , isInfixOf
  , isPrefixOf
  , null
  , split
  , splitOn
  , strip
  , words
  , unwords
  )
import Data.Tuple.Extra
  ( first
  , second
  )

-- | Determines if a node is an element node.
htmlNodeIsElem :: HTMLNode -> Bool
htmlNodeIsElem HTMLElement {} = True
htmlNodeIsElem _ = False

-- | Determines if a node is a text node.
htmlNodeIsText :: HTMLNode -> Bool
htmlNodeIsText HTMLText {} = True
htmlNodeIsText _ = False

-- | Gets the content of a node.
htmlNodeContent :: HTMLNode -> [HTMLNode]
htmlNodeContent (HTMLDocument _ _ c) = c
htmlNodeContent (HTMLFragment _ c) = c
htmlNodeContent (HTMLElement _ _ _ c) = c
htmlNodeContent _ = []

-- | Sets the content of a node.
htmlNodeContentSet :: [HTMLNode] -> HTMLNode -> HTMLNode
htmlNodeContentSet x (HTMLDocument n m c) = HTMLDocument n m x
htmlNodeContentSet x (HTMLFragment n c) = HTMLFragment n x
htmlNodeContentSet x (HTMLElement n s a c) = HTMLElement n s a x
htmlNodeContentSet x y = y

-- | Shows the node without its content.
htmlNodeShow :: HTMLNode -> String
htmlNodeShow = show . htmlNodeContentSet []

-- | Finds a child node using a predicate.
htmlNodeFind :: (HTMLNode -> Bool) -> HTMLNode -> Maybe HTMLNode
htmlNodeFind p x = find p $ htmlNodeContent x

-- | Counts the number of nodes matching a predicate.
htmlNodeCount :: (HTMLNode -> Bool) -> HTMLNode -> Int
htmlNodeCount f = runIdentity . htmlNodeCountM (pure . f)

-- | Counts the number of nodes matching a predicate.
htmlNodeCountM :: Monad m => (HTMLNode -> m Bool) -> HTMLNode -> m Int
htmlNodeCountM f x = do
  n <- sum <$> mapM (htmlNodeCountM f) (htmlNodeContent x)
  ifM (f x) (pure $ 1 + n) (pure n)

-- | Determines if a node is a text node containing only whitespace.
htmlTextSpace :: HTMLNode -> Bool
htmlTextSpace (HTMLText x) = T.all isSpace x
htmlTextSpace _ = False

-- | Appends text to a text node.
htmlTextAppend :: Text -> HTMLNode -> HTMLNode
htmlTextAppend a (HTMLText x) = HTMLText $ T.append x a
htmlTextAppend a x = x

-- | Prepends text to a text node.
htmlTextPrepend :: Text -> HTMLNode -> HTMLNode
htmlTextPrepend a (HTMLText x) = HTMLText $ T.append a x
htmlTextPrepend a x = x

-- | A predicate for checking attribute names.
htmlAttrHasName :: Text -> HTMLAttr -> Bool
htmlAttrHasName x a = x == htmlAttrName a

-- | Renames an attribute.
htmlAttrRename :: Text -> HTMLAttr -> HTMLAttr
htmlAttrRename x (HTMLAttr n v s) = HTMLAttr x v s

-- | Gets the attributes for an element.
htmlElemAttr :: HTMLNode -> [HTMLAttr]
htmlElemAttr (HTMLElement _ _ a _) = a
htmlElemAttr _ = []

-- | Gets the number of attributes for an element.
htmlElemAttrCount :: HTMLNode -> Int
htmlElemAttrCount = length . htmlElemAttr

-- | Finds an attribute for an element.
htmlElemAttrFind :: (HTMLAttr -> Bool) -> HTMLNode -> Maybe HTMLAttr
htmlElemAttrFind f (HTMLElement _ _ a _) = find f a
htmlElemAttrFind _ _ = Nothing

-- | Finds an attribute by name for an element.
htmlElemAttrFindName :: Text -> HTMLNode -> Maybe HTMLAttr
htmlElemAttrFindName x = htmlElemAttrFind $ htmlAttrHasName x

-- | Applies a function to the attributes for an element.
htmlElemAttrApply :: ([HTMLAttr] -> [HTMLAttr]) -> HTMLNode -> HTMLNode
htmlElemAttrApply f (HTMLElement n s a c) = HTMLElement n s (f a) c
htmlElemAttrApply _ x = x

-- | Filters the attributes for an element.
htmlElemAttrFilter :: (HTMLAttr -> Bool) -> HTMLNode -> HTMLNode
htmlElemAttrFilter f = htmlElemAttrApply $ filter f

-- | Maps an endofunctor over an element attributes.
htmlElemAttrMap :: (HTMLAttr -> HTMLAttr) -> HTMLNode -> HTMLNode
htmlElemAttrMap f = htmlElemAttrApply $ map f

-- | Determines if the element has attributes.
htmlElemHasAttr :: HTMLNode -> Bool
htmlElemHasAttr x = htmlElemAttrCount x > 0

-- | Determines if an element has an attribute.
htmlElemHasAttrName :: Text -> HTMLNode -> Bool
htmlElemHasAttrName x = isJust . htmlElemAttrFindName x

-- | Determines if an element has an attribute value.
htmlElemHasAttrVal :: Text -> Text -> HTMLNode -> Bool
htmlElemHasAttrVal x y z =
  maybe False (\a -> y == htmlAttrVal a) $ htmlElemAttrFindName x z

-- | Determines if an element has part of an attribute value.
htmlElemHasAttrValInfix :: Text -> Text -> HTMLNode -> Bool
htmlElemHasAttrValInfix x y z =
  maybe False (\a -> y `T.isInfixOf` htmlAttrVal a) $ htmlElemAttrFindName x z

-- | Adds an attribute to an element.
htmlElemAddAttr :: HTMLAttr -> HTMLNode -> HTMLNode
htmlElemAddAttr x (HTMLElement n s a c) = HTMLElement n s (a <> [x]) c
htmlElemAddAttr x y = y

-- | Sets an attribute value.
htmlElemSetAttr :: Text -> Text -> HTMLNode -> HTMLNode
htmlElemSetAttr x v n =
  if htmlElemHasAttrName x n
     then htmlElemAttrMap f n
     else htmlElemAddAttr (htmlAttr x v) n
  where
    f a@(HTMLAttr an av as) =
      if an == x then (HTMLAttr an v as) else a

-- | Gets an attribute value.
htmlElemGetAttr :: Text -> HTMLNode -> Maybe Text
htmlElemGetAttr x n = htmlAttrVal <$> htmlElemAttrFindName x n

-- | Removes an attribute from an element.
htmlElemAttrRemove :: Text -> HTMLNode -> HTMLNode
htmlElemAttrRemove x (HTMLElement n s a c) = HTMLElement n s a' c
  where a' = filter (\y -> htmlAttrName y /= x) a
htmlElemAttrRemove x y = y

-- | Removes all attributes from an element.
htmlElemRemoveAllAttr :: HTMLNode -> HTMLNode
htmlElemRemoveAllAttr (HTMLElement n s a c) = HTMLElement n s [] c
htmlElemRemoveAllAttr x = x

-- | Renames an attribute for an element.
htmlElemAttrRename :: Text -> Text -> HTMLNode -> HTMLNode
htmlElemAttrRename old new = htmlElemAttrMap rename
  where
    rename x =
      if htmlAttrHasName old x
         then htmlAttrRename new x
         else x

-- | Gets the id attribute for an element.
htmlElemID :: HTMLNode -> Maybe Text
htmlElemID = htmlElemGetAttr "id"

-- | Sets the id attribute for an element.
htmlElemIDSet :: Text -> HTMLNode -> HTMLNode
htmlElemIDSet = htmlElemSetAttr "id"

-- | Determines if an element has an id.
htmlElemHasID :: Text -> HTMLNode -> Bool
htmlElemHasID x y = htmlElemID y == Just x

-- | Searches for an element with an id.
htmlElemFindID :: Text -> HTMLNode -> Maybe HTMLNode
htmlElemFindID x = htmlElemSearch $ htmlElemHasID x

-- | Gets the id attribute for an element.
htmlElemClass :: HTMLNode -> Maybe Text
htmlElemClass = htmlElemGetAttr "class"

-- | Sets the class attribute for an element.
htmlElemClassSet :: Text -> HTMLNode -> HTMLNode
htmlElemClassSet = htmlElemSetAttr "class"

-- | Gets the element classes.
htmlElemClasses :: HTMLNode -> Set Text
htmlElemClasses = maybe Set.empty (Set.fromList . T.words) . htmlElemClass

-- | Sets the element classes.
htmlElemClassesSet :: Set Text -> HTMLNode -> HTMLNode
htmlElemClassesSet s = htmlElemClassSet (T.unwords $ Set.toList s)

-- | Adds the class to the element's classes.
htmlElemClassesAdd :: Text -> HTMLNode -> HTMLNode
htmlElemClassesAdd c x =
  htmlElemClassesSet (Set.insert c $ htmlElemClasses x) x

-- | Removes a class from the element's classes.
htmlElemClassesRemove :: Text -> HTMLNode -> HTMLNode
htmlElemClassesRemove c x =
  htmlElemClassesSet (Set.delete c $ htmlElemClasses x) x

-- | Determines if the element contains a class.
htmlElemClassesContains :: Text -> HTMLNode -> Bool
htmlElemClassesContains c = Set.member c . htmlElemClasses

-- | Gets the style attribute for an element.
htmlElemStyle :: HTMLNode -> Maybe Text
htmlElemStyle = htmlElemGetAttr "style"

-- | Gets the styles for an element.
htmlElemStyles :: HTMLNode -> Map Text Text
htmlElemStyles =
  maybe Map.empty parse . htmlElemStyle
  where
    parse =
      ( Map.fromList
      . map
        ( first T.strip
        . second T.strip
        . second (T.drop 1)
        . T.breakOn ":"
        )
      . filter (not . T.null)
      . map T.strip
      . T.splitOn ";"
      )

-- | Parses and returns a url style value.
htmlElemStyleParseURL :: Text -> Maybe Text
htmlElemStyleParseURL x
  | "url" `T.isPrefixOf` x =
      ( T.strip
      . T.dropAround (=='\'')
      -- Only a stylesheet can have a double quote, but we check for it anyway.
      . T.dropAround (=='\"')
      . T.strip
      ) <$> textExtract "(" ")" x
  | otherwise = Nothing

-- | Gets the children for the element if the node is an element.
htmlElemContent :: HTMLNode -> [HTMLNode]
htmlElemContent (HTMLElement _ _ _ c) = c
htmlElemContent _ = []

-- | Sets the content for an element.
htmlElemContentSet :: [HTMLNode] -> HTMLNode -> HTMLNode
htmlElemContentSet x (HTMLElement n s a c) = HTMLElement n s a x
htmlElemContentSet x y = y

-- | Determines if the element has children.
htmlElemHasContent :: HTMLNode -> Bool
htmlElemHasContent (HTMLElement _ _ _ []) = False
htmlElemHasContent (HTMLElement _ _ _ (x:xs)) = True
htmlElemHasContent _ = False

-- | Gets the first child for an element.
htmlElemNodeFirst :: HTMLNode -> Maybe HTMLNode
htmlElemNodeFirst = listToMaybe . htmlElemContent

-- | Gets the last child for an element.
htmlElemNodeLast :: HTMLNode -> Maybe HTMLNode
htmlElemNodeLast =  listToMaybe . reverse . htmlElemContent

-- | Gets the number of children for an element.
htmlElemNodeCount :: HTMLNode -> Int
htmlElemNodeCount = length . htmlElemContent

-- | Gets the name of an element.
htmlElemName :: HTMLNode -> Text
htmlElemName (HTMLElement n _ _ _) = n
htmlElemName _ = T.empty

-- | Checks if the name of an element matches a specified name.
htmlElemHasName :: Text -> HTMLNode -> Bool
htmlElemHasName x y = htmlElemName y == x

-- | Sets the name of an element.
htmlElemRename :: Text -> HTMLNode -> HTMLNode
htmlElemRename n (HTMLElement _ s a c) = HTMLElement n s a c
htmlElemRename n x = x

-- | Finds a child element using a predicate.
htmlElemFindElem :: (HTMLNode -> Bool) -> HTMLNode -> Maybe HTMLNode
htmlElemFindElem p (HTMLElement _ _ _ c) = find p c
htmlElemFindElem _ _ = Nothing

-- | Finds a child element with a specified name.
htmlElemFindElemNamed :: Text -> HTMLNode -> Maybe HTMLNode
htmlElemFindElemNamed x = htmlElemFindElem $ htmlElemHasName x

-- | Determines if an element has a child.
htmlElemHasElem :: (HTMLNode -> Bool) -> HTMLNode -> Bool
htmlElemHasElem p = isJust . htmlElemFindElem p

-- | Determines if an element has a child.
htmlElemHasElemNamed :: Text -> HTMLNode -> Bool
htmlElemHasElemNamed x = isJust . htmlElemFindElemNamed x

-- | Modifies an elements children by applying a function.
htmlElemContentApply :: ([HTMLNode] -> [HTMLNode]) -> HTMLNode -> HTMLNode
htmlElemContentApply f (HTMLElement n s a c) = HTMLElement n s a $ f c
htmlElemContentApply _ x = x

-- | Modifies an elements children by mapping a function over them.
htmlElemContentMap :: (HTMLNode -> HTMLNode) -> HTMLNode -> HTMLNode
htmlElemContentMap f = htmlElemContentApply $ map f

-- | Modifies an elements children by filtering them.
htmlElemContentFilter :: (HTMLNode -> Bool) -> HTMLNode -> HTMLNode
htmlElemContentFilter f = htmlElemContentApply $ filter f

-- | Finds an element using a depth-first search.
htmlElemSearch :: (HTMLNode -> Bool) -> HTMLNode -> Maybe HTMLNode
htmlElemSearch f x = case x of
  HTMLElement _ _ _ c ->
    if f x then Just x else firstJust (htmlElemSearch f) c
  _otherwise -> Nothing

-- | Gets the text content for an element.
htmlElemText :: HTMLNode -> Maybe Text
htmlElemText (HTMLElement n s a c) =
  case filter htmlNodeIsText c of
    a@(x:xs) -> Just . T.concat . map htmlTextData $ a
    [] -> Nothing
htmlElemText _ = Nothing

-- | Finds the html element given a document.
htmlDocHtml :: HTMLNode -> Maybe HTMLNode
htmlDocHtml = htmlNodeFind $ htmlElemHasName "html"

-- | Finds the body element given a document.
htmlDocBody :: HTMLNode -> Maybe HTMLNode
htmlDocBody = htmlDocHtml >=> htmlElemFindElemNamed "body"

-- | Finds the head element given a document.
htmlDocHead :: HTMLNode -> Maybe HTMLNode
htmlDocHead = htmlDocHtml >=> htmlElemFindElemNamed "head"

-- | Finds the title for a document.
htmlDocTitle :: HTMLNode -> Maybe Text
htmlDocTitle = htmlDocHead
  >=> htmlElemFindElemNamed "title"
  >=> htmlElemText

-- | Maps a function over all the elements defined by a node.
htmlMapElem :: (HTMLNode -> HTMLNode) -> HTMLNode -> HTMLNode
htmlMapElem f = runIdentity . htmlMapElemM (pure . f)

-- | Maps a function over all the elements defined by a node.
htmlMapElemM :: Monad m => (HTMLNode -> m HTMLNode) -> HTMLNode -> m HTMLNode

-- htmlMapElemM f x@(HTMLElement {}) = do
--   HTMLElement n s a c <- f x
--   HTMLElement n s a <$> mapM (htmlMapElemM f) c
-- htmlMapElemM f x = pure x

htmlMapElemM f x =
  case x of
    HTMLElement {} -> do
      a <- f x
      case a of
        HTMLElement n s a c ->
          HTMLElement n s a <$> mapM (htmlMapElemM f) c
        _otherwise ->
          pure a
    _otherwise ->
      pure x

-- | Collapses a tree of elements based on a predicate.
htmlElemCollapse :: (HTMLNode -> Bool) -> HTMLNode -> [HTMLNode]
htmlElemCollapse f = runIdentity . htmlElemCollapseM (pure . f)

-- | Collapses a tree of elements based on a predicate.
htmlElemCollapseM :: Monad m => (HTMLNode -> m Bool) -> HTMLNode -> m [HTMLNode]
htmlElemCollapseM f x@(HTMLElement n s a c) = do
  c' <- concatMapM (htmlElemCollapseM f) c
  ifM (f x) (pure c') $ pure [ HTMLElement n s a c' ]
htmlElemCollapseM _ x = pure [x]
