{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines types for zipping and iterating over HTML trees.
module Zenacy.HTML.Internal.Zip
  ( HTMLZipper
  , HTMLZipAction
  , HTMLIter
  , HTMLZipPath(..)
  , htmlZip
  , htmlZipM
  , htmlUnzip
  , htmlUnzipM
  , htmlZipNode
  , htmlZipNodeM
  , htmlZipRoot
  , htmlZipRootM
  , htmlZipUp
  , htmlZipParent
  , htmlZipFirst
  , htmlZipLast
  , htmlZipFind
  , htmlZipNext
  , htmlZipPrev
  , htmlZipGet
  , htmlZipTest
  , htmlZipTestNode
  , htmlZipTestName
  , htmlZipTestFirst
  , htmlZipTestLast
  , htmlZipModify
  , htmlZipModifyM
  , htmlZipDelete
  , htmlZipCollapse
  , htmlZipInsertBefore
  , htmlZipInsertAfter
  , htmlZipContentBefore
  , htmlZipContentAfter
  , htmlZipContentLeft
  , htmlZipContentRight
  , htmlZipDropBefore
  , htmlZipDropAfter
  , htmlZipDropLeft
  , htmlZipDropRight
  , htmlZipPruneBefore
  , htmlZipPruneAfter
  , htmlZipPruneLeft
  , htmlZipPruneRight
  , htmlZipRepeat
  , htmlZipStepNext
  , htmlZipStepBack
  , htmlZipSearch
  , htmlZipIndex
  , htmlIter
  , htmlIterZipper
  , htmlIterSearch
  , htmlIterModify
  , htmlIterNext
  , htmlIterBack
  , htmlZipPath
  , htmlZipPathEmpty
  , htmlZipPathFind
  ) where

import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.HTML
import Zenacy.HTML.Internal.Oper
import Control.Monad
  ( (>=>)
  )
import Data.Bool
  ( bool
  )
import Data.Default
  ( Default(..)
  )
import Data.Maybe
  ( fromMaybe
  , isNothing
  )
import Data.Monoid
  ( (<>)
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( unpack
  )

-- | The zipper crumb definition.
data HTMLCrumb = HTMLCrumb HTMLNode [HTMLNode] [HTMLNode]

-- | The zipper type.
data HTMLZipper = HTMLZipper HTMLNode [HTMLCrumb]

-- | Defines an action on a zipper.
type HTMLZipAction = HTMLZipper -> Maybe HTMLZipper

-- | Defines a zip direction.
data Direction = Down | Across | Up

-- | The zipper iterator type.
data HTMLIter = HTMLIter Direction HTMLZipper

-- | Defines the type for a path.
newtype HTMLZipPath = HTMLZipPath [Int] deriving (Show, Eq, Ord)

-- | Defaults for zip path.
instance Default HTMLZipPath where
  def = htmlZipPathEmpty

-- | Creates a zipper for a HTML node.
htmlZip :: HTMLNode -> HTMLZipper
htmlZip x = HTMLZipper x []

-- | Creates a zipper for a HTML node in a Monad.
htmlZipM :: Monad m => HTMLNode -> m HTMLZipper
htmlZipM = pure . htmlZip

-- | Extracts the HTML node from a zipper.
htmlUnzip :: HTMLZipper -> HTMLNode
htmlUnzip = htmlZipNode . htmlZipRoot

-- | Extracts the HTML node from a zipper in a Monad.
htmlUnzipM :: Monad m => HTMLZipper -> m HTMLNode
htmlUnzipM = pure . htmlUnzip

-- | Gets the current HTML node.
htmlZipNode :: HTMLZipper -> HTMLNode
htmlZipNode (HTMLZipper x _) = x

-- | Gets the current HTML node in a Monad.
htmlZipNodeM :: Monad m => HTMLZipper -> m HTMLNode
htmlZipNodeM = pure . htmlZipNode

-- | Moves the zipper to the root HTML node.
htmlZipRoot :: HTMLZipper -> HTMLZipper
htmlZipRoot x = maybe x htmlZipRoot $ htmlZipParent x

-- | Moves the zipper to the root HTML node in a Monad.
htmlZipRootM :: Monad m => HTMLZipper -> m HTMLZipper
htmlZipRootM = pure . htmlZipRoot

-- | Moves the zipper to the parent node.
htmlZipUp :: HTMLZipper -> Maybe HTMLZipper
htmlZipUp = htmlZipParent

-- | Moves the zipper to the parent node.
htmlZipParent :: HTMLZipper -> Maybe HTMLZipper
htmlZipParent = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n ls rs):cs) ->
    let c = reverse ls <> [x] <> rs
    in case n of
      HTMLDocument n m [] ->
        Just $ HTMLZipper (HTMLDocument n m c) cs
      HTMLDoctype n p s ->
        Nothing
      HTMLFragment n [] ->
        Just $ HTMLZipper (HTMLFragment n c) cs
      HTMLElement n s a [] ->
        Just $ HTMLZipper (HTMLElement n s a c) cs
      HTMLTemplate s a c ->
        Nothing
      HTMLText t ->
        Nothing
      HTMLComment c ->
        Nothing

-- | Moves the zipper to the first child node.
htmlZipFirst :: HTMLZipper -> Maybe HTMLZipper
htmlZipFirst (HTMLZipper y z) = case y of
  HTMLDocument n m c ->
    f c $ HTMLDocument n m []
  HTMLFragment n c ->
    f c $ HTMLFragment n []
  HTMLElement n s a c ->
    f c $ HTMLElement n s a []
  _ -> Nothing
  where
    f [] n = Nothing
    f (h:rs) n = Just $ HTMLZipper h ((HTMLCrumb n [] rs):z)

-- | Moves the zipper to the last child node.
htmlZipLast :: HTMLZipper -> Maybe HTMLZipper
htmlZipLast (HTMLZipper y z) = case y of
  HTMLDocument n m c ->
    f c $ HTMLDocument n m []
  HTMLFragment n c ->
    f c $ HTMLFragment n []
  HTMLElement n s a c ->
    f c $ HTMLElement n s a []
  _ -> Nothing
  where
    f xs n =
      case reverse xs of
        (h:ls) ->
          Just $ HTMLZipper h ((HTMLCrumb n ls []):z)
        [] ->
          Nothing

-- | Moves the zipper to a named child element.
htmlZipFind :: (HTMLNode -> Bool) -> HTMLZipper -> Maybe HTMLZipper
htmlZipFind p (HTMLZipper y z) = case y of
  HTMLDocument n m c ->
    f c $ HTMLDocument n m []
  HTMLFragment n c ->
    f c $ HTMLFragment n []
  HTMLElement n s a c ->
    f c $ HTMLElement n s a []
  _ -> Nothing
  where
    f c n = case break p c of
      (ls, []) -> Nothing
      (ls, h:rs) -> Just $
        HTMLZipper h ((HTMLCrumb n (reverse ls) rs):z)

-- | Moves to the next sibling.
htmlZipNext :: HTMLZipper -> Maybe HTMLZipper
htmlZipNext = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n ls []):cs) -> Nothing
  HTMLZipper x ((HTMLCrumb n ls (h:rs)):cs) ->
    Just $ HTMLZipper h ((HTMLCrumb n (x:ls) rs):cs)

-- | Moves to the previous sibling.
htmlZipPrev :: HTMLZipper -> Maybe HTMLZipper
htmlZipPrev = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n [] rs):cs) -> Nothing
  HTMLZipper x ((HTMLCrumb n (h:ls) rs):cs) ->
    Just $ HTMLZipper h ((HTMLCrumb n ls (x:rs)):cs)

-- | Gets the child specified by an index.
htmlZipGet :: Int -> HTMLZipper -> Maybe HTMLZipper
htmlZipGet n z
  | n < 0 = Nothing
  | n == 0 = htmlZipFirst z
  | otherwise = htmlZipFirst z >>= f n
  where
    f 0 = Just
    f n = htmlZipNext >=> f (n - 1)

-- | Continues a zipper if a test is passed.
htmlZipTest :: (HTMLZipper -> Bool) -> HTMLZipper -> Maybe HTMLZipper
htmlZipTest f z = bool Nothing (Just z) $ f z

-- | Continues a zipper if a node test is passed.
htmlZipTestNode :: (HTMLNode -> Bool) -> HTMLZipper -> Maybe HTMLZipper
htmlZipTestNode f = htmlZipTest $ f . htmlZipNode

-- | Tests the current node for an element name.
htmlZipTestName :: Text -> HTMLZipper -> Maybe HTMLZipper
htmlZipTestName x = htmlZipTest (htmlElemHasName x . htmlZipNode)

-- | Test whether the zipper is at the first child node.
htmlZipTestFirst :: HTMLZipper -> Maybe HTMLZipper
htmlZipTestFirst = htmlZipTest (isNothing . htmlZipPrev)

-- | Test whether the zipper is at the last child node.
htmlZipTestLast :: HTMLZipper -> Maybe HTMLZipper
htmlZipTestLast = htmlZipTest (isNothing . htmlZipNext)

-- | Modifies the currently focused node.
htmlZipModify :: (HTMLNode -> HTMLNode) -> HTMLZipper -> HTMLZipper
htmlZipModify f (HTMLZipper y z) = HTMLZipper (f y) z

-- | Modifies the currently focused node in a Monad.
htmlZipModifyM :: Monad m => (HTMLNode -> HTMLNode) -> HTMLZipper -> m HTMLZipper
htmlZipModifyM f = pure . htmlZipModify f

-- | Deletes the current node.
htmlZipDelete :: HTMLZipper -> Maybe HTMLZipper
htmlZipDelete = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n l r):cs) ->
    let c = reverse l <> r
    in case n of
      HTMLDocument n m [] ->
        Just $ HTMLZipper (HTMLDocument n m c) cs
      HTMLFragment n [] ->
        Just $ HTMLZipper (HTMLFragment n c) cs
      HTMLElement n s a [] ->
        Just $ HTMLZipper (HTMLElement n s a c) cs
      _ -> Nothing

-- | Collapses the current node.
htmlZipCollapse :: HTMLZipper -> Maybe HTMLZipper
htmlZipCollapse = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n l r):cs) ->
    let c = reverse l <> htmlNodeContent x <> r
    in case n of
      HTMLDocument n m [] ->
        Just $ HTMLZipper (HTMLDocument n m c) cs
      HTMLFragment n [] ->
        Just $ HTMLZipper (HTMLFragment n c) cs
      HTMLElement n s a [] ->
        Just $ HTMLZipper (HTMLElement n s a c) cs
      _ -> Nothing

-- | Inserts a node before the current node.
htmlZipInsertBefore :: HTMLNode -> HTMLZipper -> Maybe HTMLZipper
htmlZipInsertBefore y = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n l r):cs) ->
    Just $ HTMLZipper x ((HTMLCrumb n (y:l) r):cs)

-- | Inserts a node after the current node.
htmlZipInsertAfter :: HTMLNode -> HTMLZipper -> Maybe HTMLZipper
htmlZipInsertAfter y = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n l r):cs) ->
    Just $ HTMLZipper x ((HTMLCrumb n l (y:r)):cs)

-- | Gets the siblings to the left of the current node.
htmlZipContentBefore :: HTMLZipper -> [HTMLNode]
htmlZipContentBefore = \case
  HTMLZipper x [] -> []
  HTMLZipper x ((HTMLCrumb n l r):cs) -> reverse l

-- | Gets the siblings to the right of the current node.
htmlZipContentAfter :: HTMLZipper -> [HTMLNode]
htmlZipContentAfter = \case
  HTMLZipper x [] -> []
  HTMLZipper x ((HTMLCrumb n l r):cs) -> r

-- | Synonym for htmlZipContentBefore.
htmlZipContentLeft :: HTMLZipper -> [HTMLNode]
htmlZipContentLeft = htmlZipContentBefore

-- | Synonym for htmlZipContentAfter.
htmlZipContentRight :: HTMLZipper -> [HTMLNode]
htmlZipContentRight = htmlZipContentAfter

-- | Drops the siblings to the left of the current node.
htmlZipDropBefore :: HTMLZipper -> Maybe HTMLZipper
htmlZipDropBefore = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n _ r):cs) ->
    Just $ HTMLZipper x ((HTMLCrumb n [] r):cs)

-- | Drops the siblings to the right of the current node.
htmlZipDropAfter :: HTMLZipper -> Maybe HTMLZipper
htmlZipDropAfter = \case
  HTMLZipper x [] -> Nothing
  HTMLZipper x ((HTMLCrumb n l _):cs) ->
    Just $ HTMLZipper x ((HTMLCrumb n l []):cs)

-- | Synonym for htmlZipDropBefore.
htmlZipDropLeft :: HTMLZipper -> Maybe HTMLZipper
htmlZipDropLeft = htmlZipDropBefore

-- | Synonym for htmlZipDropAfter.
htmlZipDropRight :: HTMLZipper -> Maybe HTMLZipper
htmlZipDropRight = htmlZipDropAfter

-- | Drops all of the branches to the left of the current node
--   while moving up to and ending at the root.
htmlZipPruneBefore :: HTMLZipper -> Maybe HTMLZipper
htmlZipPruneBefore = htmlZipRepeat safeDrop htmlZipParent
  where
    safeDrop z = Just $ fromMaybe z $ htmlZipDropBefore z

-- | Drops all of the branches to the right of the current node
--   while moving up to and ending at the root.
htmlZipPruneAfter :: HTMLZipper -> Maybe HTMLZipper
htmlZipPruneAfter = htmlZipRepeat safeDrop htmlZipParent
  where
    safeDrop z = Just $ fromMaybe z $ htmlZipDropAfter z

-- | Synonym for htmlZipPruneBefore.
htmlZipPruneLeft :: HTMLZipper -> Maybe HTMLZipper
htmlZipPruneLeft = htmlZipPruneBefore

-- | Synonym for htmlZipPruneAfter.
htmlZipPruneRight :: HTMLZipper -> Maybe HTMLZipper
htmlZipPruneRight = htmlZipPruneAfter

-- | Repeats a zipper action until another zipper returns Nothing.
htmlZipRepeat :: HTMLZipAction -> HTMLZipAction -> HTMLZipAction
htmlZipRepeat f g z =
  case f z of
    Nothing -> Nothing
    Just z1 -> case g z1 of
      Nothing -> Just z1
      Just z2 -> htmlZipRepeat f g z2

-- | Step a zipper forward one node.
htmlZipStepNext :: HTMLZipper -> Maybe HTMLZipper
htmlZipStepNext = htmlZipStep htmlZipFirst htmlZipNext

-- | Step a zipper back one node.
htmlZipStepBack :: HTMLZipper -> Maybe HTMLZipper
htmlZipStepBack = htmlZipStep htmlZipLast htmlZipPrev

-- | Step a zipper.
htmlZipStep :: HTMLZipAction -> HTMLZipAction -> HTMLZipAction
htmlZipStep first next z =
  case first z of
    Just x -> Just x
    Nothing -> f z
  where
    f x = case next x of
      Just a -> Just a
      Nothing -> case htmlZipParent x of
        Just b -> f b
        Nothing -> Nothing

-- | Searches a zipper until a predicate is true.
htmlZipSearch
  :: (HTMLZipper -> Maybe HTMLZipper)
  -> (HTMLZipper -> Bool)
  -> HTMLZipper
  -> Maybe HTMLZipper
htmlZipSearch step test x
  | test x = Just x
  | otherwise = maybe Nothing (htmlZipSearch step test) $ step x

-- | Gets the index for a node.
htmlZipIndex :: HTMLZipper -> Maybe Int
htmlZipIndex = \case
  HTMLZipper _ [] -> Nothing
  HTMLZipper _ ((HTMLCrumb _ ls _):_) -> Just $ length ls

-- | Dumps a zipper to a string.
htmlZipDump :: HTMLZipper -> String
htmlZipDump (HTMLZipper n cs) =
  name n <> "\n" <> go cs
  where
    go :: [HTMLCrumb] -> String
    go [] = ""
    go ((HTMLCrumb n ls rs):cs) =
      name n <> "\n"
      <> " ls: " <> names ls <> "\n"
      <> " rs: " <> names rs <> "\n"
      <> go cs
    name = T.unpack . htmlElemName
    names = show . map name

-- | Returns an iterator for a zipper.
htmlIter :: HTMLZipper -> HTMLIter
htmlIter = HTMLIter Down

-- | Gets the iterator for a zipper.
htmlIterZipper :: HTMLIter -> HTMLZipper
htmlIterZipper (HTMLIter _ z) = z

-- | Modifies the zipper for an interator.
htmlIterModify :: (HTMLZipper -> HTMLZipper) -> HTMLIter -> HTMLIter
htmlIterModify f (HTMLIter d z) = (HTMLIter d $ f z)

-- | Advances an iterator to the next element.
htmlIterNext :: HTMLIter -> Maybe HTMLIter
htmlIterNext = iterStep htmlZipFirst htmlZipNext

-- | Advances an iterator to the previous element.
htmlIterBack :: HTMLIter -> Maybe HTMLIter
htmlIterBack = iterStep htmlZipLast htmlZipPrev

-- | Steps an iterator.
iterStep
  :: (HTMLZipper -> Maybe HTMLZipper)
  -> (HTMLZipper -> Maybe HTMLZipper)
  -> HTMLIter
  -> Maybe HTMLIter
iterStep first next = go
  where
    go (HTMLIter d z) =
      case d of
        Down ->
          case first z of
            Just x -> Just $ HTMLIter Down x
            Nothing -> go $ HTMLIter Across z
        Across ->
          case next z of
            Just x -> Just $ HTMLIter Down x
            Nothing -> go $ HTMLIter Up z
        Up ->
          case htmlZipParent z of
            Just x -> Just $ HTMLIter Across x
            Nothing -> Nothing

-- | Searches an iterator until a predicate is true.
htmlIterSearch
  :: (HTMLIter -> Maybe HTMLIter)
  -> (HTMLZipper -> Bool)
  -> HTMLIter
  -> Maybe HTMLIter
htmlIterSearch step test x@(HTMLIter _ z)
  | test z = Just x
  | otherwise = maybe Nothing (htmlIterSearch step test) $ step x

-- | Defines an empty path.
htmlZipPathEmpty :: HTMLZipPath
htmlZipPathEmpty = HTMLZipPath []

-- | Gets the path for a node.
htmlZipPath :: HTMLZipper -> HTMLZipPath
htmlZipPath = maybe (HTMLZipPath []) id . go htmlZipPathEmpty
  where
    go :: HTMLZipPath -> HTMLZipper -> Maybe HTMLZipPath
    go (HTMLZipPath p) z =
      case htmlZipIndex z of
        Nothing ->
          Just $ HTMLZipPath p
        Just x ->
          case htmlZipParent z of
            Nothing -> Nothing
            Just y -> go (HTMLZipPath $ x : p) y

-- | Finds the zipper for a path starting from the current node.
htmlZipPathFind :: HTMLZipPath -> HTMLZipper -> Maybe HTMLZipper
htmlZipPathFind (HTMLZipPath p) = f p
  where
    f [] = pure
    f (x:xs) = htmlZipGet x >=> f xs
