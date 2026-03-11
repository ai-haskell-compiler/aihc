{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Expat.Cursor
--
-- This module ported from Text.XML.Light.Cursor
--
-- XML cursors for working XML content withing the context of
-- an XML document.  This implementation is based on the general
-- tree zipper written by Krasimir Angelov and Iavor S. Diatchki.
--
-- With the exception of 'modifyContentM', then M-suffixed functions are
-- for use with monadic node types, as used when dealing with chunked I\/O
-- with the /hexpat-iteratee/ package.  In the more common pure case, you
-- wouldn't need these *M functions.

module Text.XML.Expat.Cursor
  ( 
  -- * Types
    Cursor, CursorG(..), Path, PathG
  , Tag(..), getTag, fromTag

  -- * Conversions
  , fromTree
  , fromForest
  , toForest
  , toTree

  -- * Moving around
  , parent
  , root
  , getChild
  , getChildM
  , firstChild
  , firstChildM
  , lastChild
  , lastChildM
  , left
  , leftM
  , right
  , rightM
  , nextDF
  , nextDFM

  -- ** Searching
  , findChild
  , findLeft
  , findRight
  , findRec
  , findRecM

  -- * Node classification
  , isRoot
  , isFirst
  , isFirstM
  , isLast
  , isLastM
  , isLeaf
  , isChild
  , hasChildren
  , getNodeIndex

  -- * Updates
  , setContent
  , modifyContent
  , modifyContentList
  , modifyContentListM
  , modifyContentM

  -- ** Inserting content
  , insertLeft
  , insertRight
  , insertManyLeft
  , insertManyRight
  , insertFirstChild
  , insertLastChild
  , insertManyFirstChild
  , insertManyLastChild
  , insertGoLeft
  , insertGoRight

  -- ** Removing content
  , removeLeft
  , removeLeftM
  , removeRight
  , removeRightM
  , removeGoLeft
  , removeGoLeftM
  , removeGoRight
  , removeGoRightM
  , removeGoUp

  ) where

import Text.XML.Expat.Tree
import Control.Monad (mzero, mplus)
import Data.Maybe(isNothing)
import Data.Monoid
import Data.Functor.Identity
import Data.List.Class (List(..), ListItem(..), cons, foldlL, lengthL)

data Tag tag text = Tag { tagName    :: tag
                        , tagAttribs :: Attributes tag text
                        } deriving (Show)

{-
setTag :: Tag -> Element -> Element
setTag t e = fromTag t (elContent e)
-}

fromTag :: MkElementClass n c => Tag tag text -> c (n c tag text) -> n c tag text
fromTag t cs = mkElement (tagName t) (tagAttribs t) cs

-- | Generalized path within an XML document.
type PathG n c tag text = [(c (n c tag text),Tag tag text,c (n c tag text))]

-- | A path specific to @Text.XML.Expat.Tree.Node@ trees.
type Path tag text = PathG NodeG [] tag text

-- | Generalized cursor: The position of a piece of content in an XML document.
-- @n@ is the Node type and @c@ is the list type, which would usually be [],
-- except when you're using chunked I\/O.
data CursorG n c tag text = Cur
  { current :: n c tag text       -- ^ The currently selected content.
  , lefts   :: c (n c tag text)   -- ^ Siblings on the left, closest first.
  , rights  :: c (n c tag text)   -- ^ Siblings on the right, closest first.
  , parents :: PathG n c tag text -- ^ The contexts of the parent elements of this location.
  }

instance (Show (n c tag text), Show (c (n c tag text)), Show tag, Show text)
                                           => Show (CursorG n c tag text) where
    show (Cur c l r p) = "Cur { current="++show c++
                         ", lefts="++show l++
                         ", rights="++show r++
                         ", parents="++show p++" }"

-- | A cursor specific to @Text.XML.Expat.Tree.Node@ trees.
type Cursor tag text = CursorG NodeG [] tag text

-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: MkElementClass n c => CursorG n c tag text -> Maybe (CursorG n c tag text)
parent loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Cur { current = (fromTag v
                    (combChildren (lefts loc) (current loc) (rights loc)))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: MkElementClass n c => CursorG n c tag text -> CursorG n c tag text
root loc = maybe loc root (parent loc)

-- | The left sibling of the given location - pure version.
left :: CursorG n [] tag text -> Maybe (CursorG n [] tag text)
left loc = runIdentity $ leftM loc

-- | The left sibling of the given location - used for monadic node types.
leftM :: List c => CursorG n c tag text -> ItemM c (Maybe (CursorG n c tag text))
leftM loc = do
  let l = lefts loc
  li <- runList l
  case li of
    Nil -> return Nothing
    Cons t ts -> return $ Just loc { current = t, lefts = ts
                                   , rights = cons (current loc) (rights loc) }

-- | The right sibling of the given location - pure version.
right :: CursorG n [] tag text -> Maybe (CursorG n [] tag text)
right loc = runIdentity $ rightM loc

-- | The right sibling of the given location - used for monadic node types.
rightM :: List c => CursorG n c tag text -> ItemM c (Maybe (CursorG n c tag text))
rightM loc = do
  let r = rights loc
  li <- runList r
  case li of
    Nil -> return Nothing
    Cons t ts -> return $ Just loc { current = t, lefts = cons (current loc) (lefts loc)
                                   , rights = ts }

-- | The first child of the given location - pure version.
firstChild :: (NodeClass n [], Monoid tag) => CursorG n [] tag text -> Maybe (CursorG n [] tag text)
firstChild loc = runIdentity $ firstChildM loc

-- | The first child of the given location - used for monadic node types.
firstChildM :: (NodeClass n c, Monoid tag) => CursorG n c tag text -> ItemM c (Maybe (CursorG n c tag text))
firstChildM loc = do
    case downParents loc of
        Just (l, ps) -> do
            li <- runList l
            return $ case li of
                Cons t ts -> Just $ Cur { current = t, lefts = mzero, rights = ts , parents = ps }
                Nil       -> Nothing
        Nothing -> return $ Nothing

-- | The last child of the given location - pure version.
lastChild :: (NodeClass n [], Monoid tag) => CursorG n [] tag text -> Maybe (CursorG n [] tag text)
lastChild loc = runIdentity $ lastChildM loc

-- | The last child of the given location - used for monadic node types.
lastChildM :: (NodeClass n c, Monoid tag) => CursorG n c tag text -> ItemM c (Maybe (CursorG n c tag text))
lastChildM loc = do
    case downParents loc of
        Just (l, ps) -> do
            li <- runList (reverseL l)
            return $ case li of
                Cons t ts -> Just $ Cur { current = t, lefts = ts, rights = mzero , parents = ps }
                Nil       -> Nothing
        Nothing -> return $ Nothing

-- | Find the next left sibling that satisfies a predicate.
findLeft :: NodeClass n [] =>
            (CursorG n [] tag text -> Bool)
         -> CursorG n [] tag text
         -> Maybe (CursorG n [] tag text)
findLeft p loc = runIdentity (findLeftM p loc)

-- | Find the next left sibling that satisfies a predicate.
findLeftM :: NodeClass n c =>
             (CursorG n c tag text -> Bool)
          -> CursorG n c tag text
          -> ItemM c (Maybe (CursorG n c tag text))
findLeftM p loc = do
    mLoc1 <- leftM loc
    case mLoc1 of
        Just loc1 -> if p loc1 then return (Just loc1) else findLeftM p loc1
        Nothing   -> return Nothing

-- | Find the next right sibling that satisfies a predicate - pure version.
findRight :: (CursorG n [] tag text -> Bool)
          -> CursorG n [] tag text
          -> Maybe (CursorG n [] tag text)
findRight p loc = runIdentity $ findRightM p loc

-- | Find the next right sibling that satisfies a predicate - used for monadic node types.
findRightM :: List c =>
              (CursorG n c tag text -> Bool)
           -> CursorG n c tag text
           -> ItemM c (Maybe (CursorG n c tag text))
findRightM p loc = do
    mLoc1 <- rightM loc
    case mLoc1 of
        Just loc1 -> if p loc1 then return $ Just loc1 else findRightM p loc1
        Nothing   -> return Nothing

-- | The first child that satisfies a predicate - pure version.
findChild :: (NodeClass n [], Monoid tag) =>
             (CursorG n [] tag text -> Bool)
          -> CursorG n [] tag text
          -> Maybe (CursorG n [] tag text)
findChild p loc = runIdentity $ findChildM p loc

-- | The first child that satisfies a predicate - used for monadic node types.
findChildM :: (NodeClass n c, Monoid tag) =>
              (CursorG n c tag text -> Bool)
           -> CursorG n c tag text
           -> ItemM c (Maybe (CursorG n c tag text))
findChildM p loc = do
    mLoc1 <- firstChildM loc
    case mLoc1 of
        Just loc1 -> if p loc1 then return $ Just loc1 else findRightM p loc1
        Nothing   -> return Nothing

-- | The next position in a left-to-right depth-first traversal of a document:
-- either the first child, right sibling, or the right sibling of a parent that
-- has one. Pure version.
nextDF :: (MkElementClass n [], Monoid tag) => CursorG n [] tag text -> Maybe (CursorG n [] tag text)
nextDF c = runIdentity $ nextDFM c

-- | The next position in a left-to-right depth-first traversal of a document:
-- either the first child, right sibling, or the right sibling of a parent that
-- has one. Used for monadic node types.
nextDFM :: (MkElementClass n c, Monoid tag) => CursorG n c tag text -> ItemM c (Maybe (CursorG n c tag text))
nextDFM c = do
    mFirst <- firstChildM c
    case mFirst of
        Just c' -> return $ Just c'
        Nothing -> up c
  where
    up x = do
        mRight <- rightM x
        case mRight of
            Just c' -> return $ Just c'
            Nothing ->
                case parent x of
                    Just p -> up p
                    Nothing -> return Nothing

-- | Perform a depth first search for a descendant that satisfies the
-- given predicate. Pure version.
findRec :: (MkElementClass n [], Monoid tag) =>
           (CursorG n [] tag text -> Bool)
        -> CursorG n [] tag text
        -> Maybe (CursorG n [] tag text)
findRec p c = runIdentity $ findRecM (return . p) c

-- | Perform a depth first search for a descendant that satisfies the
-- given predicate. Used for monadic node types.
findRecM :: (MkElementClass n c, Monoid tag) =>
            (CursorG n c tag text -> ItemM c Bool)
         -> CursorG n c tag text
         -> ItemM c (Maybe (CursorG n c tag text))
findRecM p c = do
    found <- p c
    if found
        then return $ Just c
        else do
            mC' <- nextDFM c
            case mC' of
                Just c' -> findRecM p c'
                Nothing -> return Nothing

-- | The child with the given index (starting from 0). - pure version.
getChild :: (NodeClass n [], Monoid tag) => Int -> CursorG n [] tag text -> Maybe (CursorG n [] tag text)
getChild n loc = runIdentity $ getChildM n loc

-- | The child with the given index (starting from 0) - used for monadic node types.
getChildM :: (NodeClass n c, Monoid tag) =>
             Int
          -> CursorG n c tag text
          -> ItemM c (Maybe (CursorG n c tag text))
getChildM n loc = do
    let mParents = downParents loc
    case mParents of
        Just (ts, ps) -> do
            mSplit <- splitChildrenM ts n
            case mSplit of
                Just (ls,t,rs) -> return $ Just $
                    Cur { current = t, lefts = ls, rights = rs, parents = ps }
                Nothing -> return Nothing
        Nothing -> return Nothing


-- | private: computes the parent for "down" operations.
downParents :: (NodeClass n c, Monoid tag) => CursorG n c tag text -> Maybe (c (n c tag text), PathG n c tag text)
downParents loc =
  case current loc of
    e | isElement e ->
        let n = getName e
            a = getAttributes e
            c = getChildren e
        in  Just ( c
                      , cons (lefts loc, Tag n a, rights loc) (parents loc)
                      )
    _      -> Nothing

getTag :: Node tag text -> Tag tag text
getTag e = Tag { tagName = eName e
               , tagAttribs = eAttributes e
               }


-- Conversions -----------------------------------------------------------------

-- | A cursor for the given content.
fromTree :: List c => n c tag text -> CursorG n c tag text
fromTree t = Cur { current = t, lefts = mzero, rights = mzero, parents = [] }

-- | The location of the first tree in a forest - pure version.
fromForest :: NodeClass n [] => [n [] tag text] -> Maybe (CursorG n [] tag text)
fromForest l = runIdentity $ fromForestM l

-- | The location of the first tree in a forest - used with monadic node types.
fromForestM :: List c => c (n c tag text) -> ItemM c (Maybe (CursorG n c tag text))
fromForestM l = do
    li <- runList l
    return $ case li of
        Cons t ts -> Just Cur { current = t, lefts = mzero, rights = ts
                                                          , parents = [] }
        Nil       -> Nothing

-- | Computes the tree containing this location.
toTree :: MkElementClass n c => CursorG n c tag text -> n c tag text
toTree loc = current (root loc)

-- | Computes the forest containing this location.
toForest :: MkElementClass n c => CursorG n c tag text -> c (n c tag text)
toForest loc = let r = root loc in combChildren (lefts r) (current r) (rights r)


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the document?
isRoot :: CursorG n c tag text -> Bool
isRoot loc = null (parents loc)

-- | Are we at the left end of the the document? (Pure version.)
isFirst :: CursorG n [] tag text -> Bool
isFirst loc = runIdentity $ isFirstM loc

-- | Are we at the left end of the the document? (Used for monadic node types.)
isFirstM :: List c => CursorG n c tag text -> ItemM c Bool
isFirstM loc = do
    li <- runList (lefts loc)
    return $ case li of
        Nil -> True
        _   -> False

-- | Are we at the right end of the document? (Pure version.)
isLast :: CursorG n [] tag text -> Bool
isLast loc = runIdentity $ isLastM loc

-- | Are we at the right end of the document? (Used for monadic node types.)
isLastM :: List c => CursorG n c tag text -> ItemM c Bool
isLastM loc = do
    li <- runList (rights loc)
    return $ case li of
        Nil -> True
        _   -> False

-- | Are we at the bottom of the document?
isLeaf :: (NodeClass n c, Monoid tag) => CursorG n c tag text -> Bool
isLeaf loc = isNothing (downParents loc)

-- | Do we have a parent?
isChild :: CursorG n c tag text -> Bool
isChild loc = not (isRoot loc)

-- | Get the node index inside the sequence of children - pure version.
getNodeIndex :: CursorG n [] tag text -> Int
getNodeIndex loc = runIdentity $ getNodeIndexM loc

-- | Get the node index inside the sequence of children - used for monadic node types.
getNodeIndexM :: List c => CursorG n c tag text -> ItemM c Int
getNodeIndexM loc = lengthL (lefts loc)

-- | Do we have children?
hasChildren :: (NodeClass n c, Monoid tag) => CursorG n c tag text -> Bool
hasChildren loc = not (isLeaf loc)



-- Updates ---------------------------------------------------------------------

-- | Change the current content.
setContent :: n c tag text -> CursorG n c tag text -> CursorG n c tag text
setContent t loc = loc { current = t }

-- | Modify the current content.
modifyContent :: (n c tag text -> n c tag text) -> CursorG n c tag text -> CursorG n c tag text
modifyContent f loc = setContent (f (current loc)) loc

-- | Modify the current content - pure version.
modifyContentList :: NodeClass n [] =>
                     (n [] tag text -> [n [] tag text]) -> CursorG n [] tag text -> Maybe (CursorG n [] tag text)
modifyContentList f loc = runIdentity $ modifyContentListM f loc

-- | Modify the current content - used for monadic node types.
modifyContentListM :: NodeClass n c =>
                      (n c tag text -> c (n c tag text))
                   -> CursorG n c tag text
                   -> ItemM c (Maybe (CursorG n c tag text))
modifyContentListM f loc = removeGoRightM $ insertManyRight (f $ current loc) loc

-- | Modify the current content, allowing for an effect.
modifyContentM :: Monad m => (n [] tag text -> m (n [] tag text)) -> CursorG n [] tag text -> m (CursorG n [] tag text)
modifyContentM f loc = do x <- f (current loc)
                          return (setContent x loc)

-- | Insert content to the left of the current position.
insertLeft :: List c => n c tag text -> CursorG n c tag text -> CursorG n c tag text
insertLeft t loc = loc { lefts = t `cons` lefts loc }

-- | Insert content to the right of the current position.
insertRight :: List c => n c tag text -> CursorG n c tag text -> CursorG n c tag text
insertRight t loc = loc { rights = t `cons` rights loc }

-- | Insert content to the left of the current position.
insertManyLeft :: List c => c (n c tag text) -> CursorG n c tag text -> CursorG n c tag text
insertManyLeft t loc = loc { lefts = reverseL t `mplus` lefts loc }

-- | Insert content to the right of the current position.
insertManyRight :: List c => c (n c tag text) -> CursorG n c tag text -> CursorG n c tag text
insertManyRight t loc = loc { rights = t `mplus` rights loc }

-- | Insert content as the first child of the current position.
mapChildren :: NodeClass n c => (c (n c tag text) -> c (n c tag text))
            -> CursorG n c tag text
            -> Maybe (CursorG n c tag text)
mapChildren f loc = let e = current loc in
  if isElement e then
      Just $ loc { current = modifyChildren f e }
  else
      Nothing

-- | Insert content as the first child of the current position.
insertFirstChild :: NodeClass n c => n c tag text -> CursorG n c tag text -> Maybe (CursorG n c tag text)
insertFirstChild t = mapChildren (t `cons`)

-- | Insert content as the first child of the current position.
insertLastChild :: NodeClass n c => n c tag text -> CursorG n c tag text -> Maybe (CursorG n c tag text)
insertLastChild t = mapChildren (`mplus` return t)

-- | Insert content as the first child of the current position.
insertManyFirstChild :: NodeClass n c => c (n c tag text) -> CursorG n c tag text -> Maybe (CursorG n c tag text)
insertManyFirstChild t = mapChildren (t `mplus`)

-- | Insert content as the first child of the current position.
insertManyLastChild :: NodeClass n c => c (n c tag text) -> CursorG n c tag text -> Maybe (CursorG n c tag text)
insertManyLastChild t = mapChildren (`mplus` t)

-- | Remove the content on the left of the current position, if any - pure version.
removeLeft :: CursorG n [] tag text -> Maybe (n [] tag text, CursorG n [] tag text)
removeLeft loc = runIdentity $ removeLeftM loc

-- | Remove the content on the left of the current position, if any - used for monadic node types.
removeLeftM :: List c => CursorG n c tag text -> ItemM c (Maybe (n c tag text, CursorG n c tag text))
removeLeftM loc = do
    li <- runList (lefts loc)
    return $ case li of
       Cons l ls -> Just $ (l,loc { lefts = ls })
       Nil       -> Nothing

-- | Remove the content on the right of the current position, if any - pure version.
removeRight :: CursorG n [] tag text -> Maybe (n [] tag text, CursorG n [] tag text)
removeRight loc = runIdentity $ removeRightM loc

-- | Remove the content on the left of the current position, if any - used for monadic node types.
removeRightM :: List c => CursorG n c tag text -> ItemM c (Maybe (n c tag text, CursorG n c tag text))
removeRightM loc = do
    li <- runList (rights loc)
    return $ case li of
       Cons l ls -> Just $ (l,loc { rights = ls })
       Nil       -> Nothing

-- | Insert content to the left of the current position.
-- The new content becomes the current position.
insertGoLeft :: List c => n c tag text -> CursorG n c tag text -> CursorG n c tag text
insertGoLeft t loc = loc { current = t, rights = current loc `cons` rights loc }

-- | Insert content to the right of the current position.
-- The new content becomes the current position.
insertGoRight :: List c => n c tag text -> CursorG n c tag text -> CursorG n c tag text
insertGoRight t loc = loc { current = t, lefts = current loc `cons` lefts loc }

-- | Remove the current element.
-- The new position is the one on the left. Pure version.
removeGoLeft :: CursorG n [] tag text -> Maybe (CursorG n [] tag text)
removeGoLeft loc = case lefts loc of
                     l : ls -> Just loc { current = l, lefts = ls }
                     []     -> Nothing

-- | Remove the current element.
-- The new position is the one on the left. Pure version.
removeGoLeftM :: List c => CursorG n c tag text -> ItemM c (Maybe (CursorG n c tag text))
removeGoLeftM loc = do
    li <- runList (lefts loc)
    return $ case li of
        Cons l ls -> Just loc { current = l, lefts = ls }
        Nil       -> Nothing

-- | Remove the current element.
-- The new position is the one on the right. Pure version.
removeGoRight :: CursorG n [] tag text -> Maybe (CursorG n [] tag text)
removeGoRight loc = runIdentity $ removeGoRightM loc

-- | Remove the current element.
-- The new position is the one on the right. Used for monadic node types.
removeGoRightM :: List c => CursorG n c tag text -> ItemM c (Maybe (CursorG n c tag text))
removeGoRightM loc = do
     li <- runList (rights loc)
     return $ case li of
         Cons l ls -> Just loc { current = l, rights = ls }
         Nil       -> Nothing

-- | Remove the current element.
-- The new position is the parent of the old position.
removeGoUp :: MkElementClass n c => CursorG n c tag text -> Maybe (CursorG n c tag text)
removeGoUp loc =
    case (parents loc) of
        [] -> Nothing
        (pls, v, prs):ps -> Just $
            Cur { current = fromTag v (reverseL (lefts loc) `mplus` rights loc)
                , lefts = pls, rights = prs, parents = ps
                }


-- | private: Gets the given element of a list.
-- Also returns the preceding elements (reversed) and the following elements.
splitChildrenM :: List c => c a -> Int -> ItemM c (Maybe (c a,a,c a))
splitChildrenM _ n | n < 0 = return Nothing
splitChildrenM cs pos = loop mzero cs pos
  where
    loop acc l n = do
        li <- runList l
        case li of
            Nil -> return Nothing
            Cons x l' -> if n == 0
                then return $ Just (acc, x, l')
                else loop (cons x acc) l' $! n-1

-- | private: combChildren ls x ys = reverse ls ++ [x] ++ rs
combChildren :: List c =>
                c a    -- ^ ls
             -> a      -- ^ x
             -> c a    -- ^ rs
             -> c a
combChildren ls t rs = joinL $ foldlL (flip cons) (cons t rs) ls

reverseL :: List c => c a -> c a
reverseL = joinL . foldlL (flip cons) mzero

