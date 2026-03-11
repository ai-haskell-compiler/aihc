{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  ELynx.Tree.Rooted
-- Description :  Rooted trees with labeled branches
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 09:57:29 2019.
--
-- Rooted 'Tree's differs from a classical rose 'Data.Tree.Tree's in that they
-- have labeled branches.
--
-- For rooted topologies, please see 'ELynx.Topology.Rooted'.
--
-- A 'Tree' is defined as:
--
-- @
-- data Tree e a = Node
--   { branch :: e,
--     label :: a,
--     forest :: Forest e a
--   }
-- @
--
-- where
--
-- @
-- type Forest e a = [Tree e a]
-- @
--
-- This means, that the word 'Node' is reserved for the constructor of a tree,
-- and that a 'Node' has an attached 'branch', a 'label', and a sub-'forest'.
-- The terms /Node/ and /label/ referring to the value constructor 'Node' and
-- the record function 'label', respectively, are not to be confused. The
-- elements of the sub-forest are often called /children/.
--
-- In mathematical terms: A 'Tree' is a directed acyclic graph without loops,
-- with vertex labels, and with edge labels.
--
-- A short recap of recursive tree traversals:
--
-- - Pre-order: Root first, then sub trees from left to right. Also called depth
--   first.
--
-- - In-order: Only valid for bifurcating trees. Left sub tree first, then root,
--   then right sub tree.
--
-- - Post-order: Sub trees from left to right, then the root. Also called
--   breadth first.
--
-- Here, pre-order traversals are used exclusively, for example, by accessor
-- functions such as 'branches', or 'labels' which is the same as 'toList'.
-- Please let me know, if post-order algorithms are required.
module ELynx.Tree.Rooted
  ( -- * Tree with branch labels
    Tree (..),
    Forest,
    fromRoseTree,
    toTreeBranchLabels,
    toTreeNodeLabels,

    -- * Access leaves, branches and labels
    leaves,
    duplicateLeaves,
    setStem,
    modifyStem,
    branches,
    setBranches,
    setLabel,
    modifyLabel,
    labels,
    duplicateLabels,
    setLabels,
    identify,

    -- * Structure
    degree,
    depth,
    prune,
    dropNodesWith,
    dropLeavesWith,
    zipTreesWith,
    zipTrees,
    flipLabels,

    -- * Newtypes with specific instances
    ZipTree (..),
    BranchTree (..),
    ZipBranchTree (..),
  )
where

import Control.Applicative
import Control.Comonad
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Data
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Tree as T
import GHC.Generics

-- | Rooted rose trees with branch labels.
--
-- Unary instances such as 'Functor' act on node labels, and not on branch
-- labels. Binary instances such as 'Bifunctor' act on both labels (`first` acts
-- on branches, `second` on node labels).
data Tree e a = Node
  { branch :: e,
    label :: a,
    forest :: Forest e a
  }
  deriving (Eq, Read, Show, Data, Generic)

-- | Shorthand.
type Forest e a = [Tree e a]

-- | Map over node labels.
instance Functor (Tree e) where
  fmap f ~(Node br lb ts) = Node br (f lb) $ map (fmap f) ts
  lb <$ ~(Node br _ ts) = Node br lb (map (lb <$) ts)

-- | The function 'first' acts on branch labels, 'second' on node labels.
instance Bifunctor Tree where
  bimap f g ~(Node br lb ts) = Node (f br) (g lb) $ map (bimap f g) ts
  first f ~(Node br lb ts) = Node (f br) lb $ map (first f) ts
  second g ~(Node br lb ts) = Node br (g lb) $ map (second g) ts

-- | Combine node labels in pre-order.
instance Foldable (Tree e) where
  foldMap f ~(Node _ lb ts) = f lb <> foldMap (foldMap f) ts
  null _ = False
  {-# INLINE null #-}
  toList = labels
  {-# INLINE toList #-}

instance Bifoldable Tree where
  bifoldMap f g ~(Node br lb ts) = f br <> g lb <> foldMap (bifoldMap f g) ts

instance Traversable (Tree e) where
  traverse g ~(Node br lb ts) = Node br <$> g lb <*> traverse (traverse g) ts

instance Bitraversable Tree where
  bitraverse f g ~(Node br lb ts) = Node <$> f br <*> g lb <*> traverse (bitraverse f g) ts

-- | The 'Semigroup' instance of the branch labels determines how the
-- branches are combined. For example, distances can be summed using
-- 'Data.Semigroup.Sum'.
--
-- The 'Monoid' instance of the branch labels determines the default branch
-- label when using 'pure'.
--
-- This instance is similar to the one provided by 'Data.Tree.Tree'. For an
-- alternative, see 'ZipTree'.
instance (Semigroup e, Monoid e) => Applicative (Tree e) where
  pure lb = Node mempty lb []
  ~(Node brF lbF tsF) <*> ~tx@(Node brX lbX tsX) =
    Node (brF <> brX) (lbF lbX) (map (bimap (brF <>) lbF) tsX ++ map (<*> tx) tsF)
  liftA2 f ~(Node brX lbX tsX) ~ty@(Node brY lbY tsY) =
    Node (brX <> brY) (f lbX lbY) (map (bimap (brX <>) (f lbX)) tsY ++ map (\tx -> liftA2 f tx ty) tsX)
  ~(Node brX _ tsX) *> ~ty@(Node brY lbY tsY) =
    Node (brX <> brY) lbY (map (first (brX <>)) tsY ++ (tsX <&> (*> ty)))
  ~(Node brX lbX tsX) <* ~ty@(Node brY _ tsY) =
    Node (brX <> brY) lbX (map (bimap (brX <>) (const lbX)) tsY ++ (tsX <&> (<* ty)))

-- | The 'Semigroup' instance of the branch labels determines how the branches
-- are combined. For example, distances can be summed using
-- 'Data.Semigroup.Sum'.
--
-- The 'Monoid' instance of the branch labels determines the default branch
-- label when using 'return'.
instance (Semigroup e, Monoid e) => Monad (Tree e) where
  ~(Node br lb ts) >>= f = case f lb of
    Node br' lb' ts' -> Node (br <> br') lb' (map (first (br <>)) ts' ++ map (>>= f) ts)

-- -- NOTE: We cannot provide a MonadZip instance because branch labels cannot
-- -- be recovered from the combined label.
--
-- instance Monoid e => MonadZip (Tree e) where
--   mzipWith f (Node brL lbL tsL) (Node brR lbR tsR) =
--     Node (brL <> brR) (f lbL lbR) (mzipWith (mzipWith f) tsL tsR)
--
--   munzip (Node br (lbL, lbR) ts) = (Node ? lbL tsL, Node ? lbR tsR)
--     where
--       (tsL, tsR) = munzip (map munzip ts)

-- -- NOTE: I don't really know much about 'MonadFix', and so do not provide the
-- -- instance.
--
-- instance Monoid e => MonadFix (Tree e) where
--   mfix = mfixTree

-- mfixTree :: (a -> Tree e a) -> Tree e a
-- mfixTree f
--   | Node br lb ts <- fix (f . label) =
--     Node
--       br
--       lb
--       ( zipWith
--           (\i _ -> mfixTree ((!! i) . forest . f))
--           [0 ..]
--           ts
--       )

instance Comonad (Tree e) where
  duplicate t@(Node br _ ts) = Node br t (map duplicate ts)
  extract = label
  {-# INLINE extract #-}

instance (NFData e, NFData a) => NFData (Tree e a) where
  rnf (Node br lb ts) = rnf br `seq` rnf lb `seq` rnf ts

instance (ToJSON e, ToJSON a) => ToJSON (Tree e a)

instance (FromJSON e, FromJSON a) => FromJSON (Tree e a)

-- | Conversion from 'T.Tree'.
fromRoseTree :: T.Tree a -> Tree () a
fromRoseTree (T.Node l ts) = Node () l $ map fromRoseTree ts

-- | Conversion to 'T.Tree' using branch labels.
toTreeBranchLabels :: Tree e a -> T.Tree e
toTreeBranchLabels (Node br _ ts) = T.Node br (map toTreeBranchLabels ts)

-- | Conversion to 'T.Tree' using node labels.
toTreeNodeLabels :: Tree e a -> T.Tree a
toTreeNodeLabels (Node _ lb ts) = T.Node lb (map toTreeNodeLabels ts)

-- | List of leaves.
leaves :: Tree e a -> [a]
leaves t = squish t []
  where
    squish (Node _ lb []) xs = lb : xs
    squish (Node _ _ ts) xs = foldr squish xs ts

duplicates :: Ord a => [a] -> Bool
duplicates = go S.empty
  where
    go _ [] = False
    go seen (x : xs) = x `S.member` seen || go (S.insert x seen) xs

-- | Check if a tree has duplicate leaves.
duplicateLeaves :: Ord a => Tree e a -> Bool
duplicateLeaves = duplicates . leaves

-- | Set the stem to a given value.
setStem :: e -> Tree e a -> Tree e a
setStem br (Node _ lb ts) = Node br lb ts

-- | Modify the stem of a tree.
modifyStem :: (e -> e) -> Tree e a -> Tree e a
modifyStem f t = t {branch = f $ branch t}

-- | Get branch labels in pre-order.
branches :: Tree e a -> [e]
branches t = squish t []
  where
    squish (Node br _ ts) xs = br : foldr squish xs ts

-- | Set branch labels in pre-order.
--
-- Return 'Nothing' if the provided list of branch labels is too short.
setBranches :: Bitraversable t => [f] -> t e a -> Maybe (t f a)
setBranches xs = bisequenceA . snd . bimapAccumL setBranch noChange xs
  where
    setBranch [] _ = ([], Nothing)
    setBranch (y : ys) _ = (ys, Just y)
    noChange ys z = (ys, Just z)

-- | Set label.
setLabel :: a -> Tree e a -> Tree e a
setLabel lb (Node br _ ts) = Node br lb ts

-- | Modify the root label of a tree.
modifyLabel :: (a -> a) -> Tree e a -> Tree e a
modifyLabel f t = t {label = f $ label t}

-- | Return node labels in pre-order.
labels :: Tree e a -> [a]
labels t = squish t []
  where
    squish (Node _ lb ts) xs = lb : foldr squish xs ts

-- | Check if a tree has duplicate labels.
duplicateLabels :: Ord a => Tree e a -> Bool
duplicateLabels = duplicates . labels

-- | Set node labels in pre-order.
--
-- Return 'Nothing' if the provided list of node labels is too short.
setLabels :: Traversable t => [b] -> t a -> Maybe (t b)
setLabels xs = sequenceA . snd . mapAccumL setLabelM xs
  where
    setLabelM [] _ = ([], Nothing)
    setLabelM (y : ys) _ = (ys, Just y)

-- | Label the nodes in pre-order with unique indices starting at 0.
identify :: Traversable t => t a -> t Int
identify = snd . mapAccumL (\i _ -> (i + 1, i)) (0 :: Int)

-- | Degree of the root node.
--
-- The degree of a node is the number of branches attached to the node.
degree :: Tree e a -> Int
degree = (+ 1) . length . forest

-- | Depth of a tree.
--
-- The [depth of a tree](https://en.wikipedia.org/wiki/Tree-depth) is the
-- largest number of nodes traversed on a path from the root to a leaf.
--
-- By convention, the depth is larger equal 1. That is, the depth of a leaf tree
-- is 1.
depth :: Tree e a -> Int
depth = maximum . go 1
  where
    go n (Node _ _ []) = [n]
    go n (Node _ _ xs) = concatMap (go (n + 1)) xs

-- | Prune degree two nodes.
--
-- The label of a pruned node is lost. The branches are combined according to
-- their 'Semigroup' instance of the form
--
-- @\daughterBranch parentBranch -> combinedBranch@.
prune :: Semigroup e => Tree e a -> Tree e a
prune t@(Node _ _ []) = t
prune (Node paBr _ [Node daBr daLb daTs]) = Node (daBr <> paBr) daLb daTs
prune (Node paBr paLb paTs) = Node paBr paLb $ map prune paTs

-- | Drop nodes satisfying predicate.
--
-- Degree two nodes may arise.
--
-- Also drop nodes of which all daughter nodes are dropped.
--
-- Return 'Nothing' if
--
-- - The root node satisfies the predicate.
--
-- - All daughter nodes of the root are dropped.
dropNodesWith :: (a -> Bool) -> Tree e a -> Maybe (Tree e a)
dropNodesWith p (Node br lb ts)
  | p lb = Nothing
  | otherwise =
      if null ts'
        then Nothing
        else Just $ Node br lb ts'
  where
    ts' = mapMaybe (dropNodesWith p) ts

-- | Drop leaves satisfying predicate.
--
-- Degree two nodes may arise.
--
-- Also drop nodes of which all daughter nodes are dropped.
--
-- Return 'Nothing' if all leaves satisfy the predicate.
dropLeavesWith :: (a -> Bool) -> Tree e a -> Maybe (Tree e a)
dropLeavesWith p (Node br lb [])
  | p lb = Nothing
  | otherwise = Just $ Node br lb []
dropLeavesWith p (Node br lb ts) =
  if null ts'
    then Nothing
    else Just $ Node br lb ts'
  where
    ts' = mapMaybe (dropLeavesWith p) ts

-- | Zip two trees with the same topology.
--
-- This function differs from the 'Applicative' instance of 'ZipTree' in that it
-- fails when the topologies don't match. Further, it allows specification of a
-- zipping function for the branches.
--
-- Return 'Nothing' if the topologies are different.
zipTreesWith ::
  (e1 -> e2 -> e) ->
  (a1 -> a2 -> a) ->
  Tree e1 a1 ->
  Tree e2 a2 ->
  Maybe (Tree e a)
zipTreesWith f g (Node brL lbL tsL) (Node brR lbR tsR) =
  if length tsL == length tsR
    then -- I am proud of that :)).
      zipWithM (zipTreesWith f g) tsL tsR >>= Just . Node (f brL brR) (g lbL lbR)
    else Nothing

-- | See 'zipTreesWith'.
zipTrees :: Tree e1 a1 -> Tree e2 a2 -> Maybe (Tree (e1, e2) (a1, a2))
zipTrees = zipTreesWith (,) (,)

-- | Flip the branch and node lables.
flipLabels :: Tree e a -> Tree a e
flipLabels (Node x y zs) = Node y x $ map flipLabels zs

-- | This newtype provides instances acting on the branch labels, and not on the
-- node labels as it is the case in 'Tree'.
newtype BranchTree a e = BranchTree {getBranchTree :: Tree e a}
  deriving (Eq, Read, Show, Data, Generic)

-- | Map over branch labels.
instance Functor (BranchTree a) where
  fmap f ~(BranchTree (Node br lb ts)) =
    BranchTree $ Node (f br) lb $ map (getBranchTree . fmap f . BranchTree) ts
  br <$ ~(BranchTree (Node _ lb ts)) =
    BranchTree $ Node br lb (map (getBranchTree . (br <$) . BranchTree) ts)

-- | Combine branch labels in pre-order.
instance Foldable (BranchTree a) where
  foldMap f ~(BranchTree (Node br _ ts)) =
    f br <> foldMap (foldMap f . BranchTree) ts
  null _ = False
  {-# INLINE null #-}
  toList = branches . getBranchTree
  {-# INLINE toList #-}

instance Traversable (BranchTree a) where
  traverse g ~(BranchTree (Node br lb ts)) =
    assemble lb <$> fbr' <*> fts'
    where
      assemble lb' br' ts' = BranchTree $ Node br' lb' ts'
      fbr' = g br
      fts' = map getBranchTree <$> traverse (traverse g . BranchTree) ts

instance Comonad (BranchTree a) where
  duplicate (BranchTree t@(Node _ lb ts)) =
    BranchTree $
      Node (BranchTree t) lb $
        map (getBranchTree . duplicate . BranchTree) ts
  extract = branch . getBranchTree

instance Monoid a => Applicative (BranchTree a) where
  -- Infinite layers with infinite subtrees.
  pure br = BranchTree $ Node br mempty []
  (BranchTree ~(Node brF lbF tsF)) <*> tx@(BranchTree ~(Node brX lbX tsX)) =
    BranchTree $
      Node
        (brF brX)
        (lbF <> lbX)
        ( map (bimap brF (lbF <>)) tsX
            ++ map (getBranchTree . (<*> tx) . BranchTree) tsF
        )
  liftA2 f (BranchTree ~(Node brX lbX tsX)) ty@(BranchTree ~(Node brY lbY tsY)) =
    BranchTree $
      Node
        (f brX brY)
        (lbX <> lbY)
        ( map (bimap (f brX) (lbX <>)) tsY
            ++ map (\tx -> getBranchTree $ liftA2 f (BranchTree tx) ty) tsX
        )
  (BranchTree ~(Node _ lbX tsX)) *> ty@(BranchTree ~(Node brY lbY tsY)) =
    BranchTree $
      Node
        brY
        (lbX <> lbY)
        ( getBranchTree
            <$> ( map (BranchTree . second (lbX <>)) tsY
                    ++ map ((*> ty) . BranchTree) tsX
                )
        )
  (BranchTree ~(Node brX lbX tsX)) <* ty@(BranchTree ~(Node _ lbY tsY)) =
    BranchTree $
      Node
        brX
        (lbX <> lbY)
        ( map (bimap (const brX) (lbX <>)) tsY
            ++ map (getBranchTree . (<* ty) . BranchTree) tsX
        )

-- | This newtype provides a zip-like applicative instance, similar to
-- 'Control.Applicative.ZipList'.
--
-- The default applicative instance of 'Tree' is not zip-like, because the
-- zip-like instance makes the Monad instance meaningless (similar to the
-- behavior observed with lists).
newtype ZipTree e a = ZipTree {getZipTree :: Tree e a}
  deriving (Eq, Read, Show, Data, Generic)

deriving instance Functor (ZipTree e)

deriving instance Foldable (ZipTree e)

instance Traversable (ZipTree e) where
  traverse f (ZipTree t) = ZipTree <$> traverse f t

instance Comonad (ZipTree e) where
  duplicate (ZipTree t) = ZipTree $ second ZipTree $ duplicate t
  extract = label . getZipTree

-- | The 'Monoid' instance of the branch labels determines the default branch
-- label, and how the branches are combined. For example, distances can be
-- summed using the 'Data.Monoid.Sum' monoid.
--
-- >>> let t = ZipTree $ Node "" 0 [Node "" 1 [], Node "" 2 []] :: ZipTree String Int
-- >>> let f = ZipTree $ Node "+3" (+3) [Node "*5" (*5) [], Node "+10" (+10) []] :: ZipTree String (Int -> Int)
-- >>> f <*> t
--
-- ZipTree {getZipTree = Node {branch = "+3", label = 3, forest = [Node {branch = "*5", label = 5, forest = []},Node {branch = "+10", label = 12, forest = []}]}}
instance Monoid e => Applicative (ZipTree e) where
  -- Infinite layers with infinite subtrees.
  pure lb = ZipTree $ Node mempty lb $ repeat (getZipTree $ pure lb)
  (ZipTree ~(Node brF lbF tsF)) <*> (ZipTree ~(Node brX lbX tsX)) =
    ZipTree $ Node (brF <> brX) (lbF lbX) (zipWith f tsF tsX)
    where
      f x y = getZipTree $ ZipTree x <*> ZipTree y
  liftA2 f (ZipTree ~(Node brX lbX tsX)) (ZipTree ~(Node brY lbY tsY)) =
    ZipTree $ Node (brX <> brY) (f lbX lbY) (zipWith g tsX tsY)
    where
      g x y = getZipTree $ liftA2 f (ZipTree x) (ZipTree y)
  (ZipTree ~(Node brX _ tsX)) *> (ZipTree ~(Node brY lbY tsY)) =
    ZipTree $ Node (brX <> brY) lbY (zipWith f tsX tsY)
    where
      f x y = getZipTree $ ZipTree x *> ZipTree y
  (ZipTree ~(Node brX lbX tsX)) <* (ZipTree ~(Node brY _ tsY)) =
    ZipTree $ Node (brX <> brY) lbX (zipWith f tsX tsY)
    where
      f x y = getZipTree $ ZipTree x <* ZipTree y

-- | Like 'ZipTree' but act on branch labels; see 'BranchTree'.
newtype ZipBranchTree a e = ZipBranchTree {getZipBranchTree :: Tree e a}
  deriving (Eq, Read, Show, Data, Generic)

-- | Map over branch labels.
instance Functor (ZipBranchTree a) where
  fmap f ~(ZipBranchTree (Node br lb ts)) =
    ZipBranchTree $ Node (f br) lb $ map g ts
    where
      g = getZipBranchTree . fmap f . ZipBranchTree
  br <$ ~(ZipBranchTree (Node _ lb ts)) =
    ZipBranchTree $ Node br lb (map f ts)
    where
      f = getZipBranchTree . (br <$) . ZipBranchTree

-- | Combine branch labels in pre-order.
instance Foldable (ZipBranchTree a) where
  foldMap f ~(ZipBranchTree (Node br _ ts)) =
    f br <> foldMap g ts
    where
      g = foldMap f . ZipBranchTree
  null _ = False
  {-# INLINE null #-}
  toList = branches . getZipBranchTree
  {-# INLINE toList #-}

instance Traversable (ZipBranchTree a) where
  traverse g ~(ZipBranchTree (Node br lb ts)) =
    assemble lb <$> fbr' <*> fts'
    where
      assemble lb' br' ts' = ZipBranchTree $ Node br' lb' ts'
      fbr' = g br
      fts' = map getZipBranchTree <$> traverse (traverse g . ZipBranchTree) ts

instance Comonad (ZipBranchTree a) where
  duplicate (ZipBranchTree t@(Node _ lb ts)) =
    ZipBranchTree $
      Node (ZipBranchTree t) lb $
        map (getZipBranchTree . duplicate . ZipBranchTree) ts
  extract = branch . getZipBranchTree

-- | See the 'Applicative' instance of 'ZipTree'.
instance Monoid a => Applicative (ZipBranchTree a) where
  -- Infinite layers with infinite subtrees.
  pure br = ZipBranchTree $ Node br mempty $ repeat (getZipBranchTree $ pure br)
  (ZipBranchTree ~(Node brF lbF tsF)) <*> (ZipBranchTree ~(Node brX lbX tsX)) =
    ZipBranchTree $ Node (brF brX) (lbF <> lbX) (zipWith f tsF tsX)
    where
      f x y = getZipBranchTree $ ZipBranchTree x <*> ZipBranchTree y
  liftA2 f (ZipBranchTree ~(Node brX lbX tsX)) (ZipBranchTree ~(Node brY lbY tsY)) =
    ZipBranchTree $ Node (f brX brY) (lbX <> lbY) (zipWith g tsX tsY)
    where
      g x y = getZipBranchTree $ liftA2 f (ZipBranchTree x) (ZipBranchTree y)
  (ZipBranchTree ~(Node _ lbX tsX)) *> (ZipBranchTree ~(Node brY lbY tsY)) =
    ZipBranchTree $ Node brY (lbX <> lbY) (zipWith f tsX tsY)
    where
      f x y = getZipBranchTree $ ZipBranchTree x *> ZipBranchTree y
  (ZipBranchTree ~(Node brX lbX tsX)) <* (ZipBranchTree ~(Node _ lbY tsY)) =
    ZipBranchTree $ Node brX (lbX <> lbY) (zipWith f tsX tsY)
    where
      f x y = getZipBranchTree $ ZipBranchTree x <* ZipBranchTree y
