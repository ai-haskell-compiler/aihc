{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Phylogeny
-- Description :  Phylogenetic trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 16:08:54 2019.
--
-- The purpose of this module is to facilitate usage of 'Tree's in phylogenetic
-- analyses. A /phylogeny/ is a 'Tree' with unique leaf labels, and unordered
-- sub-forest.
--
-- Using the 'Tree' data type has some disadvantages.
--
-- 1. All trees are rooted. Unrooted trees can be treated with a rooted data
-- structure, as it is used here. However, some functions may be meaningless.
--
-- 2. Changing branch labels, node labels, or the topology of the tree is slow,
-- especially when the changes are close to the leaves of the tree.
--
-- 3. Internally, the underlying 'Tree' data structure stores the sub-forest as
-- an ordered list. Hence, we have to do some tricks when comparing phylogenies
-- (see 'equal'), and comparison is slow.
--
-- 4. Uniqueness of the leaves is not ensured by the data type, but has to be
-- checked at runtime. Functions relying on the tree to have unique leaves do
-- perform this check, and return 'Left' with a message, if the tree has
-- duplicate leaves.
--
-- NOTE: 'Tree's are rooted.
--
-- NOTE: 'Tree's encoded in Newick format correspond to rooted trees. By
-- convention only, a tree parsed from Newick format is usually thought to be
-- unrooted, when the root node is multifurcating and has three or more
-- children. This convention is not used here. Newick trees are just parsed as
-- they are, and a rooted tree is returned.
module ELynx.Tree.Phylogeny
  ( -- * Functions
    equal,
    equal',
    intersect,
    bifurcating,
    outgroup,
    midpoint,
    roots,

    -- * Branch labels
    Phylo (..),
    toPhyloLabel,
    toPhyloTree,
    lengthToPhyloLabel,
    lengthToPhyloTree,
    supportToPhyloLabel,
    supportToPhyloTree,
    toLengthTree,
    toSupportTree,

    -- * Explicit branch labels
    PhyloExplicit (..),
    toExplicitTree,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Bifunctor
import Data.Default.Class
import Data.List hiding (intersect)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import ELynx.Tree.Bipartition
import ELynx.Tree.Length
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import ELynx.Tree.Support
import GHC.Generics

-- | The equality check is slow because the order of children is considered to
-- be arbitrary.
--
-- Return 'Left' if a tree does not have unique leaves.
equal :: (Eq e, Eq a, Ord a) => Tree e a -> Tree e a -> Either String Bool
equal tL tR
  | duplicateLeaves tL = Left "equal: Left tree has duplicate leaves."
  | duplicateLeaves tR = Left "equal: Right tree has duplicate leaves."
  | otherwise = Right $ equal' tL tR

-- | Same as 'equal', but assume that leaves are unique.
equal' :: (Eq e, Eq a) => Tree e a -> Tree e a -> Bool
equal' ~(Node brL lbL tsL) ~(Node brR lbR tsR) =
  (brL == brR)
    && (lbL == lbR)
    && (length tsL == length tsR)
    && all (`elem'` tsR) tsL
  where
    elem' t ts = isJust $ find (equal' t) ts

-- | Intersection of trees.
--
-- The intersections are the largest subtrees sharing the same leaf set.
--
-- Degree two nodes are pruned with 'prune'.
--
-- Return 'Left' if:
--
-- - the intersection of leaves is empty.
intersect ::
  (Semigroup e, Eq e, Ord a) => Forest e a -> Either String (Forest e a)
intersect ts
  | S.null lvsCommon = Left "intersect: Intersection of leaves is empty."
  | otherwise = case sequence [dropLeavesWith (predicate ls) t | (ls, t) <- zip leavesToDrop ts] of
      Nothing -> Left "intersect: A tree is empty."
      Just ts' -> Right ts'
  where
    -- Leaf sets.
    lvss = map (S.fromList . leaves) ts
    -- Common leaf set.
    lvsCommon = foldl1' S.intersection lvss
    -- Leaves to drop for each tree in the forest.
    leavesToDrop = map (S.\\ lvsCommon) lvss
    -- Predicate.
    predicate lvsToDr l = l `S.member` lvsToDr

-- | Check if tree is bifurcating.
--
-- A Bifurcating tree only contains degree one (leaves) and degree three nodes
-- (internal bifurcating nodes).
bifurcating :: Tree e a -> Bool
bifurcating (Node _ _ []) = True
bifurcating (Node _ _ [x, y]) = bifurcating x && bifurcating y
bifurcating _ = False

-- | Root tree using an outgroup.
--
-- If the root note is bifurcating, the root node is moved to the position
-- specified by the outgroup.
--
-- If the root node is multifurcating, a new root node is introduced using the
-- 'Default' instance of the node labels. Thereby, the degree of the original
-- root node is reduced by one.
--
-- Branches are connected and split according to the provided 'Semigroup' and
-- 'Splittable' instances.
--
-- Return 'Left' if
--
-- - the root node is a leaf;
--
-- - the root node has degree two;
--
-- - the tree has duplicate leaves;
--
-- - the provided outgroup is polyphyletic or not found on the tree.
outgroup ::
  (Semigroup e, Splittable e, Default a, Ord a) =>
  Set a ->
  Tree e a ->
  Either String (Tree e a)
outgroup _ (Node _ _ []) = Left "outgroup: Root node is a leaf."
outgroup _ (Node _ _ [_]) = Left "outgroup: Root node has degree two."
outgroup o t = do
  bip <- bp o (S.fromList (leaves t) S.\\ o)
  rootAt bip t

-- Root the tree at the branch defined by the given bipartition. The original
-- root node is moved to the new position.
rootAt ::
  (Semigroup e, Splittable e, Eq a, Default a, Ord a) =>
  Bipartition a ->
  Tree e a ->
  Either String (Tree e a)
rootAt b t
  -- Do not use 'duplicateLeaves' here, because we also need to compare the leaf
  -- set with the bipartition.
  | length lvLst /= S.size lvSet = Left "rootAt: Tree has duplicate leaves."
  | toSet b /= lvSet = Left "rootAt: Bipartition does not match leaves of tree."
  | otherwise = do
      ts <- roots t
      case find (\x -> bipartition x == Right b) ts of
        Nothing -> Left "rootAt': Bipartition not found on tree."
        Just t' -> Right t'
  where
    lvLst = leaves t
    lvSet = S.fromList $ leaves t

-- NOTE: The 'midpoint' algorithm has not been optimized. All rooted trees are
-- calculated and then the one minimizing the difference between the heights of
-- the left and right sub tree is chosen. Better: Move left or right minimizing
-- the height difference between the left and right sub tree.

-- | Root tree at midpoint.
--
-- Branches are connected and split according to the provided 'Semigroup' and
-- 'Splittable' instances.
--
-- Return 'Left' if
--
-- - the root node is a leaf;
--
-- - the root node has degree two.
midpoint ::
  (Semigroup e, Splittable e, HasLength e, Default a) =>
  Tree e a ->
  Either String (Tree e a)
midpoint (Node _ _ []) = Left "midpoint: Root node is a leaf."
midpoint (Node _ _ [_]) = Left "midpoint: Root node has degree two."
midpoint t = roots t >>= getMidpoint

-- Find the index of the smallest element.
findMinIndex :: Ord a => [a] -> Either String Int
findMinIndex (x : xs) = go (0, x) 1 xs
  where
    go (i, _) _ [] = Right i
    -- Indices with respect to original list: i is index of z, j is index of y.
    go (i, z) j (y : ys) = if z < y then go (i, z) (j + 1) ys else go (j, y) (j + 1) ys
findMinIndex [] = Left "findMinIndex: Empty list."

getMidpoint :: HasLength e => [Tree e a] -> Either String (Tree e a)
getMidpoint ts = case t of
  Right (Node br lb [l, r]) ->
    let hl = height l
        hr = height r
        dh = (hl - hr) / 2
     in Right $
          Node
            br
            lb
            [ modifyStem (modifyLength (subtract' dh)) l,
              modifyStem (modifyLength (+ dh)) r
            ]
  Right _ -> error "getMidpoint: Root node is not bifurcating?"
  Left e -> Left e
  where
    dhs = map getDeltaHeight ts
    -- Find index of minimum. Take this tree and move root to the midpoint of
    -- the branch.
    t = (ts !!) <$> findMinIndex dhs
    -- Subtract, and check that larger equal 0 with a precision close to the
    -- machine precision of roughly 1e-16.
    subtract' dx x =
      let x' = subtract dx x
       in case compare x' 0 of
            LT -> if x' < 1e-14 then error "getMidpoint: Length less than zero." else 0
            _ -> x'

-- Get delta height of left and right sub tree.
getDeltaHeight :: HasLength e => Tree e a -> Length
getDeltaHeight (Node _ _ [l, r]) = abs $ height l - height r
getDeltaHeight _ = error "getDeltaHeight: Root node is not bifurcating?"

-- | Get all rooted trees with bifurcating root nodes.
--
-- If the root node of the original tree is bifurcating, the root node (label
-- and branch) is moved, and the original tree is part of the result.
--
-- If the root node of the original tree is multifurcating, a new root node is
-- introduced using the 'Default' instance of the node labels. Thereby, the
-- degree of the original root node is reduced by one. The original,
-- multifurcating tree is not part of the result.
--
-- Branches are connected and split according to the provided 'Semigroup' and
-- 'Splittable' instances.
--
-- For a tree with @n@ nodes we have:
--
-- - @n-2@ rooted trees if the root node is bifurcating;
--
-- - (n-1) rooted trees if the root node is multifurcating.
roots :: (Semigroup e, Splittable e, Default a) => Tree e a -> Either String (Forest e a)
roots (Node _ _ []) = Left "roots: Root node is a leaf."
roots (Node _ _ [_]) = Left "roots: Root node has degree two."
roots t@(Node b c [tL, tR]) = Right $ t : descend b c tR tL ++ descend b c tL tR
roots (Node b c ts) = roots $ Node b def [tL, tR]
  where
    (Node bL lL tsL) = head ts
    bL' = split bL
    tL = Node bL' lL tsL
    tR = Node bL' c $ tail ts

complementaryForests :: Tree e a -> Forest e a -> [Forest e a]
complementaryForests t ts = [t : take i ts ++ drop (i + 1) ts | i <- [0 .. (n - 1)]]
  where
    n = length ts

-- Descend into the downward tree.
--
-- @
-- descend rootBranch rootLabel complementaryTree downwardsTree
-- @
descend :: (Semigroup e, Splittable e) => e -> a -> Tree e a -> Tree e a -> Forest e a
descend _ _ _ (Node _ _ []) = []
descend brR lbR tC (Node brD lbD tsD) =
  [ Node brR lbR [Node (split brDd) lbD f, Node (split brDd) lbDd tsDd]
    | (Node brDd lbDd tsDd, f) <- zip tsD cfs
  ]
    ++ concat
      [ descend brR lbR (Node (split brDd) lbD f) (Node (split brDd) lbDd tsDd)
        | (Node brDd lbDd tsDd, f) <- zip tsD cfs
      ]
  where
    brC' = branch tC <> brD
    tC' = tC {branch = brC'}
    cfs = complementaryForests tC' tsD

-- | Branch label for phylogenetic trees.
--
-- Branches may have a length and a support value.
--
-- Especially useful to export trees to Newick format; see
-- 'ELynx.Tree.Export.Newick.toNewick'.
data Phylo = Phylo
  { pBranchLength :: Maybe Length,
    pBranchSupport :: Maybe Support
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData)

instance Semigroup Phylo where
  Phylo mBL mSL <> Phylo mBR mSR =
    Phylo
      (getSum <$> (Sum <$> mBL) <> (Sum <$> mBR))
      (getMin <$> (Min <$> mSL) <> (Min <$> mSR))

instance HasMaybeLength Phylo where
  getMaybeLength = pBranchLength

instance HasMaybeSupport Phylo where
  getMaybeSupport = pBranchSupport

instance ToJSON Phylo

instance FromJSON Phylo

-- | Set branch length and support value.
toPhyloLabel :: (HasMaybeLength e, HasMaybeSupport e) => e -> Phylo
toPhyloLabel x = Phylo (getMaybeLength x) (getMaybeSupport x)

-- | See 'toPhyloLabel'.
toPhyloTree :: (HasMaybeLength e, HasMaybeSupport e) => Tree e a -> Tree Phylo a
toPhyloTree = first toPhyloLabel

-- | Set branch length. Do not set support value.
lengthToPhyloLabel :: HasMaybeLength e => e -> Phylo
lengthToPhyloLabel x = Phylo (getMaybeLength x) Nothing

-- | See 'lengthToPhyloLabel'.
lengthToPhyloTree :: HasMaybeLength e => Tree e a -> Tree Phylo a
lengthToPhyloTree = first lengthToPhyloLabel

-- | Set support value. Do not set branch length.
supportToPhyloLabel :: HasMaybeSupport e => e -> Phylo
supportToPhyloLabel x = Phylo Nothing (getMaybeSupport x)

-- | See 'supportToPhyloLabel'.
supportToPhyloTree :: HasMaybeSupport e => Tree e a -> Tree Phylo a
supportToPhyloTree = first supportToPhyloLabel

fromMaybeWithError :: String -> Maybe a -> Either String a
fromMaybeWithError s = maybe (Left s) Right

-- | If root branch length is not available, set it to 0.
--
-- Return 'Left' if any other branch length is unavailable.
toLengthTree :: HasMaybeLength e => Tree e a -> Either String (Tree Length a)
toLengthTree (Node br lb ts) =
  case traverse go ts of
    Nothing -> Left "toLengthTree: Length unavailable for some branches."
    Just ts' -> Right $ Node br' lb ts'
  where
    br' = fromMaybe 0 $ getMaybeLength br
    go t = getBranchTree <$> traverse getMaybeLength (BranchTree t)

-- | Set branch support values of branches leading to the leaves and of the root
-- branch to maximum support.
--
-- Return 'Left' if any other branch has no available support value.
toSupportTree :: HasMaybeSupport e => Tree e a -> Either String (Tree Support a)
toSupportTree t@(Node br lb ts) =
  fromMaybeWithError "toSupportTree: Support value unavailable for some branches." $
    getBranchTree <$> sequenceA (BranchTree (Node br' lb $ map go ts))
  where
    m = getMaxSupport t
    br' = cleanSupportWith m br
    go (Node b l []) = Node (cleanSupportWith m b) l []
    go (Node b l xs) = Node (getMaybeSupport b) l (map go xs)

-- If all branch support values are below 1.0, set the max support to 1.0.
getMaxSupport :: HasMaybeSupport e => Tree e a -> Support
getMaxSupport = fromJust . max (Just 1.0) . maximum . fmap getMaybeSupport . ZipBranchTree

cleanSupportWith :: HasMaybeSupport e => Support -> e -> Maybe Support
cleanSupportWith m x = case getMaybeSupport x of
  Nothing -> Just m
  Just y -> Just y

-- | Explicit branch label with branch length and branch support value.
data PhyloExplicit = PhyloExplicit
  { eBranchLength :: Length,
    eBranchSupport :: Support
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance Semigroup PhyloExplicit where
  PhyloExplicit bL sL <> PhyloExplicit bR sR = PhyloExplicit (bL + bR) (min sL sR)

instance HasMaybeLength PhyloExplicit where
  getMaybeLength = Just . eBranchLength

instance HasLength PhyloExplicit where
  getLength = eBranchLength
  setLength b pl = pl {eBranchLength = b}
  modifyLength f (PhyloExplicit l s) = PhyloExplicit (f l) s

instance Splittable PhyloExplicit where
  split l = l {eBranchLength = b'}
    where
      b' = eBranchLength l / 2.0

instance HasMaybeSupport PhyloExplicit where
  getMaybeSupport = Just . eBranchSupport

instance HasSupport PhyloExplicit where
  getSupport = eBranchSupport
  setSupport s pl = pl {eBranchSupport = s}
  modifySupport f (PhyloExplicit l s) = PhyloExplicit l (f s)

instance ToJSON PhyloExplicit

instance FromJSON PhyloExplicit

-- | Conversion to a 'PhyloExplicit' tree.
--
-- See 'toLengthTree' and 'toSupportTree'.
toExplicitTree ::
  (HasMaybeLength e, HasMaybeSupport e) =>
  Tree e a ->
  Either String (Tree PhyloExplicit a)
toExplicitTree t = do
  lt <- toLengthTree t
  st <- toSupportTree t
  case zipTreesWith PhyloExplicit const lt st of
    -- Explicit use of error, since this case should never happen.
    Nothing -> error "toExplicitTree: Can not zip two trees with different topologies."
    Just zt -> return zt
