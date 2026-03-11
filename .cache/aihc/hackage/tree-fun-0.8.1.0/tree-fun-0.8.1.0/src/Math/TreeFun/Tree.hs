-- Tree module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to trees

{-# LANGUAGE BangPatterns #-}

module Math.TreeFun.Tree where

-- Built-in
import Data.List
import Data.Tree
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import Control.Applicative
import qualified Data.Foldable as F
import Control.Monad.State

-- Local
import Math.TreeFun.Types

-- | Convert a bool to an integer
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- | Find out if a node is a leaf or not
isLeaf :: Tree a -> Bool
isLeaf (Node { subForest = [] }) = True
isLeaf _                         = False

-- | Return the labels of the leaves of the tree
leaves :: Tree a -> [a]
leaves (Node { rootLabel = x, subForest = [] }) = [x]
leaves (Node { rootLabel = _, subForest = xs }) = concatMap leaves xs

-- | Return the labels of the leaves of the tree with their relative heights
-- from the root (the input number you give determines how many steps away the
-- leaves are, should almost always start at 0)
leavesHeight :: (Ord a) => Int -> Tree a -> M.Map a Int
leavesHeight !h (Node { rootLabel = x, subForest = [] }) = M.singleton x h
leavesHeight !h (Node { rootLabel = _, subForest = xs }) =
    M.unions . map (leavesHeight (h + 1)) $ xs

-- | Return the labels of the leaves of the tree with their relative heights
-- from the root (the input number you give determines how many steps away the
-- leaves are, should almost always start at 0). Also, here we give leaves that
-- share a parent a separate label.
leavesCommonHeight :: (Ord a) => Int -> Tree a -> M.Map a (Int, Int)
leavesCommonHeight startHeight tree = evalState (iter startHeight tree) 0
  where
    iter !h (Node { rootLabel = x, subForest = [] }) = do
        label <- get
        return $ M.singleton x (h, label)
    iter !h (Node { rootLabel = _, subForest = xs }) = do
        -- Get leaves and assign them the label
        ls    <- mapM (iter (h + 1)) . filter isLeaf $ xs

        -- Increment label
        label <- get
        put $ label + 1

        -- Get rest of the trees
        ts    <- mapM (iter (h + 1)) . filter (not . isLeaf) $ xs
        -- Combine the results
        return . M.unions . (++) ts $ ls

-- | Return the labels of the leaves of the tree with their weights
-- determined by the product of the number of children of their parents all
-- the way up to the root, along with their distance. Returns Double for
-- more precision.
leavesParentMult :: (Ord a) => Double
                            -> Double
                            -> Tree a
                            -> M.Map a (Double, Double)
leavesParentMult !w !d (Node { rootLabel = x, subForest = [] }) =
    M.singleton x (w, d)
leavesParentMult !w !d (Node { rootLabel = _, subForest = xs }) =
    M.unions . map (leavesParentMult (w * genericLength xs) (d + 1)) $ xs

-- | Return the labels of the leaves of the tree with their weights
-- determined by the product of the number of children of their parents all
-- the way up to the root. Also, here we give leaves that share a parent
-- a separate label.
leavesCommonParentMult :: (Ord a) => Int -> Tree a -> M.Map a (Int, Int)
leavesCommonParentMult numChildren tree = evalState (iter numChildren tree) 0
  where
    iter multChildren (Node { rootLabel = x, subForest = [] }) = do
        label <- get
        return $ M.singleton x (multChildren, label)
    iter multChildren (Node { rootLabel = _, subForest = xs }) = do
        -- Get leaves and assign them the label
        ls    <- mapM (iter (multChildren * length xs)) . filter isLeaf $ xs

        -- Increment label
        label <- get
        put $ label + 1

        -- Get rest of the trees
        ts    <- mapM (iter (multChildren * length xs))
               . filter (not . isLeaf)
               $ xs
        -- Combine the results
        return . M.unions . (++) ts $ ls

-- | Return the labels of the leaves of the tree with their relative heights
-- from the root (the input number you give determines how many steps away the
-- leaves are, should almost always start at 0), slower version not requiring
-- Ord but no Maps
leavesHeightList :: Int -> Tree a -> [(a, Int)]
leavesHeightList h (Node { rootLabel = x, subForest = [] }) = [(x, h)]
leavesHeightList h (Node { rootLabel = _, subForest = xs }) =
    concatMap (leavesHeightList (h + 1)) xs

-- | Return the inner nodes of the tree
innerNodes :: Tree a -> [a]
innerNodes (Node { rootLabel = _, subForest = [] }) = []
innerNodes (Node { rootLabel = x, subForest = xs }) = x
                                                    : concatMap innerNodes xs

-- | Return the number of leaves in a tree
numLeaves :: (Num b) => Tree a -> b
numLeaves = genericLength . leaves

-- | Return the number of inner nodes of a tree
numInner :: (Num b) => Tree a -> b
numInner = genericLength . innerNodes

-- | Return True if a tree has a leaf connected to the root of the given
-- tree
hasRootLeaf :: Tree a -> Bool
hasRootLeaf (Node { subForest = ts }) = not . null . filter isLeaf $ ts

-- | Return the list of root leaves
getRootLeaves :: Tree a -> [a]
getRootLeaves (Node { subForest = ts }) = map rootLabel . filter isLeaf $ ts

-- | Return the list of properties in a property map for a tree
getProperties :: (Eq b) => PropertyMap a b -> [b]
getProperties = nub . F.toList . F.foldl' (S.><) S.empty . M.elems

-- | Remove leaves from a tree
filterLeaves :: Tree a -> Tree a
filterLeaves tree = tree {subForest = filter (not . isLeaf) . subForest $ tree}

-- | Remove leaves attached to the root of the tree
filterRootLeaves :: Tree a -> Tree a
filterRootLeaves root@(Node { subForest = ts }) =
    root { subForest = filter (not . isLeaf) ts }

-- | Return the map of distances from each leaf to another leaf
getDistanceMap :: (Eq a, Ord a) => Tree a -> DistanceMap a
getDistanceMap tree = M.fromListWith (M.unionWith (S.><))
                    $ (\x y -> if x == y
                                   then (x, M.singleton 0 (S.singleton y))
                                   else ( x
                                        , M.singleton
                                          (getDistance tree x y)
                                          (S.singleton y) ) )
                  <$> leaves tree
                  <*> leaves tree

-- | Find the distance between two leaves in a tree.
getDistance :: (Eq a) => Tree a -> a -> a -> Int
getDistance (Node { rootLabel = l, subForest = [] }) x y = boolToInt
                                                         $ l `elem` [x, y]
getDistance n@(Node { rootLabel = _, subForest = xs }) x y
    | none      = 0
    | otherwise = sum
                . (:) (boolToInt notShared)
                . map (\t -> getDistance t x y)
                $ xs
  where
    -- Only count nodes that have one or the other, not shared or empty
    notShared = (elem x ls) || (elem y ls) && not (elem x ls && elem y ls)
      where
        ls = leaves n
    none = not (elem x ls || elem y ls)
      where
        ls = leaves n

-- | Return the map of distances from each leaf to another leaf
getDistanceMapSuperNode :: (Eq a, Ord a) => Tree (SuperNode a) -> DistanceMap a
getDistanceMapSuperNode tree = M.fromListWith (M.unionWith (S.><))
                             $ (\x y -> if x == y
                                            then
                                                (x , M.singleton 0 (S.singleton y))
                                            else ( x
                                                 , M.singleton
                                                   (getDistanceSuperNode tree x y)
                                                   (S.singleton y) ) )
                           <$> allLeaves
                           <*> allLeaves
  where
    allLeaves = M.keys . myLeaves . rootLabel $ tree

-- | Find the distance between two leaves in a leafNode tree. Begin recording
-- distances when record is True (should have height starting at 0)
getDistanceSuperNode :: (Eq a, Ord a) => Tree (SuperNode a) -> a -> a -> Int
getDistanceSuperNode (Node { rootLabel = SuperNode { myLeaves = ls
                                                   , myParent = p }
                           , subForest = ts } ) x y
    | shared ls    = head
                   . filter (/= 1)
                   . map (\a -> getDistanceSuperNode a x y)
                   $ ts
    | notShared ls = getParentLeafDist x p + getParentLeafDist y p
    | otherwise    = 0
  where
    -- Only count nodes that have one or the other, not shared or empty
    notShared xs = (M.member x xs || M.member y xs)
                && not (M.member x xs && M.member y xs)
    shared xs    = M.member x xs && M.member y xs
    getParentLeafDist a b = fst . fromJust . M.lookup a . myLeaves $ b

-- | Get the sum of a tree for a tree with numbered labels
sumTree :: (Num a) => Tree a -> a
sumTree = F.foldl' (+) 0

-- | Convert a tree to the LeafNode tree data structure (the leaves are in the
-- nodes)
toSuperNodeTree :: (Ord a) => SuperNode a -> Tree a -> Tree (SuperNode a)
toSuperNodeTree p n@(Node { rootLabel = x, subForest = xs }) =
    Node { rootLabel = newNode
         , subForest = map (toSuperNodeTree newNode) xs }
  where
    newNode = SuperNode { myRootLabel = x
                        , myLeaves = leavesCommonHeight 0 n
                        , myParent = p }
