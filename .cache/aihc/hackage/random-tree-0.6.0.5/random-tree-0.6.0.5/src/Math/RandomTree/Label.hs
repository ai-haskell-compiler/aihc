-- Label module
-- By Gregory W. Schwartz

-- | Collects all functions pertaining to the labeling of a tree

module Math.RandomTree.Label where

-- Built-in
import Data.List
import Data.Maybe
import Data.Tree
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

-- Cabal
import Control.Monad.Random
import System.Random.Shuffle
import Math.TreeFun.Types

-- | Modify the label of a tree
modifyLabel :: (Eq a) => a -> a -> a -> a
modifyLabel old new n = if n == old
                            then new
                            else n

-- | Get the neighbors of a label in a fast, theoretically efficient way
getNeighbors :: (Ord a) => Int -> a -> Tree (SuperNode a) -> [a]
getNeighbors neighborDistance l ( Node { rootLabel = SuperRoot
                                       , subForest = ts } ) =
    getNeighbors neighborDistance l
  . head
  . filter (M.member l . myLeaves . rootLabel)
  $ ts
getNeighbors neighborDistance l ( Node { rootLabel = SuperNode { myRootLabel = _
                                                               , myParent = p
                                                               , myLeaves = ls }
                                       , subForest = ts } )
    | M.size ls == neighborDistance && relevant =
        map fst . M.toAscList $ ls
    | M.size ls > neighborDistance && relevant  =
        getNeighbors neighborDistance l
      . head
      . filter (M.member l . myLeaves . rootLabel)
      $ ts
    | M.size ls < neighborDistance && relevant && p == SuperRoot =
        take neighborDistance
      . (:) l
      . filter (/= l)
      . map fst
      . M.toAscList
      $ ls -- Don't want the non-existant parent--I've reached the root here
    | M.size ls < neighborDistance && relevant  =
        take neighborDistance
      . (:) l
      . filter (/= l)
      . M.keys
      . myLeaves
      $ p
    | otherwise                                 = []
  where
    relevant = M.member l ls

-- | Assign clumps to the label list. Takes an old label and reassigns it to the
-- new label in the labelmap, but looks at all neighbors defined by the
-- distanceMap and the neighborDistance. If the reassigned nodes have already
-- been reassigned (in the propertyList), then ignore.
clumpIt :: (Ord a, Eq b)
        => Int
        -> Tree (SuperNode a)
        -> a
        -> b
        -> MaybePropertyMap a b
        -> MaybePropertyMap a b
clumpIt neighborDistance tree pointer property propertyMap =
    F.foldl' (\acc x -> updateMap x property acc) propertyMap
  $ neighbors pointer
  where
    updateMap k p = M.adjust
                    --(\_ -> Just p)
                    (\x -> if isNothing x then Just (Seq.singleton p) else x)
                    k
    neighbors x   = getNeighbors neighborDistance x tree

-- | Assign random labels to the leaves of a tree in a clumped fashion
assignRandomClumpedProperties :: (Ord a, Eq b)
                              => [b]
                              -> Int
                              -> Tree (SuperNode a)
                              -> StdGen
                              -> MaybePropertyMap a b
                              -> MaybePropertyMap a b
assignRandomClumpedProperties propertyList neighborDistance tree g propertyMap =
    foldl' ( \acc (x, y)
          -> clumpIt neighborDistance tree x y acc)
    propertyMap
  . zip shuffledLeaves
  $ propertyList
  where
    shuffledLeaves = shuffle' (M.keys propertyMap) (M.size propertyMap) g

-- | Assign random labels to the leaves of a tree
assignRandomProperties :: (Ord a)
                       => [b]
                       -> StdGen
                       -> MaybePropertyMap a b
                       -> MaybePropertyMap a b
assignRandomProperties propertyList g propertyMap = M.fromList
                                                  . zip shuffledLeaves
                                                  . map (Just . Seq.singleton)
                                                  $ propertyList
  where
    shuffledLeaves = shuffle' (M.keys propertyMap) (M.size propertyMap) g

-- | Return the empty propertyMap
emptyPropertyMap :: (Ord a) => [a] -> MaybePropertyMap a b
emptyPropertyMap x = M.fromList . zip x . repeat $ Nothing

-- | Return the propertyMap
getPropertyMap :: (Ord a) => [a] -> PropertyMap a a
getPropertyMap x = M.fromList . zip x . map Seq.singleton $ x
