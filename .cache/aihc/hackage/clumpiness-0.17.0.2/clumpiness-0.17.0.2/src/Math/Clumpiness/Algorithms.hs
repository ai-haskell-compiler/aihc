-- Algorithms
-- By Gregory W. Schwartz

{-# LANGUAGE BangPatterns #-}

module Math.Clumpiness.Algorithms where

-- Built-in
import Data.Maybe
import Data.List
import Data.Tree
import Control.Applicative
import Data.Ratio
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Function (on)

-- Cabal
import Math.TreeFun.Tree
import Math.TreeFun.Types

-- Local
import Math.Clumpiness.Types

-- | Get the geometric average of a list
geomAvg :: [Double] -> Double
geomAvg xs = product xs ** (1 / genericLength xs)

-- | Weigh the nodes by what weight they have (based on the product of the
-- number of children their parents have, see leavesParentMult in tree-fun)
-- and invert it
weigh :: Double -> Double
weigh x = 1 / x

-- | Only look at these properties, if they aren't both in the list
-- (or if p1 == p2 and the length is 1), then ignore it
relevantList :: (Eq a) => a -> a -> [(a, Int)] -> [Int]
relevantList p1 p2 l
    | p1 `elem` map fst relevantNodes && p2 `elem` map fst relevantNodes
   && (length relevantNodes > 1)               = map snd relevantNodes
    | otherwise                                = []
  where
    relevantNodes = filter (\x -> fst x `elem` [p1, p2]) l

-- | Only look at these properties, if they aren't both in the list
-- (or if p1 == p2 and the length is 1), then ignore it, Map version.
-- Ignore nodes not in propertyMap
relevantMap :: (Ord a, Ord b)
            => b
            -> b
            -> PropertyMap a b
            -> M.Map a c
            -> M.Map a c
relevantMap p1 p2 propertyMap lm
    | Set.member p1 relevantProperties && Set.member p2 relevantProperties
   && (M.size relevantNodes > 1) = relevantNodes
    | otherwise                  = M.empty
  where
    relevantNodes = M.filterWithKey ( \k _ -> maybeToBool
                                            . fmap (F.any (`elem` [p1, p2]))
                                            $ property k ) lm
    relevantProperties   = Set.fromList
                         . F.toList
                         . F.foldl' (Seq.><) Seq.empty
                         . map fromJust
                         . filter isJust
                         . map property
                         . M.keys
                         $ lm
    maybeToBool Nothing  = False
    maybeToBool (Just x) = x
    property x           = M.lookup x propertyMap

-- | Only look at these properties, if they aren't both in the list
-- (or if p1 == p2 and the length is 1 meaning that there is no other leaf
-- of any other type), then ignore it.
-- Ignore nodes not in propertyMap
relevantMapSame :: (Ord a, Ord b)
                => b
                -> PropertyMap a b
                -> M.Map a c
                -> M.Map a c
relevantMapSame p1 propertyMap lm
    | Set.member p1 relevantProperties
   && (not . Set.null . Set.filter (/= p1) $ relevantProperties) = lm
    | otherwise                                                  = M.empty
  where
    relevantProperties   = Set.fromList
                         . F.toList
                         . F.foldl' (Seq.><) Seq.empty
                         . map fromJust
                         . filter isJust
                         . map property
                         . M.keys
                         $ lm
    property x         = M.lookup x $ propertyMap

-- | Get the clumpiness of a single node. Ignore the root node. Only count p1 ==
-- p2 case when there are at least one set of neighboring leaves in order to
-- account for the extreme cases (complete mixture, complete separation of
-- properties) which are throwing off the p1 == p2 case. So explicitly calculate
-- cases where the number of descendent leaves is 2. Ignore nodes not in
-- propertyMap
getNodeClumpiness :: (Ord a, Ord b)
                  => b
                  -> b
                  -> PropertyMap a b
                  -> Tree (SuperNode a)
                  -> Double
getNodeClumpiness _ _ _ (Node {rootLabel = SuperNode {myParent = SuperRoot}})
    = 0
getNodeClumpiness p1 p2 propertyMap n
    = sum
    . map (weigh . fst . snd)
    . M.toAscList
    . getRelevant (p1 == p2)
    . M.mapKeys myRootLabel
    . leavesParentMult 1 0
    $ n
  where
    getRelevant True  = relevantMapSame
                        p1
                        propertyMap
    getRelevant False = relevantMap p1 p2 propertyMap

-- | Get the clumpiness metric (before sample size correction)
getPropertyClumpiness :: (Ord a, Ord b)
                      => b
                      -> b
                      -> PropertyMap a b
                      -> Tree (SuperNode a)
                      -> Double
getPropertyClumpiness _ _ _ (Node { subForest = [] }) = 0
getPropertyClumpiness p1 p2 propertyMap n@(Node { subForest = xs })
    = sum $ getNodeClumpiness p1 p2 propertyMap n : rest
  where
    rest = map (getPropertyClumpiness p1 p2 propertyMap) xs

-- | Get the heatmap for the clumping metric, how "clumped together" the
-- properties are. Found by counting the parents whose descendent leaves are of
-- those properties. They are weighted by how far away those leaves are.
-- Remove any unwanted properties by having the "viable" function take in
-- a property and return if it is viable or not. The PropertyMap should
-- have all of the vertices within the tree but no more --- any additional
-- and the clumpiness changes as the sample sizes change --- so this
-- property has not been tested and so it may lead to new ways of viewing
-- clumpiness.
generateClumpMap :: (Ord a, Ord b)
                 => (b -> Bool)
                 -> PropertyMap a b
                 -> Tree (SuperNode a)
                 -> ClumpList b
generateClumpMap viable originalPropertyMap originalTree =
    map getRelationship propertyCompareList
  where
    propertyCompareList = (\ !p1 !p2 -> (p1, p2))
                      <$> propertyList
                      <*> propertyList
    getRelationship (!p1, !p2) = divResult clump p1 p2
    getRelationship _          = error "Unsupported metric"
    divResult f p1 p2 =
        if p1 == p2
            then
                ( p1, p2, 1 - ( (geomAvg [divWeight False p1 p2 f p1, divWeight True p1 p2 f p2])
                              / numProperties ) )
            else
                ( p1, p2, (geomAvg [divWeight False p1 p2 f p1, divWeight False p1 p2 f p2])
                          / numProperties )
    -- If we have no leaves of that property than the value is 0 (0 if it's
    -- the same and they are all the relevant property, 1 otherwise so it
    -- becomes the opposite in the final calculation). If all leaves are of
    -- a single property and p1 /= p2 than the value is also 0. If there is
    -- only 1 leaf and p1 == p2 then we want 0 (opposite) to maximize the
    -- clumpiness.
    divWeight True p1 p2 f p = trivialCheck True p
                             $ (f p1 p2 * fromRational (1 % numInner'))
                             * fromRational (numLeaves' % numNotPLeavesF p)
    divWeight False p1 p2 f p = trivialCheck False p
                              $ (f p1 p2 * fromRational (1 % numInner'))
                              * fromRational (numLeaves' % numPLeavesF p)
    trivialCheck True p f  = if numNotPLeavesF p > 0 && numPLeavesF p > 1
                                 then if numNotPLeavesF p < numLeaves'
                                      && numInner' > 0
                                      && numLeaves' > 0
                                          then f
                                          else 1
                                 else 0
    trivialCheck False p f = if numPLeavesF p > 0
                             && numInner' > 0
                             && numLeaves' > 0
                                then f
                                else 0
    clump p1 p2          = getPropertyClumpiness p1 p2 propertyMap tree
    -- Number of leaves that meet a certain criteria
    numPLeavesF p        = numPLeaves (F.elem p)
    numNotPLeavesF p     = numPLeaves (not . Seq.null . Seq.filter (/= p))
    numPLeaves f         = fromIntegral
                         . M.size
                         . M.filter f
                         $ propertyMap
    property x           = fromMaybe Seq.empty $ M.lookup x propertyMap
    propertyList         = filter viable . getProperties $ propertyMap
    -- The number of properties being compared here
    numProperties        = 2 --genericLength . nub $ propertyList
    numLeaves'           = numLeaves tree
    numInner'            = numInner tree - 1 -- We don't count the root
    -- Remove root leaves from the tree and propertyMap
    propertyMap          = M.filterWithKey
                           (\k _ -> not . Set.member k $ rootLeaves)
                           originalPropertyMap
    rootLeaves           = Set.fromList
                         . map myRootLabel
                         . getRootLeaves
                         $ originalTree
    tree                 = filterRootLeaves originalTree
