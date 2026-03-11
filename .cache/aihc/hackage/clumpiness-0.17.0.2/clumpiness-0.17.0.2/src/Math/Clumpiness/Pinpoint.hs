-- Pinpoint
-- By Gregory W. Schwartz

module Math.Clumpiness.Pinpoint where

-- Built-in
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Tree
import Control.Arrow

-- Cabal
import Math.TreeFun.Tree
import Math.TreeFun.Types

-- Local
import Math.Clumpiness.Types
import Math.Clumpiness.Utilities
import Math.Clumpiness.Algorithms

-- | Filter the PropertyMap based on the tree, getting rid of extra vertices
getValidPropertyMap :: (Ord a, Ord b)
                    => Tree (SuperNode a)
                    -> PropertyMap a b
                    -> PropertyMap a b
getValidPropertyMap tree = M.filterWithKey (\k _ -> Set.member k vertices)
  where
    vertices = Set.fromList . map myRootLabel . flatten $ tree

-- | Convert a subtree first node to a root
rootSubtree :: Tree (SuperNode a) -> Tree (SuperNode a)
rootSubtree ( Node { rootLabel = SuperNode  { myRootLabel = x
                                            , myLeaves = y
                                            }
                   , subForest = xs
                   }
            ) =
    Node { rootLabel = SuperNode  { myRootLabel = x
                                  , myLeaves = y
                                  , myParent = SuperRoot
                                  }
         , subForest = xs
         }

-- | Check if a node belongs to a label
viableNode :: (Ord a, Ord b) => (b -> Bool) -> PropertyMap a b -> a -> Bool
viableNode viable propertyMap x = (>= Just 0)
                                . fmap (Seq.length . fmap viable)
                                . M.lookup x
                                $ propertyMap

-- | Assign the clumpiness to each subtree and add them to a list. Ignore
-- if the vertex is a root
pinpointRecursion :: (Ord a, Ord b)
                  => (b -> Bool)
                  -> PropertyMap a b
                  -> Tree (SuperNode a)
                  -> Seq.Seq (Pinpoint a b)
pinpointRecursion _ _ (Node { subForest = [] }) = Seq.empty
pinpointRecursion viable propertyMap tree@( Node { rootLabel = SuperNode { myRootLabel = label
                                                                         , myLeaves    = descendents
                                                                         , myParent    = parent
                                                                         }
                                                 , subForest = xs }
                                          ) =
    case newPinpoint of
        Nothing  -> continue
        (Just x) -> x Seq.<| continue
  where
    continue       = mconcat
                   . map (pinpointRecursion viable validPropertyMap)
                   $ xs
    newPinpoint    = case parent of
                         SuperRoot -> Nothing
                         _         -> Just
                                      Pinpoint { pinpointLabel      = label
                                               , pinpointClumpiness = clump
                                               , pinpointLeaves = relevantLeaves
                                               }
    relevantLeaves   = Seq.fromList
                     . filter (viableNode viable validPropertyMap)
                     . M.keys
                     $ descendents
    clump            = Seq.fromList
                     $ generateClumpMap viable validPropertyMap newTree
    validPropertyMap = getValidPropertyMap newTree propertyMap
    newTree          = filterRootLeaves . rootSubtree $ tree

-- | Return the clumpiness vertices in the tree based on the minimum
-- clumpiness and the minimum number of descendent leaves. Here, viable is
-- the same usage as in Algorithms. The clumpiness values are only reported
-- if they are above a threshold of minClumpiness.
pinpoint :: (Ord a, Ord b) => Double
                           -> Int
                           -> (b -> Bool)
                           -> PropertyMap a b
                           -> Tree (SuperNode a)
                           -> Seq.Seq (Pinpoint a b)
pinpoint minClumpiness minLeaves viable propertyMap tree =
      Seq.filter
      ( \x -> (not . Seq.null . pinpointClumpiness $ x)
           && (Seq.length . pinpointLeaves $ x) >= minLeaves
      )
    . fmap (\x -> x { pinpointClumpiness = filterClumpSeq
                                         . pinpointClumpiness
                                         $ x
                    }
           )
    $ pinpointList
  where
    pinpointList             = pinpointRecursion viable propertyMap tree
    validPropertyMap subtree = getValidPropertyMap subtree propertyMap
    filterClumpSeq           = Seq.filter ((>= minClumpiness) . thd')
