module Data.Tree.Fenwick(FTree,
                         empty, insert,
                         query, invQuery,
                         toList, toFreqList,
                         fromList,
                         size, depth) where

import Data.List(sortBy, foldl')
-- ^ Fenwick trees are a O(log N) data structure for updating cumulative sums.
--   This implementation comes with an operation to find a least element for
--   which real-valued cumulative sum reaches certain value, and allows for
--   storage of arbitrary information in the nodes.
--   See http://en.wikipedia.org/wiki/Fenwick_tree

--import Control.Exception(assert) -- DEBUG

-- | Type of values that are summed.
type Val = Double

-- | Mother structure holds functions
--   that allow to get a value to be summed and comparison function.
--   Below there is a tree of `FNode`s.
data FTree a = FTree { root :: FNode a
                     , val  :: a -> Val
                     , cmp  :: a -> a -> Ordering
                     }
-- TODO: Typeable, Data and others necessary for transport?

instance (Show a) => Show (FTree a) where
  showsPrec _ ft = ("FTree " ++) . shows (root ft)

-- | Node within a tree, contains a splitting element for comparison,
--   and partial sum for this element, which is added to all lookups
--   to the right.
data FNode a = Node { psum        :: Val,
                      split       :: a,
                      left, right :: FNode a
                    }
             | Leaf
  deriving (Show)

-- | Creates an empty Fenwick tree.
empty :: (a -> Double) -> (a -> a -> Ordering) -> FTree a
empty v c = FTree { root   = Leaf
                  , val    = v
                  , cmp    = c
                  }

-- | Inserts a value into a Fenwick tree.
insert :: a -> FTree a -> FTree a
insert a ft = ft { root = insert' a (val ft) (cmp ft) (root ft) }

-- | Inserts a value into a given node of Fenwick tree.
insert' a val cmp Leaf = Node { psum  = val a
                              , split = a
                              , left  = Leaf
                              , right = Leaf
                              }
insert' a val cmp n@(Node { psum  = p
                          , split = s
                          , left  = l
                          , right = r
                          }) = case a `cmp` s of
                                 GT -> n { right = insert' a val cmp r }
                                 LT -> n { psum = p + val a
                                         , left = insert' a val cmp l }
                                 EQ -> n { psum = p + val a } -- just adjust frequency

-- | Finds a cumulative sum up to a given node of a Fenwick tree.
--   Note: if the node is not found, a sum at point corresponding to this
--   node is still returned. (Convenient for finding CDF value at a given point.)
query :: a -> FTree a -> Val
query a ft = query' (cmp ft) a (root ft)

-- | Finds a cumulative sum up to a given node within a subtree.
query' cmp a Leaf                 = 0.0
query' cmp a (Node { psum  = p
                   , split = s
                   , left  = l
                   , right = r }) = case a `cmp` s of
                                      GT -> p + query' cmp a r
                                      LT ->     query' cmp a l
                                      EQ -> p

-- | Finds a node corresponding to a given cumulative sum,
--   convenient for sampling quantile function of a distribution.
--   NOTE: returns an answer only up to a cumulative sum
--   of a whole tree.
invQuery :: Val -> FTree a -> Maybe a
invQuery v ft = invQuery' v (root ft)

-- | Finds a node corresponding to a given cumulative sum,
--   if it is in a given subtree.
invQuery' :: Val -> FNode a -> Maybe a 
invQuery' v Leaf = Nothing
invQuery' v (Node { psum  = p
                  , split = s
                  , left  = l
                  , right = r }) = case v `compare` p of
                                     EQ -> Just s
                                     GT -> invQuery' (v-p) r
                                     LT -> case invQuery' v l of
                                             Just r  -> Just r
                                             Nothing -> Just s

-- | Extract a sorted list of inserted values from the tree.
toList :: FTree a -> [a]
toList ft = toList' (root ft) []

-- | Extract a sorted list of inserted objects from a subtree,
--   and prepends it to a last argument. (For efficiency.)
toList'  Leaf                  cont = cont
toList' (Node { split = s
              , left  = l
              , right = r })   cont = toList' l $ s:toList' r cont

-- | Extract a sorted list of cumulative sums, and corresponding
--   objects from the tree.
toFreqList :: FTree a -> [(Double, a)]
toFreqList ft = toFreqList' 0.0 (root ft) []

-- | Extract a sorted list of cumulative sums, and corresponding
--   objects from a subtree, assuming a given cumulative sum
--   from the start (left side of a tree), and list of values
--   from the right side of a tree as two helper arguments.
--   (For efficiency.)
toFreqList' cSum Leaf cont = cont
toFreqList' cSum (Node { split = s
                       , psum  = p
                       , left  = l
                       , right = r
                       }) cont = toFreqList' cSum l $
                                   (nSum, s):toFreqList' nSum r cont
  where
    nSum = p+cSum

-- | Creates a tree from a list and helper functions: compare, and value.
fromList cmp val ls = FTree { cmp  = cmp
                            , val  = val
                            , root = fromList' cmp val l $ sortBy cmp ls
                            }
  where
    l = length ls

-- | Creates a subtree from a list and helper functions.
--   O(n^2): First it splits a list in half, then
fromList' cmp val 0 [ ] = Leaf
fromList' cmp val 1 [a] = Node { split = a
                               , psum  = val a
                               , left  = Leaf
                               , right = Leaf
                               }
fromList' cmp val n ls =   Node { split = a
                                , psum  = val a
                                , left  = fromList' cmp val n'  lsLeft
                                , right = fromList' cmp val n'' lsRight
                                }
  where
    a       = head rest
    lsRight = tail rest
    (lsLeft, rest) = splitAt n' ls
    n'  = n `div` 2
    n'' = n - n' - 1
-- TODO: Make it O(n) by recursion with continuations.
{-
    assertions r = assert (n' + n'' + 1   == n  ) $
                   assert (length lsRight == n'') $
                   assert (length lsLeft  == n' ) $
                   r
-}

-- | Returns a maximum depth of a tree.
depth :: FTree a -> Int
depth = depth' . root

-- | Returns maximum depth of a given subtree.
depth' Leaf                 = 0
depth' (Node { left  = l
             , right = r }) = (depth' l `max` depth' r) + 1

-- | Returns number of elements in a tree.
size :: FTree a -> Int
size = length . toList

