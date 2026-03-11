module Acc.NeAcc.Def
  ( NeAcc (..),
    foldM,
    prependReverseList,
    uncons,
    unconsTo,
    unsnoc,
    unsnocTo,
    appendEnumFromTo,
  )
where

import Acc.Prelude hiding (foldM, unsnoc)

-- |
-- Non-empty accumulator.
--
-- Relates to 'Acc.Acc' the same way as 'NonEmpty' to list.
data NeAcc a
  = Leaf a
  | Branch !(NeAcc a) !(NeAcc a)

instance (Show a) => Show (NeAcc a) where
  show =
    show . toList

instance (NFData a) => NFData (NeAcc a) where
  rnf = \case
    Leaf a -> rnf a
    Branch l r -> seq (rnf l) (rnf r)

instance NFData1 NeAcc where
  liftRnf rnfLeaf = rnfTree
    where
      rnfTree = \case
        Leaf a -> rnfLeaf a
        Branch l r -> seq (rnfTree l) (rnfTree r)

instance IsList (NeAcc a) where
  type Item (NeAcc a) = a
  {-# INLINE [0] fromList #-}
  fromList list =
    case reverse list of
      a : b -> prependReverseList b (Leaf a)
      _ -> error "Empty input list"
  {-# INLINE [0] toList #-}
  toList =
    foldr (:) []

deriving instance Functor NeAcc

instance Applicative NeAcc where
  pure =
    Leaf
  {-# INLINE [1] (<*>) #-}
  (<*>) =
    \case
      Branch a b ->
        \c ->
          Branch (a <*> c) (b <*> c)
      Leaf a ->
        fmap a

instance Foldable NeAcc where
  {-# INLINEABLE [0] foldr #-}
  foldr :: (a -> b -> b) -> b -> NeAcc a -> b
  foldr step =
    go []
    where
      go stack next = \case
        Branch l r -> go (r : stack) next l
        Leaf a -> step a $ case stack of
          tree : stack -> go stack next tree
          [] -> next

  {-# INLINE [0] foldr' #-}
  foldr' :: (a -> b -> b) -> b -> NeAcc a -> b
  foldr' step =
    peel []
    where
      peel layers acc =
        \case
          Leaf a ->
            unpeel (step a acc) layers
          Branch l r ->
            peel (l : layers) acc r
      unpeel !acc =
        \case
          h : t ->
            peel t acc h
          _ ->
            acc

  {-# INLINE [0] foldl #-}
  foldl :: (b -> a -> b) -> b -> NeAcc a -> b
  foldl step acc =
    \case
      Branch a b ->
        foldlOnBranch step acc a b
      Leaf a ->
        step acc a
    where
      foldlOnBranch :: (b -> a -> b) -> b -> NeAcc a -> NeAcc a -> b
      foldlOnBranch step acc a b =
        case b of
          Leaf c ->
            step (foldl step acc a) c
          Branch c d ->
            foldlOnBranch step acc (Branch a c) d

  {-# INLINE [0] foldl' #-}
  foldl' :: (b -> a -> b) -> b -> NeAcc a -> b
  foldl' step = build []
    where
      build stack !acc = \case
        Branch l r -> build (r : stack) acc l
        Leaf leaf -> case stack of
          tree : stack -> build stack (step acc leaf) tree
          _ -> step acc leaf

  {-# INLINE [0] foldMap #-}
  foldMap :: (Monoid m) => (a -> m) -> NeAcc a -> m
  foldMap map =
    peel
    where
      peel =
        \case
          Branch a b ->
            peelLeftStacking b a
          Leaf a ->
            map a
      peelLeftStacking buff =
        \case
          Branch a b ->
            peelLeftStacking (Branch b buff) a
          Leaf a ->
            map a <> peel buff

  {-# INLINE [0] foldMap' #-}
  foldMap' :: (Monoid m) => (a -> m) -> NeAcc a -> m
  foldMap' =
    foldMapTo' mempty
    where
      foldMapTo' :: (Monoid m) => m -> (a -> m) -> NeAcc a -> m
      foldMapTo' !acc map =
        \case
          Branch a b -> foldMapToOnBranch' acc map a b
          Leaf a -> acc <> map a
      foldMapToOnBranch' :: (Monoid m) => m -> (a -> m) -> NeAcc a -> NeAcc a -> m
      foldMapToOnBranch' acc map a b =
        case a of
          Leaf c -> foldMapTo' (acc <> map c) map b
          Branch c d -> foldMapToOnBranch' acc map c (Branch d b)

instance Traversable NeAcc where
  {-# INLINE [0] traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> NeAcc a -> f (NeAcc b)
  traverse map =
    \case
      Branch a b ->
        traverseOnBranch map a b
      Leaf a ->
        Leaf <$> map a
    where
      traverseOnBranch :: (Applicative f) => (a -> f b) -> NeAcc a -> NeAcc a -> f (NeAcc b)
      traverseOnBranch map a b =
        case a of
          Leaf c ->
            Branch . Leaf <$> map c <*> traverse map b
          Branch c d ->
            traverseOnBranch map a (Branch d b)

instance Foldable1 NeAcc where
  {-# INLINE [0] fold1 #-}
  fold1 :: (Semigroup m) => NeAcc m -> m
  fold1 =
    \case
      Branch l r ->
        rebalancingLeft l r (foldl' (<>))
      Leaf a ->
        a

  {-# INLINE [0] foldMap1 #-}
  foldMap1 :: (Semigroup m) => (a -> m) -> NeAcc a -> m
  foldMap1 f =
    \case
      Branch l r ->
        rebalancingLeft l r (foldl' (\m a -> m <> f a) . f)
      Leaf a ->
        f a

  {-# INLINE [0] toNonEmpty #-}
  toNonEmpty :: NeAcc a -> NonEmpty a
  toNonEmpty =
    findFirst
    where
      findFirst =
        \case
          Branch l r ->
            findFirstOnBranch l r
          Leaf a ->
            a :| []
      findFirstOnBranch l r =
        case l of
          Branch ll lr ->
            findFirstOnBranch ll (Branch lr r)
          Leaf a ->
            a :| foldr (:) [] r

instance Traversable1 NeAcc where
  {-# INLINE [0] traverse1 #-}
  traverse1 map =
    \case
      Branch a b ->
        traverseOnBranch map a b
      Leaf a ->
        Leaf <$> map a
    where
      traverseOnBranch map a b =
        case a of
          Leaf c ->
            Branch . Leaf <$> map c <.> traverse1 map b
          Branch c d ->
            traverseOnBranch map a (Branch d b)

instance Alt NeAcc where
  {-# INLINE [1] (<!>) #-}
  (<!>) =
    Branch

instance Semigroup (NeAcc a) where
  {-# INLINE [1] (<>) #-}
  (<>) =
    Branch

{-# INLINE rebalancingLeft #-}
rebalancingLeft :: NeAcc a -> NeAcc a -> (a -> NeAcc a -> b) -> b
rebalancingLeft l r cont =
  case l of
    Branch ll lr ->
      rebalancingLeft ll (Branch lr r) cont
    Leaf a ->
      cont a r

foldM :: (Monad m) => (a -> b -> m a) -> a -> NeAcc b -> m a
foldM step !acc =
  \case
    Branch a b -> foldMOnBranch step acc a b
    Leaf a -> step acc a
  where
    foldMOnBranch :: (Monad m) => (a -> b -> m a) -> a -> NeAcc b -> NeAcc b -> m a
    foldMOnBranch step acc a b =
      case a of
        Leaf c -> step acc c >>= \acc' -> foldM step acc' b
        Branch c d -> foldMOnBranch step acc c (Branch d b)

prependReverseList :: [a] -> NeAcc a -> NeAcc a
prependReverseList list tree =
  case list of
    head : tail -> prependReverseList tail (Branch (Leaf head) tree)
    _ -> tree

{-# INLINE uncons #-}
uncons :: NeAcc a -> (a, Maybe (NeAcc a))
uncons =
  \case
    Branch l r ->
      fmap Just (unconsTo r l)
    Leaf a ->
      (a, Nothing)

{-# INLINE unconsTo #-}
unconsTo :: NeAcc a -> NeAcc a -> (a, NeAcc a)
unconsTo buff =
  \case
    Branch l r ->
      unconsTo (Branch r buff) l
    Leaf a ->
      (a, buff)

unsnoc :: NeAcc a -> (a, Maybe (NeAcc a))
unsnoc =
  \case
    Branch l r ->
      fmap Just (unsnocTo l r)
    Leaf a ->
      (a, Nothing)

unsnocTo :: NeAcc a -> NeAcc a -> (a, NeAcc a)
unsnocTo buff =
  \case
    Branch l r ->
      unsnocTo (Branch buff l) r
    Leaf a ->
      (a, buff)

appendEnumFromTo :: (Enum a, Ord a) => a -> a -> NeAcc a -> NeAcc a
appendEnumFromTo from to =
  if from <= to
    then appendEnumFromTo (succ from) to . flip Branch (Leaf from)
    else id
