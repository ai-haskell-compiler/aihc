-- | A queue data structure with \(\mathcal{O}(1)\) (worst-case) operations, as described in
--
--   * Okasaki, Chris. \"Simple and efficient purely functional queues and deques.\" /Journal of functional programming/ 5.4 (1995): 583-592.
--   * Okasaki, Chris. /Purely Functional Data Structures/. Diss. Princeton University, 1996.
module Queue
  ( -- * Queue
    Queue (Empty, Full),

    -- ** Initialization
    empty,
    singleton,
    fromList,

    -- * Basic interface
    enqueue,
    dequeue,

    -- ** Extended interface
    enqueueFront,

    -- * Queries
    isEmpty,

    -- * Transformations
    map,
    traverse,

    -- * Conversions
    toList,
  )
where

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Traversable qualified as Traversable
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (foldMap, length, map, span, traverse)

------------------------------------------------------------------------------------------------------------------------
-- Queue type and instances

-- | A queue data structure with \(\mathcal{O}(1)\) (worst-case) operations.
data Queue a
  = Q
      -- The front of the queue.
      -- Invariant: not shorter than the back
      [a]
      -- The back of the queue, in reverse order.
      [a]
      -- Some tail of the front of the queue.
      -- Invariant: length = length of front - length of back
      Schedule
  -- fmap loses exact sharing of front of queue and schedule, but the schedule still works, forcing cons cells of the
  -- original front (before fmap)
  deriving stock (Functor)

instance (Eq a) => Eq (Queue a) where
  (==) :: Queue a -> Queue a -> Bool
  xs == ys =
    Queue.toList xs == Queue.toList ys

instance Foldable Queue where
  foldMap :: (Monoid m) => (a -> m) -> Queue a -> m
  foldMap f =
    go
    where
      go = \case
        Empty -> mempty
        Full x xs -> f x <> go xs

  null :: Queue a -> Bool
  null =
    isEmpty

  toList :: Queue a -> [a]
  toList =
    Queue.toList

instance Monoid (Queue a) where
  mempty :: Queue a
  mempty =
    empty

-- | \(\mathcal{O}(n)\), where \(n\) is the size of the second argument.
instance Semigroup (Queue a) where
  (<>) :: Queue a -> Queue a -> Queue a
  xs <> Empty = xs
  xs <> Full y ys = enqueue y xs <> ys

instance (Show a) => Show (Queue a) where
  show :: Queue a -> String
  show =
    show . Queue.toList

instance Traversable Queue where
  traverse :: (Applicative f) => (a -> f b) -> Queue a -> f (Queue b)
  traverse =
    Queue.traverse

------------------------------------------------------------------------------------------------------------------------
-- Patterns

-- | An empty queue.
pattern Empty :: Queue a
pattern Empty <- (dequeue -> Nothing)

-- | The front of a queue, and the rest of it.
pattern Full :: a -> Queue a -> Queue a
pattern Full x xs <- (dequeue -> Just (x, xs))

{-# COMPLETE Empty, Full #-}

------------------------------------------------------------------------------------------------------------------------
-- Internal smart constructor utils

-- `queue xs ys schedule` is always called when |schedule| = |xs| - |ys| + 1 (i.e. just after a enqueue or dequeue)
makeQueue :: [a] -> [a] -> Schedule -> Queue a
makeQueue xs ys = \case
  Z -> Queue.fromList (rotate xs ys [])
  S schedule -> Q xs ys schedule

-- rotate xs ys zs = xs ++ reverse ys ++ zs
-- Precondition: |ys| = |xs| + 1
rotate :: [a] -> NonEmptyList a -> [a] -> [a]
rotate [] (y :| _) zs = y : zs
rotate (x : xs) (y :| ys) zs = x : rotate xs ys (y : zs)

------------------------------------------------------------------------------------------------------------------------
-- Initialization

-- | An empty queue.
empty :: Queue a
empty =
  Q [] [] Z

-- | A singleton queue.
singleton :: a -> Queue a
singleton x =
  Queue.fromList [x]

-- | \(\mathcal{O}(1)\). Construct a queue from a list. The head of the list corresponds to the front of the queue.
fromList :: [a] -> Queue a
fromList xs =
  Q xs [] (unsafeCoerce xs)

------------------------------------------------------------------------------------------------------------------------
-- Basic interface

-- | \(\mathcal{O}(1)\). Enqueue an element at the back of a queue, to be dequeued last.
enqueue :: a -> Queue a -> Queue a
enqueue y (Q xs ys schedule) =
  makeQueue xs (y : ys) schedule

-- | \(\mathcal{O}(1)\) front, \(\mathcal{O}(1)\) rest. Dequeue an element from the front of a queue.
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue = \case
  Q [] _ _ -> Nothing
  Q (x : xs) ys schedule -> Just (x, makeQueue xs ys schedule)

------------------------------------------------------------------------------------------------------------------------
-- Extended interface

-- | \(\mathcal{O}(1)\). Enqueue an element at the front of a queue, to be dequeued next.
enqueueFront :: a -> Queue a -> Queue a
enqueueFront x (Q xs ys schedule) =
  -- smart constructor not needed here
  -- we also add useless work to the schedule to maintain the convenient rotate-on-empty-schedule trigger
  Q (x : xs) ys (unsafeCoerce x : schedule)

------------------------------------------------------------------------------------------------------------------------
-- Queries

-- | \(\mathcal{O}(1)\). Is a queue empty?
isEmpty :: Queue a -> Bool
isEmpty = \case
  Q [] _ _ -> True
  _ -> False

------------------------------------------------------------------------------------------------------------------------
-- Transformations

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
map :: (a -> b) -> Queue a -> Queue b
map =
  fmap

-- | \(\mathcal{O}(n)\). Apply a function to every element in a queue.
traverse :: (Applicative f) => (a -> f b) -> Queue a -> f (Queue b)
traverse f =
  -- FIXME can we do better here?
  fmap fromList . Traversable.traverse f . toList

------------------------------------------------------------------------------------------------------------------------
-- Conversions

-- | \(\mathcal{O}(n)\). Construct a list from a queue. The head of the list corresponds to the front of the queue.
toList :: Queue a -> [a]
toList =
  List.unfoldr dequeue

------------------------------------------------------------------------------------------------------------------------
-- Schedule utils

type Schedule =
  [Any]

pattern Z :: Schedule
pattern Z = []

pattern S :: Schedule -> Schedule
pattern S xs <- _ : xs

{-# COMPLETE Z, S #-}

------------------------------------------------------------------------------------------------------------------------
-- Non-empty list utils

-- A list that we know is non-empty somehow.
type NonEmptyList a =
  [a]

pattern (:|) :: a -> [a] -> NonEmptyList a
pattern (:|) x xs = x : xs

{-# COMPLETE (:|) #-}
