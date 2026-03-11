-- |
-- Definitions of strict linked list.
--
-- Most basic operations like `fmap`, `filter`, `<*>`
-- can only be implemented efficiently by producing an intermediate list in reversed order
-- and then reversing it to the original order.
-- These intermediate reversed functions are exposed by the API,
-- because they very well may be useful for efficient implementations of data-structures built on top of list.
-- E.g., the <http://hackage.haskell.org/package/deque "deque"> package exploits them heavily.
--
-- One useful rule of thumb would be that
-- whenever you see that a function has a reversed counterpart,
-- that counterpart is faster and hence if you don't care about the order or
-- intend to reverse the list further down the line, you should give preference to that counterpart.
--
-- The typical `toList` and `fromList` conversions are provided by means of
-- the `Foldable` and `IsList` instances.
module StrictList where

import StrictList.Prelude hiding (drop, dropWhile, reverse, take, takeWhile)

-- |
-- Strict linked list.
data List a = Cons !a !(List a) | Nil
  deriving
    (Eq, Ord, Show, Read, Generic, Generic1, Data, Typeable)

instance IsList (List a) where
  type Item (List a) = a
  fromList = reverse . fromListReversed
  toList = foldr (:) []

instance Semigroup (List a) where
  (<>) a b = case b of
    Nil -> a
    _ -> prependReversed (reverse a) b

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f = reverse . mapReversed f

instance Foldable List where
  foldr step init =
    let loop = \case
          Cons head tail -> step head (loop tail)
          _ -> init
     in loop
  foldl' step init =
    let loop !acc = \case
          Cons head tail -> loop (step acc head) tail
          _ -> acc
     in loop init

instance Traversable List where
  sequenceA = foldr (liftA2 Cons) (pure Nil)

instance Apply List where
  (<.>) fList aList = apReversed (reverse fList) (reverse aList)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) = (<.>)

instance Alt List where
  (<!>) = mappend

instance Plus List where
  zero = mempty

instance Alternative List where
  empty = zero
  (<|>) = (<!>)

instance Bind List where
  (>>-) ma amb = reverse (explodeReversed amb ma)
  join = reverse . joinReversed

instance Monad List where
  return = pure
  (>>=) = (>>-)

instance MonadPlus List where
  mzero = empty
  mplus = (<|>)

instance (Hashable a) => Hashable (List a)

instance (NFData a) => NFData (List a)

instance NFData1 List

-- |
-- Convert to lazy list in normal form (with all elements and spine evaluated).
toListReversed :: List a -> [a]
toListReversed = go []
  where
    go !outputList = \case
      Cons element list -> go (element : outputList) list
      Nil -> outputList

-- |
-- Reverse the list.
{-# INLINE reverse #-}
reverse :: List a -> List a
reverse = foldl' (flip Cons) Nil

-- |
-- Leave only the specified amount of elements.
{-# INLINE take #-}
take :: Int -> List a -> List a
take amount = reverse . takeReversed amount

-- |
-- Leave only the specified amount of elements, in reverse order.
takeReversed :: Int -> List a -> List a
takeReversed =
  let loop !output !amount =
        if amount > 0
          then \case
            Cons head tail -> loop (Cons head output) (pred amount) tail
            _ -> output
          else const output
   in loop Nil

-- |
-- Leave only the elements after the specified amount of first elements.
drop :: Int -> List a -> List a
drop amount =
  if amount > 0
    then \case
      Cons _ tail -> drop (pred amount) tail
      _ -> Nil
    else id

-- |
-- Leave only the elements satisfying the predicate.
{-# INLINE filter #-}
filter :: (a -> Bool) -> List a -> List a
filter predicate = reverse . filterReversed predicate

-- |
-- Leave only the elements satisfying the predicate,
-- producing a list in reversed order.
filterReversed :: (a -> Bool) -> List a -> List a
filterReversed predicate =
  let loop !newList = \case
        Cons head tail ->
          if predicate head
            then loop (Cons head newList) tail
            else loop newList tail
        Nil -> newList
   in loop Nil

-- |
-- Leave only the first elements satisfying the predicate.
{-# INLINE takeWhile #-}
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile predicate = reverse . takeWhileReversed predicate

-- |
-- Leave only the first elements satisfying the predicate,
-- producing a list in reversed order.
takeWhileReversed :: (a -> Bool) -> List a -> List a
takeWhileReversed predicate =
  let loop !newList = \case
        Cons head tail ->
          if predicate head
            then loop (Cons head newList) tail
            else newList
        _ -> newList
   in loop Nil

-- |
-- Drop the first elements satisfying the predicate.
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile predicate = \case
  Cons head tail ->
    if predicate head
      then dropWhile predicate tail
      else Cons head tail
  Nil -> Nil

-- |
-- An optimized version of the same predicate applied to `takeWhile` and `dropWhile`.
-- IOW,
--
-- >span predicate list = (takeWhile predicate list, dropWhile predicate list)
{-# INLINE span #-}
span :: (a -> Bool) -> List a -> (List a, List a)
span predicate = first reverse . spanReversed predicate

-- |
-- Same as `span`, only with the first list in reverse order.
spanReversed :: (a -> Bool) -> List a -> (List a, List a)
spanReversed predicate =
  let buildPrefix !prefix = \case
        Cons head tail ->
          if predicate head
            then buildPrefix (Cons head prefix) tail
            else (prefix, Cons head tail)
        _ -> (prefix, Nil)
   in buildPrefix Nil

-- |
-- An opposite version of `span`. I.e.,
--
-- >break predicate = span (not . predicate)
{-# INLINE break #-}
break :: (a -> Bool) -> List a -> (List a, List a)
break predicate = first reverse . breakReversed predicate

-- |
-- Same as `break`, only with the first list in reverse order.
breakReversed :: (a -> Bool) -> List a -> (List a, List a)
breakReversed predicate =
  let buildPrefix !prefix = \case
        Cons head tail ->
          if predicate head
            then (prefix, Cons head tail)
            else buildPrefix (Cons head prefix) tail
        _ -> (prefix, Nil)
   in buildPrefix Nil

-- |
-- Same as @(`takeWhile` predicate . `reverse`)@.
-- E.g.,
--
-- >>> takeWhileFromEnding (> 2) (fromList [1,4,2,3,4,5])
-- fromList [5,4,3]
{-# INLINE takeWhileFromEnding #-}
takeWhileFromEnding :: (a -> Bool) -> List a -> List a
takeWhileFromEnding predicate =
  foldl'
    ( \newList a ->
        if predicate a
          then Cons a newList
          else Nil
    )
    Nil

-- |
-- Same as @(`dropWhile` predicate . `reverse`)@.
-- E.g.,
--
-- >>> dropWhileFromEnding (> 2) (fromList [1,4,2,3,4,5])
-- fromList [2,4,1]
dropWhileFromEnding :: (a -> Bool) -> List a -> List a
dropWhileFromEnding predicate =
  let loop confirmed unconfirmed = \case
        Cons head tail ->
          if predicate head
            then loop confirmed (Cons head unconfirmed) tail
            else
              let !newConfirmed = Cons head unconfirmed
               in loop newConfirmed newConfirmed tail
        Nil -> confirmed
   in loop Nil Nil

-- |
-- Same as @(`span` predicate . `reverse`)@.
spanFromEnding :: (a -> Bool) -> List a -> (List a, List a)
spanFromEnding predicate =
  let loop !confirmedPrefix !unconfirmedPrefix !suffix = \case
        Cons head tail ->
          if predicate head
            then loop confirmedPrefix (Cons head unconfirmedPrefix) (Cons head suffix) tail
            else
              let !prefix = Cons head unconfirmedPrefix
               in loop prefix prefix Nil tail
        Nil -> (suffix, confirmedPrefix)
   in loop Nil Nil Nil

-- |
-- Pattern match on list using functions.
--
-- Allows to achieve all the same as `uncons` only without intermediate `Maybe`.
--
-- Essentially provides the same functionality as `either` for `Either` and `maybe` for `Maybe`.
{-# INLINE match #-}
match :: result -> (element -> List element -> result) -> List element -> result
match nil cons = \case
  Cons head tail -> cons head tail
  Nil -> nil

-- |
-- Get the first element and the remainder of the list if it's not empty.
{-# INLINE uncons #-}
uncons :: List a -> Maybe (a, List a)
uncons = \case
  Cons head tail -> Just (head, tail)
  _ -> Nothing

-- |
-- Get the first element, if list is not empty.
{-# INLINE head #-}
head :: List a -> Maybe a
head = \case
  Cons head _ -> Just head
  _ -> Nothing

-- |
-- Get the last element, if list is not empty.
{-# INLINE last #-}
last :: List a -> Maybe a
last =
  let loop !previous = \case
        Cons head tail -> loop (Just head) tail
        _ -> previous
   in loop Nothing

-- |
-- Get all elements of the list but the first one.
{-# INLINE tail #-}
tail :: List a -> List a
tail = \case
  Cons _ tail -> tail
  Nil -> Nil

-- |
-- Get all elements but the last one.
{-# INLINE init #-}
init :: List a -> List a
init = reverse . initReversed

-- |
-- Get all elements but the last one, producing the results in reverse order.
initReversed :: List a -> List a
initReversed =
  let loop !confirmed !unconfirmed = \case
        Cons head tail -> loop unconfirmed (Cons head unconfirmed) tail
        _ -> confirmed
   in loop Nil Nil

-- |
-- Apply the functions in the left list to elements in the right one.
{-# INLINE apZipping #-}
apZipping :: List (a -> b) -> List a -> List b
apZipping left right = apZippingReversed (reverse left) (reverse right)

-- |
-- Apply the functions in the left list to elements in the right one,
-- producing a list of results in reversed order.
apZippingReversed :: List (a -> b) -> List a -> List b
apZippingReversed =
  let loop bList = \case
        Cons f fTail -> \case
          Cons a aTail -> loop (Cons (f a) bList) fTail aTail
          _ -> bList
        _ -> const bList
   in loop Nil

-- ** Reversed intermediate functions used in instances

-------------------------

-- |
-- Construct from a lazy list in reversed order.
{-# INLINE fromListReversed #-}
fromListReversed :: [a] -> List a
fromListReversed = foldl' (flip Cons) Nil

-- |
-- Add elements of the left list in reverse order
-- in the beginning of the right list.
{-# INLINE prependReversed #-}
prependReversed :: List a -> List a -> List a
prependReversed = \case
  Cons head tail -> prependReversed tail . Cons head
  Nil -> id

-- |
-- Map producing a list in reversed order.
mapReversed :: (a -> b) -> List a -> List b
mapReversed f =
  let loop !newList = \case
        Cons head tail -> loop (Cons (f head) newList) tail
        _ -> newList
   in loop Nil

-- |
-- Apply the functions in the left list to every element in the right one,
-- producing a list of results in reversed order.
{-# INLINE apReversed #-}
apReversed :: List (a -> b) -> List a -> List b
apReversed fList aList = foldl' (\z f -> foldl' (\z a -> Cons (f a) z) z aList) Nil fList

-- |
-- Use a function to produce a list of lists and then concat them sequentially,
-- producing the results in reversed order.
{-# INLINE explodeReversed #-}
explodeReversed :: (a -> List b) -> List a -> List b
explodeReversed amb = foldl' (\z -> foldl' (flip Cons) z . amb) Nil

-- |
-- Join (concat) producing results in reversed order.
{-# INLINE joinReversed #-}
joinReversed :: List (List a) -> List a
joinReversed = foldl' (foldl' (flip Cons)) Nil

-- |
-- Map and filter elements producing results in reversed order.
{-# INLINE mapMaybeReversed #-}
mapMaybeReversed :: (a -> Maybe b) -> List a -> List b
mapMaybeReversed f = go Nil
  where
    go !outputList = \case
      Cons inputElement inputTail -> case f inputElement of
        Just outputElement -> go (Cons outputElement outputList) inputTail
        Nothing -> go outputList inputTail
      Nil -> outputList

-- |
-- Keep only the present values, reversing the order.
catMaybesReversed :: List (Maybe a) -> List a
catMaybesReversed = go Nil
  where
    go !outputList = \case
      Cons inputElement inputTail -> case inputElement of
        Just outputElement -> go (Cons outputElement outputList) inputTail
        Nothing -> go outputList inputTail
      Nil -> outputList
