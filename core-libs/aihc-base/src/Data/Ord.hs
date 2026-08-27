module Data.Ord
  ( Ord (..),
    Ordering (..),
    Down (..),
    comparing,
  )
where

import Prelude

newtype Down a = Down {getDown :: a}

instance (Eq a) => Eq (Down a) where
  Down left == Down right = left == right
  Down left /= Down right = left /= right

instance (Ord a) => Ord (Down a) where
  compare (Down left) (Down right) = compare right left
  Down left < Down right = right < left
  Down left <= Down right = right <= left
  Down left > Down right = right > left
  Down left >= Down right = right >= left
  max (Down left) (Down right) = Down (min left right)
  min (Down left) (Down right) = Down (max left right)

instance (Bounded a) => Bounded (Down a) where
  minBound = Down minBound
  maxBound = Down maxBound

instance (Enum a) => Enum (Down a) where
  succ (Down value) = Down (succ value)
  pred (Down value) = Down (pred value)
  toEnum value = Down (toEnum value)
  fromEnum (Down value) = fromEnum value
  enumFrom (Down value) = mapDown (enumFrom value)
  enumFromThen (Down first) (Down second) = mapDown (enumFromThen first second)
  enumFromTo (Down first) (Down last) = mapDown (enumFromTo first last)
  enumFromThenTo (Down first) (Down second) (Down last) =
    mapDown (enumFromThenTo first second last)

instance (Read a) => Read (Down a) where
  readsPrec precedence = readParen (precedence > 10) readDown

instance (Show a) => Show (Down a) where
  showsPrec precedence (Down value) =
    showParen (precedence > 10) (showString "Down " . showsPrec 11 value)

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing projection x y = compare (projection x) (projection y)

readDown :: (Read a) => ReadS (Down a)
readDown input =
  case lex input of
    (constructor, afterConstructor) : _ ->
      case constructor == "Down" of
        True -> wrapDownReads (readsPrec 11 afterConstructor)
        False -> []
    [] -> []

wrapDownReads :: [(a, String)] -> [(Down a, String)]
wrapDownReads [] = []
wrapDownReads ((value, rest) : results) = (Down value, rest) : wrapDownReads results

mapDown :: [a] -> [Down a]
mapDown = fmap Down
