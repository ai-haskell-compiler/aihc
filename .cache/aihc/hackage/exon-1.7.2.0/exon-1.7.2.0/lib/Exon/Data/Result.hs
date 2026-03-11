-- | Description: Internal
module Exon.Data.Result where

-- | The combined segments, either empty or a value.
data Result a =
  Empty
  |
  Result a 
  deriving stock (Eq, Show, Foldable)

instance Semigroup a => Semigroup (Result a) where
  (<>) Empty a = a
  (<>) a Empty = a
  (<>) (Result l) (Result r) = Result (l <> r)

instance Semigroup (Result a) => Monoid (Result a) where
  mempty =
    Empty
