module Internal (Box (..), identity, (<+>), Secret (..), Choice (..), Record (..)) where

-- | A box.
data Box a = Box a | Empty

-- | Return the input value.
identity :: a -> a
identity x = x

-- | Select the first value.
(<+>) :: a -> a -> a
left <+> _ = left
infixl 4 <+>

-- | A hidden type.
data Secret = Secret

-- | A value with a choice.
class Choice a where
  -- | Choose a value.
  choose :: a -> a

-- | A record.
data Record a = Record { field :: a }
