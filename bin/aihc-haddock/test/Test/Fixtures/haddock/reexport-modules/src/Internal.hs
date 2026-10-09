module Internal (Box (..), first, second) where

-- | A box.
data Box a = Box a | Empty

-- | Select the first value.
first :: a -> b -> a
first value _ = value

-- | Select the second value.
second :: a -> b -> b
second _ value = value
