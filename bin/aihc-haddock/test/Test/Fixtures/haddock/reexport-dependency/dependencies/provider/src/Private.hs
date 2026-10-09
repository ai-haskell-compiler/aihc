module Private (Box (..), identity) where

-- | A box from a dependency.
data Box a = Box a

-- | Return the input value from a dependency.
identity :: a -> a
identity value = value
