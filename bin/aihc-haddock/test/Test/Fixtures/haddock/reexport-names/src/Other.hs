module Other (identity) where

-- | Select the second value.
identity :: a -> b -> b
identity _ value = value
