-- | Safe wrappers for 'null'-partial 'Foldable' operations
--
-- >>> minimum []
-- *** Exception: Prelude.minimum: empty list
-- >>> minimum [3,1,2]
-- 1
--
-- >>> defaulting 0 minimum []
-- 0
-- >>> defaulting 0 minimum [3,1,2]
-- 1
--
-- >>> mayhap minimum []
-- Nothing
-- >>> mayhap minimum [3,1,2]
-- Just 1

module Data.Foldable.Safe
( defaulting
, mayhap
) where

-- | Apply a function to 'Foldable' data if it is not 'null'. Otherwise, return a
-- default value.
defaulting :: Foldable f => b -> (f a -> b) -> f a -> b
defaulting d f xs
  | null xs   = d
  | otherwise = f xs

-- | Apply a function to 'Foldable' data if it is not 'null', wrapping the result
-- in 'Just'. Otherwise, return 'Nothing'.
mayhap :: Foldable f => (f a -> b) -> f a -> Maybe b
mayhap f = defaulting Nothing (Just . f)
