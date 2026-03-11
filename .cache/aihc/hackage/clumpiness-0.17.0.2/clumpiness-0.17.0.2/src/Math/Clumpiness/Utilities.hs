-- Utilities
-- By Gregory W. Schwartz

module Math.Clumpiness.Utilities where

-- | Get the third element of a triple tuple
thd' :: (a, b, c) -> c
thd' (_, _, z) = z
