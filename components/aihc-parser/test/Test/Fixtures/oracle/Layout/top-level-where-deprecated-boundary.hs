{- ORACLE_TEST pass -}
module TopLevelWhereDeprecatedBoundary where

sortDirShape :: Int -> Int
sortDirShape = sortDirBy id  where

  -- HELPER:
sortDirBy :: (Int -> Int) -> Int -> Int
sortDirBy f = f

{-# DEPRECATED free "Use dirTree instead" #-}
free :: Int
free = 1
