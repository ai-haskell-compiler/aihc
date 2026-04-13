{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternCaseAlternative where

anyIdx :: a -> Maybe Int
anyIdx = undefined

f :: [a] -> Int
f x = case x of
  [anyIdx -> Just i] -> i
  _ -> 0
