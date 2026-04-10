{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

class C a where
  simple :: a -> a
  simple x = x

  guarded :: a -> a -> a
  guarded x y | x == y = x
             | otherwise = y

  patternGuard :: Maybe a -> a -> a
  patternGuard mx y | Just x <- mx = x
                    | otherwise = y

  letGuard :: a -> a
  letGuard x | let y = x
             , y == x = y
             | otherwise = x
