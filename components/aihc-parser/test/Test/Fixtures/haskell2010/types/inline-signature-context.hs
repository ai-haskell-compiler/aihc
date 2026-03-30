{- ORACLE_TEST pass -}
module T8 where
inc :: Int -> Int
inc = ((+ 1) :: Num a => a -> a)