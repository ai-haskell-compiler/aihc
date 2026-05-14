{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

$(fmap concat $ mapM deriveOnce [1..7])
concat <$> uncurry mkMap `mapM` [ (i, n) | n <- [2 .. 10], i <- [0 .. n - 1] ]
