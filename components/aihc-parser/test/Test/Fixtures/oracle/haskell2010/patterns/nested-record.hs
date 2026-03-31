{- ORACLE_TEST xfail pattern with nested record wildcard -}
module PatternNestedRecord where

delete (HKey k@(HKey' f _)) (HSet xs count) = ()
