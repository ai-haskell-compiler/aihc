{- ORACLE_TEST pass -}
module PatternNestedRecord where

delete (HKey k@(HKey' f _)) (HSet xs count) = ()
