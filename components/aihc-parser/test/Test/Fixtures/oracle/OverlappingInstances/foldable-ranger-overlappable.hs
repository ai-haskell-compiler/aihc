{- ORACLE_TEST xfail instance with OVERLAPPABLE pragma, type operators in constraint, and lambda case not yet supported -}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module OverlappingInstanceRangeR where

data RangeR a b = NilR | (RangeR a b) :++ b

instance {-# OVERLAPPABLE #-}
        Foldable (RangeR 0 (m - 1)) => Foldable (RangeR 0 m) where
        foldr (-<) z = \case NilR -> z; xs :++ x -> foldr (-<) (x -< z) xs
