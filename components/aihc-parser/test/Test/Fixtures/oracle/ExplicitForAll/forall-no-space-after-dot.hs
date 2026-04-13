{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010, ExplicitForAll, RankNTypes #-}
module ForallNoSpaceAfterDot where

data ExactPi = Approximate (forall a.Floating a => a)
