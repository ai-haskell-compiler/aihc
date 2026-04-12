{- ORACLE_TEST xfail reason="lexer does not handle 'forall a.Floating' without space after dot" -}
{-# LANGUAGE Haskell2010, ExplicitForAll, RankNTypes #-}
module ForallNoSpaceAfterDot where

data ExactPi = Approximate (forall a.Floating a => a)
