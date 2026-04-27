{- ORACLE_TEST xfail closed type family with promoted tuple patterns and polymorphic kind variables not yet supported -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module UnPosPromotedTuple where

type family UnPos (x :: k1) :: k2 where
  UnPos ('Pos x) = x
  UnPos '( 'Pos x, 'Pos y) = '(x, y)
