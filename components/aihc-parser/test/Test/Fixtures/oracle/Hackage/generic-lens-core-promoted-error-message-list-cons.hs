{- ORACLE_TEST xfail Promoted list element boundaries break after infix ErrorMessage operators -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module M where

type M1 = '[ 'Text "alpha" ':<>: 'Text "beta", 'Text "gamma" ]
