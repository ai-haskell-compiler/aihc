{- ORACLE_TEST xfail Promoted list elements containing infix ErrorMessage operators are not parenthesized in pretty-printed output -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module M where

type M1 = '[ 'Text "alpha" ':<>: 'Text "beta" ]
