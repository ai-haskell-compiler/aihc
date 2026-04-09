{- ORACLE_TEST xfail type family with [*] kind parameter and equations fails to parse -}
{-# LANGUAGE DataKinds, TypeFamilies #-}
module StarListKindTypeFamily where

type family F (xs :: [*]) (r :: *) :: * where
  F '[] r = r
