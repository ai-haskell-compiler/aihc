{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, TypeFamilies #-}
module StarListKindTypeFamily where

type family F (xs :: [*]) (r :: *) :: * where
  F '[] r = r
