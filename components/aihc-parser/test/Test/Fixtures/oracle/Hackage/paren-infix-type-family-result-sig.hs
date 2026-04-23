{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module ParenInfixTypeFamilyResultSig where

import Data.Kind (Type)

type family (a ++ b) :: Type
