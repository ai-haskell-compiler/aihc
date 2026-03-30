{- ORACLE_TEST xfail data family with explicit kind -}
{-# LANGUAGE TypeFamilies #-}
module DataFamilyIndex where

import Data.Kind (Type)

data family GMap k :: Type -> Type