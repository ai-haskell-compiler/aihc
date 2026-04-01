{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module DataFamilyIndex where

import Data.Kind (Type)

data family GMap k :: Type -> Type