{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module TypeInstanceRhsKindAnnotation where

import Data.Kind (Type)

type instance Sing = SIndex as a :: Index as a -> Type
