{- ORACLE_TEST xfail kind annotation on type instance RHS gains extra parentheses during roundtrip -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module TypeInstanceRhsKindAnnotation where

import Data.Kind (Type)

type instance Sing = SIndex as a :: Index as a -> Type
