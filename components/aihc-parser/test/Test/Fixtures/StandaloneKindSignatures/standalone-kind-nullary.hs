{- ORACLE_TEST pass -}
{-# LANGUAGE StandaloneKindSignatures #-}

module StandaloneKindNullary where

import Data.Kind (Type)

type Range :: Type
data Range = Range