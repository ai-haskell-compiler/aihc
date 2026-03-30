{- ORACLE_TEST pass -}
{-# LANGUAGE StandaloneKindSignatures #-}

module StandaloneKindHigherOrder where

import Data.Kind (Type)

type Apply :: (Type -> Type) -> Type -> Type
type Apply f a = f a