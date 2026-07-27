{-# LANGUAGE MagicHash #-}

module GHC.Exception
  ( Exception (..),
    SomeException (..),
    throw,
  )
where

import GHC.Exception.Type (Exception (..), SomeException (..))
import GHC.Prim (raise#)

throw :: (Exception e) => e -> a
throw exception = raise# (toException exception)
