{-# LANGUAGE MagicHash #-}

module GHC.Err
  ( error,
    errorWithoutStackTrace,
    undefined,
  )
where

import GHC.Base (String)
import GHC.Prim (raise#)
import GHC.Types (RuntimeRep, TYPE)

error :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
error = raise#

errorWithoutStackTrace :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
errorWithoutStackTrace = raise#

undefined :: a
undefined = raise# "Prelude.undefined"
