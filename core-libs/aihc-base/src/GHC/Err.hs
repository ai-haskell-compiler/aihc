{-# LANGUAGE MagicHash #-}

module GHC.Err
  ( error,
    errorWithoutStackTrace,
    undefined,
  )
where

import GHC.Base (String)
import GHC.Prim (raise#)

error :: String -> a
error = raise#

errorWithoutStackTrace :: String -> a
errorWithoutStackTrace = raise#

undefined :: a
undefined = raise# "Prelude.undefined"
