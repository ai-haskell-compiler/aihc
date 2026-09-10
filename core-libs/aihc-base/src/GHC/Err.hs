{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}

module GHC.Err
  ( error,
    errorWithoutStackTrace,
    undefined,
  )
where

import GHC.Base (String)
import GHC.Exception (errorCallException, errorCallWithCallStackException)
import GHC.Prim (raise#)
import GHC.Stack.Types (HasCallStack)
import GHC.Types (RuntimeRep, TYPE)

-- | Stop the program with a message and the call stack of the call site.
error :: forall (r :: RuntimeRep) (a :: TYPE r). (HasCallStack) => String -> a
error message = raise# (errorCallWithCallStackException message ?callStack)

errorWithoutStackTrace :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
errorWithoutStackTrace message = raise# (errorCallException message)

undefined :: forall (r :: RuntimeRep) (a :: TYPE r). (HasCallStack) => a
undefined = raise# (errorCallWithCallStackException "Prelude.undefined" ?callStack)
