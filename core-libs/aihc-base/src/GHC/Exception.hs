{-# LANGUAGE MagicHash #-}

module GHC.Exception
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
    throw,
  )
where

import GHC.Exception.Type
  ( ArithException (..),
    Exception (..),
    SomeException (..),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
  )
import GHC.Prim (raise#)

throw :: (Exception e) => e -> a
throw exception = raise# (toException exception)
