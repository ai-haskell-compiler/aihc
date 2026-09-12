{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Weak
  ( Weak (..),
    mkWeak,
    deRefWeak,
    finalize,
  )
where

import GHC.IO (IO (..))
import GHC.Prim
import Prelude

data Weak a = Weak (Weak# a)

mkWeak :: k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeak key value finalizer = IO $ \state ->
  case finalizer of
    Nothing -> case mkWeakNoFinalizer# key value state of
      (# next, weak #) -> (# next, Weak weak #)
    Just (IO action) -> case mkWeak# key value action state of
      (# next, weak #) -> (# next, Weak weak #)

deRefWeak :: Weak a -> IO (Maybe a)
deRefWeak (Weak weak) = IO $ \state ->
  case deRefWeak# weak state of
    (# next, flag, value #) -> case flag of
      0# -> (# next, Nothing #)
      _ -> (# next, Just value #)

finalize :: Weak a -> IO ()
finalize (Weak weak) = IO $ \state ->
  case finalizeWeak# weak state of
    (# next, flag, action #) -> case flag of
      0# -> (# next, () #)
      _ -> action next
