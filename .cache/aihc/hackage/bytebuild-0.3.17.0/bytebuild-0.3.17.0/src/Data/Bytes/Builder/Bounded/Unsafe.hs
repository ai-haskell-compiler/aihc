{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Bytes.Builder.Bounded.Unsafe
  ( -- * Types
    Builder (..)

    -- * Construct
  , construct

    -- * Run
  , pasteST
  , pasteIO
  ) where

import Data.Kind (Type)
import Data.Primitive (MutableByteArray (..))
import GHC.Exts (Int (I#), Int#, MutableByteArray#, RealWorld, State#)
import GHC.IO (stToIO)
import GHC.ST (ST (ST))
import GHC.TypeLits (Nat)

{- | A builder parameterized by the maximum number of bytes it uses
when executed.
-}
newtype Builder :: Nat -> Type where
  Builder ::
    -- | This function takes a buffer, an offset, and a number of remaining bytes.
    --   It returns the new offset.
    (forall s. MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)) ->
    Builder n

{- | Constructor for 'Builder' that works on a function with lifted
arguments instead of unlifted ones. This is just as unsafe as the
actual constructor.
-}
construct :: (forall s. MutableByteArray s -> Int -> ST s Int) -> Builder n
{-# INLINE construct #-}
construct f = Builder $
  \arr off s0 ->
    case unST (f (MutableByteArray arr) (I# off)) s0 of
      (# s1, (I# n) #) -> (# s1, n #)

{- | This function does not enforce the known upper bound on the
size. It is up to the user to do this.
-}
pasteST :: Builder n -> MutableByteArray s -> Int -> ST s Int
{-# INLINE pasteST #-}
pasteST (Builder f) (MutableByteArray arr) (I# off) =
  ST $ \s0 -> case f arr off s0 of
    (# s1, r #) -> (# s1, (I# r) #)

{- | This function does not enforce the known upper bound on the
size. It is up to the user to do this.
-}
pasteIO :: Builder n -> MutableByteArray RealWorld -> Int -> IO Int
{-# INLINE pasteIO #-}
pasteIO b m off = stToIO (pasteST b m off)

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f
