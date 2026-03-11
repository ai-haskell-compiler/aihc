{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Unlifted
  ( -- * Base
    Maybe# (..)
  , Either# (..)
  , ST# (..)
  , IO# (..)

    -- * Text
  , ShortText# (..)
  , Text# (..)

    -- * Arrays
  , PrimArray# (..)
  , MutablePrimArray# (..)

    -- * Boolean
  , Bool# (..)
  , pattern True#
  , pattern False#
  ) where

import Data.Kind (Type)
import GHC.Exts (RealWorld)
import GHC.Exts (ByteArray#, Int32#, Levity (Unlifted), MutableByteArray#, RuntimeRep (..), State#, TYPE, Word#)

{- | Variant of @ST@ where the argument type does not have to be lifted.
This does not have a monad instance and is difficult to use.
-}
newtype ST# :: forall (r :: RuntimeRep). Type -> TYPE r -> Type where
  ST# ::
    forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r).
    { unST# :: State# s -> (# State# s, a #)
    } ->
    ST# s a

newtype IO# :: forall (r :: RuntimeRep). TYPE r -> Type where
  IO# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    { unIO# :: State# RealWorld -> (# State# RealWorld, a #)
    } ->
    IO# a

-- | Unboxed variant of @Bool@.
newtype Bool# :: TYPE 'WordRep where
  Bool# :: Word# -> Bool#

-- | Unboxed variant of @Maybe@.
newtype Maybe# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('SumRep '[ 'TupleRep '[], r]) where
  Maybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# (# #) | a #) -> Maybe# @r a

-- | Unboxed variant of @Either@.
newtype Either# :: forall (ra :: RuntimeRep) (rb :: RuntimeRep). TYPE ra -> TYPE rb -> TYPE ('SumRep '[ra, rb]) where
  Either# :: forall (ra :: RuntimeRep) (rb :: RuntimeRep) (a :: TYPE ra) (b :: TYPE rb). (# a | b #) -> Either# a b

{-# COMPLETE True#, False# #-}

pattern True# :: Bool#
pattern True# = Bool# 1##

pattern False# :: Bool#
pattern False# = Bool# 0##

-- | Mutable variant of 'PrimArray#'.
newtype MutablePrimArray# :: forall (r :: RuntimeRep). Type -> TYPE r -> TYPE ('BoxedRep 'Unlifted) where
  MutablePrimArray# :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r). MutableByteArray# s -> MutablePrimArray# s a

{- | This resembles the @PrimArray@ type from @primitive@, but the phantom
parameter is an unboxed type, not a lifted type. For example:

* @PrimArray Word8@
* @PrimArray# Word8#@
-}
newtype PrimArray# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('BoxedRep 'Unlifted) where
  PrimArray# :: forall (r :: RuntimeRep) (a :: TYPE r). ByteArray# -> PrimArray# a

-- | Unlifted variant of @ShortText@.
newtype ShortText# :: TYPE ('BoxedRep 'Unlifted) where
  ShortText# :: ByteArray# -> ShortText#

{- | Unboxed variant of @Text@. This includes a somewhat dubious restriction
that on the offset and length that prevents byte arrays larger than 2GiB
from being used as the backing store.

This decision makes the type work well in the vext library, and it makes
the in-memory format close to what Apache Arrow uses.
-}
newtype Text# :: TYPE ('TupleRep ['BoxedRep 'Unlifted, 'Int32Rep, 'Int32Rep]) where
  Text# :: (# ByteArray#, Int32#, Int32# #) -> Text#
