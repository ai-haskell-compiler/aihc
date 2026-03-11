{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}

module Data.Primitive.Unlifted.Class
  ( PrimUnlifted(..)
  ) where

import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Text.Short (ShortText,toShortByteString)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)
import Data.Primitive.PrimArray (PrimArray(..),MutablePrimArray(..))
import Data.Primitive.ByteArray (ByteArray(..),MutableByteArray(..))
import Data.Primitive.Array (Array (..), MutableArray (..))
import Data.Primitive.SmallArray (SmallArray (..), SmallMutableArray (..))
import Data.Primitive.MutVar (MutVar (..))
import GHC.MVar (MVar(..))
import GHC.IORef (IORef(..))
import GHC.STRef (STRef(..))
import GHC.Weak (Weak(..))
import GHC.Conc (TVar(..),ThreadId(..))
import GHC.StableName (StableName(..))
import GHC.Exts (MutableByteArray#,ByteArray#
                ,Array#,MutableArray#,SmallArray#,SmallMutableArray#
                ,Weak#,TVar#,ThreadId#,StableName#)
import GHC.Exts (MVar#,MutVar#,RealWorld)

import qualified Data.Primitive.MVar as PM
import qualified GHC.Exts as Exts

import Data.Primitive.Unlifted.Type

class PrimUnlifted a where
  type Unlifted a :: UnliftedType
  toUnlifted# :: a -> Unlifted a
  fromUnlifted# :: Unlifted a -> a

instance PrimUnlifted (Array a) where
  type Unlifted (Array a) = Array# a
  toUnlifted# (Array a) = a
  fromUnlifted# x = Array x

instance PrimUnlifted (MutableArray s a) where
  type Unlifted (MutableArray s a) = MutableArray# s a
  toUnlifted# (MutableArray a) = a
  fromUnlifted# x = MutableArray x

instance PrimUnlifted (SmallArray a) where
  type Unlifted (SmallArray a) = SmallArray# a
  toUnlifted# (SmallArray a) = a
  fromUnlifted# x = SmallArray x

instance PrimUnlifted (SmallMutableArray s a) where
  type Unlifted (SmallMutableArray s a) = SmallMutableArray# s a
  toUnlifted# (SmallMutableArray a) = a
  fromUnlifted# x = SmallMutableArray x

instance PrimUnlifted (PrimArray a) where
  type Unlifted (PrimArray a) = ByteArray#
  toUnlifted# (PrimArray x) = x
  fromUnlifted# x = PrimArray x

instance PrimUnlifted ByteArray where
  type Unlifted ByteArray = ByteArray#
  toUnlifted# (ByteArray x) = x
  fromUnlifted# x = ByteArray x

instance PrimUnlifted ShortByteString where
  type Unlifted ShortByteString = ByteArray#
  toUnlifted# (SBS x) = x
  fromUnlifted# x = SBS x

instance PrimUnlifted ShortText where
  type Unlifted ShortText = ByteArray#
  toUnlifted# t = case toShortByteString t of { SBS x -> x }
  fromUnlifted# x = fromShortByteStringUnsafe (SBS x)

instance PrimUnlifted (MutableByteArray s) where
  type Unlifted (MutableByteArray s) = MutableByteArray# s
  toUnlifted# (MutableByteArray x) = x
  fromUnlifted# x = MutableByteArray x

instance PrimUnlifted (MutablePrimArray s a) where
  type Unlifted (MutablePrimArray s a) = MutableByteArray# s
  toUnlifted# (MutablePrimArray x) = x
  fromUnlifted# x = MutablePrimArray x

instance PrimUnlifted (PM.MVar s a) where
  type Unlifted (PM.MVar s a) = MVar# s a
  toUnlifted# (PM.MVar x) = x
  fromUnlifted# x = PM.MVar x

instance PrimUnlifted (MVar a) where
  type Unlifted (MVar a) = MVar# RealWorld a
  toUnlifted# (MVar x) = x
  fromUnlifted# x = MVar x

instance PrimUnlifted (MutVar s a) where
  type Unlifted (MutVar s a) = MutVar# s a
  toUnlifted# (MutVar x) = x
  fromUnlifted# x = MutVar x

instance PrimUnlifted (STRef s a) where
  type Unlifted (STRef s a) = MutVar# s a
  toUnlifted# (STRef x) = x
  fromUnlifted# x = STRef x

instance PrimUnlifted (IORef a) where
  type Unlifted (IORef a) = MutVar# RealWorld a
  toUnlifted# (IORef (STRef x)) = x
  fromUnlifted# x = IORef (STRef x)

instance PrimUnlifted (Weak a) where
  type Unlifted (Weak a) = Weak# a
  toUnlifted# (Weak w) = w
  fromUnlifted# w = Weak w

instance PrimUnlifted (TVar a) where
  type Unlifted (TVar a) = TVar# Exts.RealWorld a
  toUnlifted# (TVar t) = t
  fromUnlifted# t = TVar t

instance PrimUnlifted ThreadId where
  type Unlifted ThreadId = ThreadId#
  toUnlifted# (ThreadId tid) = tid
  fromUnlifted# tid = ThreadId tid

instance PrimUnlifted (StableName a) where
  type Unlifted (StableName a) = StableName# a
  toUnlifted# (StableName sn) = sn
  fromUnlifted# sn = StableName sn
