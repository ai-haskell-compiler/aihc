{- |
Custom class for storing tuples
and wrapper for storing tuples in standard 'Foreign.Storable' class.
These two solutions do not need orphan instances.
The package @storable-tuple@ makes use of this implementation.
-}
module Foreign.Storable.Record.Tuple (
   Storable(..),
   Tuple(..),
   ) where

import qualified Foreign.Storable.Record as Record
import qualified Foreign.Storable as Store
import Foreign.Ptr (Ptr, castPtr)

import qualified Control.Applicative.HT as App
import Data.Tuple.HT (fst3, snd3, thd3)

import qualified Test.QuickCheck as QC


newtype Tuple a = Tuple {getTuple :: a}
   deriving (Eq, Show)


instance (QC.Arbitrary a) => QC.Arbitrary (Tuple a) where
   arbitrary = fmap Tuple QC.arbitrary
   shrink (Tuple a) = map Tuple $ QC.shrink a


instance Storable a => Store.Storable (Tuple a) where
   {-# INLINABLE sizeOf #-}
   {-# INLINABLE alignment #-}
   {-# INLINABLE peek #-}
   {-# INLINABLE poke #-}
   sizeOf = sizeOf . getTuple
   alignment = alignment . getTuple
   peek = fmap Tuple . peek . castPtr
   poke ptr = poke (castPtr ptr) . getTuple


class Storable a where
   sizeOf :: a -> Int
   alignment :: a -> Int
   peek :: Ptr a -> IO a
   poke :: Ptr a -> a -> IO ()

instance (Store.Storable a, Store.Storable b) => Storable (a,b) where
   {-# INLINABLE sizeOf #-}
   {-# INLINABLE alignment #-}
   {-# INLINABLE peek #-}
   {-# INLINABLE poke #-}
   sizeOf    = Record.sizeOf storePair
   alignment = Record.alignment storePair
   peek      = Record.peek storePair
   poke      = Record.poke storePair

{-# INLINE storePair #-}
storePair ::
   (Store.Storable a, Store.Storable b) =>
   Record.Dictionary (a,b)
storePair =
   Record.run $
   App.lift2 (,)
      (Record.element fst)
      (Record.element snd)


instance
   (Store.Storable a, Store.Storable b, Store.Storable c) =>
      Storable (a,b,c) where
   {-# INLINABLE sizeOf #-}
   {-# INLINABLE alignment #-}
   {-# INLINABLE peek #-}
   {-# INLINABLE poke #-}
   sizeOf    = Record.sizeOf storeTriple
   alignment = Record.alignment storeTriple
   peek      = Record.peek storeTriple
   poke      = Record.poke storeTriple

{-# INLINE storeTriple #-}
storeTriple ::
   (Store.Storable a, Store.Storable b, Store.Storable c) =>
   Record.Dictionary (a,b,c)
storeTriple =
   Record.run $
   App.lift3 (,,)
      (Record.element fst3)
      (Record.element snd3)
      (Record.element thd3)

instance
   (Store.Storable a, Store.Storable b, Store.Storable c, Store.Storable d) =>
      Storable (a,b,c,d) where
   {-# INLINABLE sizeOf #-}
   {-# INLINABLE alignment #-}
   {-# INLINABLE peek #-}
   {-# INLINABLE poke #-}
   sizeOf    = Record.sizeOf storeQuadruple
   alignment = Record.alignment storeQuadruple
   peek      = Record.peek storeQuadruple
   poke      = Record.poke storeQuadruple

{-# INLINE storeQuadruple #-}
storeQuadruple ::
   (Store.Storable a, Store.Storable b, Store.Storable c, Store.Storable d) =>
   Record.Dictionary (a,b,c,d)
storeQuadruple =
   Record.run $
   App.lift4 (,,,)
      (Record.element $ \(x,_,_,_) -> x)
      (Record.element $ \(_,x,_,_) -> x)
      (Record.element $ \(_,_,x,_) -> x)
      (Record.element $ \(_,_,_,x) -> x)


{-
{- Why is this allowed? -}
test :: Char
test = const 'a' undefined

{- Why is type defaulting applied here? The type of 'c' should be fixed. -}
test1 :: (Integral a, RealField.C a) => a
test1 =
   let c = undefined
   in  asTypeOf (round c) c
-}
