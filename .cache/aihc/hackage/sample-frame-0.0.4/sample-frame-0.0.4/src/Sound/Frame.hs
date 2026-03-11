module Sound.Frame
   (C(..),
   numberOfChannelsFoldable,
   sizeOfElementFoldable,
   sizeOfElementType,
   paddedSizeOf,
   withSignal,
   ) where

import Data.Word (Word8, Word16, Word32, )
import Data.Int (Int8, Int16, Int32, )
import Foreign.Storable (Storable, sizeOf, alignment, )

import qualified Data.Foldable as Fold


{- |
This is a class for nested tuples used as sample frames.

Should we make Storable a superclass of 'C'?
-}
class C y where
   {- | The argument is not touched and can be undefined -}
   numberOfChannels :: y -> Int
   {- |
   Size of elements.
   In a nested record type, like @Stereo (Stereo a)@,
   it is the size of the atomic element, in our example @a@.
   We assume that the atomic element values all have the same size,
   such that @sizeOfElement undefined@ is defined.
   -}
   sizeOfElement :: y -> Int

{- |
Default implementations for a foldable Frame.
-}
numberOfChannelsFoldable ::
   (C y, Fold.Foldable f) => f y -> Int
numberOfChannelsFoldable =
   Fold.foldl' (\n y -> n + numberOfChannels y) 0
--   Fold.sum . fmap numberOfChannels

sizeOfElementFoldable ::
   (C y, Fold.Foldable f) => f y -> Int
sizeOfElementFoldable =
   sizeOfElement . head . Fold.toList

{- |
Returns the size of an undefined element.
This might be more efficient than 'sizeOfElementFoldable'.
-}
sizeOfElementType ::
   (C y) => f y -> Int
sizeOfElementType =
   sizeOfElement . elementType


{-# INLINE elementType #-}
elementType :: f a -> a
elementType _ =
   error "Sound.Frame.sizeOfElement may not depend on element values"


instance C Word8 where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf

instance C Int8 where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf

instance C Word16 where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf

instance C Int16 where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf

instance C Word32 where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf

instance C Int32 where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf

instance C Float where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf

instance C Double where
   numberOfChannels _ = 1
   sizeOfElement = sizeOf


{- |
Space that an element consumes in a Storable Array.
This is space for the element plus padding.
-}
-- cf. storable-record:FixedArray.roundUp
paddedSizeOf :: Storable a => a -> Int
paddedSizeOf x =
   sizeOf x + mod (- sizeOf x) (alignment x)


withSignal :: (y -> a) -> (sig y -> a)
withSignal f _ = f (error "Frame: dummy signal parameter touched")
