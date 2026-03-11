-- | This is the 'FFI' approach.
--
-- Implements casting via the FFI, using `alloca` like in
-- <http://hackage.haskell.org/package/data-binary-ieee754>.
module Data.ReinterpretCast.Internal.ImplFFI
  ( floatToWord
  , wordToFloat
  , doubleToWord
  , wordToDouble
  ) where

import qualified Foreign as F
import           System.IO.Unsafe (unsafePerformIO)


-- | Reinterpret-casts a `Float` to a `F.Word32`.
floatToWord :: Float -> F.Word32
floatToWord = fromFloat

{-# INLINABLE floatToWord #-}


-- | Reinterpret-casts a `F.Word32` to a `Float`.
wordToFloat :: F.Word32 -> Float
wordToFloat = toFloat

{-# INLINABLE wordToFloat #-}


-- | Reinterpret-casts a `Double` to a `F.Word64`.
doubleToWord :: Double -> F.Word64
doubleToWord = fromFloat

{-# INLINABLE doubleToWord #-}


-- | Reinterpret-casts a `F.Word64` to a `Double`.
wordToDouble :: F.Word64 -> Double
wordToDouble = toFloat

{-# INLINABLE wordToDouble #-}


toFloat :: (F.Storable word, F.Storable float) => word -> float
toFloat word = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) word
  F.peek buf

{-# INLINE toFloat #-}


fromFloat :: (F.Storable word, F.Storable float) => float -> word
fromFloat float = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) float
  F.peek buf

{-# INLINE fromFloat #-}
