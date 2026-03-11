{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Numeric.FFTW.Shape (
   Half(..),
   MultiCyclic(cyclicDimensions),
   Symmetric(..), symmetric,
   symmetricLogicalSize,
   Symmetry(..), SymmetrySingleton(..), Even, Odd,
   Shift(..), ShiftSingleton(..), Exact, Halfway,
   ) where

import qualified Data.Array.Comfort.Shape as Shape

import qualified Foreign.C.Types as C

import Control.DeepSeq (NFData, rnf)


newtype Half n = Half n
   deriving (Eq, Show)

instance (NFData n) => NFData (Half n) where
   rnf (Half n) = rnf n

instance (Integral n) => Shape.C (Half n) where
   size (Half n) = halfSize $ fromIntegral n

instance (Integral n) => Shape.Indexed (Half n) where
   type Index (Half n) = n
   indices (Half len) = Shape.indices $ Shape.ZeroBased $ halfSize len
   unifiedOffset (Half len) =
      Shape.unifiedOffset $ Shape.ZeroBased $ halfSize len
   inBounds (Half len) ix = 0<=ix && ix<halfSize len

instance (Integral n) => Shape.InvIndexed (Half n) where
   unifiedIndexFromOffset (Half len) k0 = do
      let k = fromIntegral k0
      Shape.assertIndexFromOffset "Half" k0 $ 0<=k && k<halfSize len
      return k

halfSize :: Integral a => a -> a
halfSize n = div n 2 + 1



class (Shape.C sh) => MultiCyclic sh where
   cyclicDimensions :: sh -> [C.CInt]

instance (Integral n) => MultiCyclic (Shape.Cyclic n) where
   cyclicDimensions (Shape.Cyclic n) = [fromIntegral n]

instance MultiCyclic () where
   cyclicDimensions () = []

instance (MultiCyclic sh0, MultiCyclic sh1) => MultiCyclic (sh0,sh1) where
   cyclicDimensions (sh0,sh1) = cyclicDimensions sh0 ++ cyclicDimensions sh1

instance
   (MultiCyclic sh0, MultiCyclic sh1, MultiCyclic sh2) =>
      MultiCyclic (sh0,sh1,sh2) where
   cyclicDimensions (sh0,sh1,sh2) =
      cyclicDimensions sh0 ++ cyclicDimensions sh1 ++ cyclicDimensions sh2


data Even
data Odd
data SymmetrySingleton symm where
   Even :: SymmetrySingleton Even
   Odd  :: SymmetrySingleton Odd

class Symmetry symm where switchSymmetry :: f Even -> f Odd -> f symm
instance Symmetry Even where switchSymmetry f _ = f
instance Symmetry Odd  where switchSymmetry _ f = f

autoSymmetry :: (Symmetry symm) => SymmetrySingleton symm
autoSymmetry = switchSymmetry Even Odd

instance Eq (SymmetrySingleton symm) where
   x==y =
      case (x,y) of
         (Even, Even) -> True
         (Odd, Odd) -> True

instance Show (SymmetrySingleton symm) where
   show Even = "Even"
   show Odd = "Odd"

instance NFData (SymmetrySingleton symm) where
   rnf s = case s of Even -> (); Odd -> ()

data Exact
data Halfway
data ShiftSingleton shift where
   Exact   :: ShiftSingleton Exact
   Halfway :: ShiftSingleton Halfway

class Shift shift where switchShift :: f Exact -> f Halfway -> f shift
instance Shift Exact   where switchShift f _ = f
instance Shift Halfway where switchShift _ f = f

autoShift :: (Shift shift) => ShiftSingleton shift
autoShift = switchShift Exact Halfway

instance Eq (ShiftSingleton shift) where
   x==y =
      case (x,y) of
         (Exact, Exact) -> True
         (Halfway, Halfway) -> True

instance Show (ShiftSingleton shift) where
   show Exact = "Exact"
   show Halfway = "Halfway"

instance NFData (ShiftSingleton shift) where
   rnf s = case s of Exact -> (); Halfway -> ()


{- |
Shape for stored data of symmetric vectors.
Even is for Cosine transform, Odd for Sine transform.
@shiftTime@ refers to no or halfway shift of the data,
@shiftSpectrum@ refers to no or halfway shift of the Cosine or Sine spectrum.

0 means Exact, 1 means Halfway:

* Even 0 0: even around 0 and even around n-1.

* Even 1 0: even around -0.5 and even around n-0.5.

* Even 0 1: even around 0 and odd around n.

* Even 1 1: even around -0.5 and odd around n-0.5.

* Odd  0 0: odd around -1 and odd around n.

* Odd  1 0: odd around -0.5 and odd around n-0.5.

* Odd  0 1: odd around -1 and even around n-1.

* Odd  1 1: odd around -0.5 and even around n-0.5.


We could pad data of Even symmetric vectors,
but we cannot pad data of Odd symmetric vectors,
because '!' would have to involve 'negate'.
Thus we provide no padding, at all.
-}
data Symmetric symmetry shiftTime shiftSpectrum n =
   Symmetric
      (SymmetrySingleton symmetry)
      (ShiftSingleton shiftTime)
      (ShiftSingleton shiftSpectrum)
      n
   deriving (Eq, Show)

symmetric ::
   (Symmetry symmetry, Shift shiftTime, Shift shiftSpectrum) =>
   n -> Symmetric symmetry shiftTime shiftSpectrum n
symmetric = Symmetric autoSymmetry autoShift autoShift

symmetricLogicalSize ::
   (Num n) => Symmetric symmetry shiftTime shiftSpectrum n -> n
symmetricLogicalSize (Symmetric symmetry shiftTime shiftSpectrum n) =
   case (shiftTime, shiftSpectrum) of
      (Exact, Exact) ->
         case symmetry of
            Even -> 2*n-2
            Odd  -> 2*n+2
      _ -> 2*n

instance
   (Symmetry symmetry, Shift shiftTime, Shift shiftSpectrum, NFData n) =>
      NFData (Symmetric symmetry shiftTime shiftSpectrum n) where
   rnf (Symmetric symmetry shiftTime shiftSpectrum n) =
      rnf (symmetry, shiftTime, shiftSpectrum, n)

instance
   (Symmetry symmetry, Shift shiftTime, Shift shiftSpectrum, Integral n) =>
      Shape.C (Symmetric symmetry shiftTime shiftSpectrum n) where
   size (Symmetric _ _ _ n) = fromIntegral n

instance
   (Symmetry symmetry, Shift shiftTime, Shift shiftSpectrum, Integral n) =>
      Shape.Indexed (Symmetric symmetry shiftTime shiftSpectrum n) where
   type Index (Symmetric symmetry shiftTime shiftSpectrum n) = n
   indices (Symmetric _ _ _ n) =
      Shape.indices $ Shape.ZeroBased $ fromIntegral n
   unifiedOffset (Symmetric _ _ _ n) =
      Shape.unifiedOffset $ Shape.ZeroBased $ fromIntegral n
   inBounds (Symmetric _ _ _ n) ix = 0<=ix && ix<fromIntegral n

instance
   (Symmetry symmetry, Shift shiftTime, Shift shiftSpectrum, Integral n) =>
      Shape.InvIndexed (Symmetric symmetry shiftTime shiftSpectrum n) where
   unifiedIndexFromOffset (Symmetric _ _ _ n) k0 = do
      let k = fromIntegral k0
      Shape.assertIndexFromOffset "Symmetric" k0 $ 0<=k && k<fromIntegral n
      return k
