{-# LANGUAGE NoImplicitPrelude #-}
{- |
Combine pitchbend and modulation in one data type.
-}
module Synthesizer.MIDI.Value.BendModulation where

import qualified Synthesizer.MIDI.Value.BendWheelPressure as BWP
import qualified Synthesizer.MIDI.Value as MV

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Ring           as Ring

import Foreign.Storable (Storable(sizeOf, alignment, peek, poke), )
import qualified Foreign.Storable.Traversable as Store

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav

import Control.Applicative (Applicative, (<*>), pure, liftA2, )

import Control.DeepSeq (NFData, rnf, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
'bend' is a frequency factor
and 'depth' is a modulation depth to be interpreted by the instrument.
-}
data T a = Cons {bend, depth :: a}
   deriving (Show, Eq)

deflt :: (Ring.C a) => T a
deflt = Cons one zero


instance (NFData a) => NFData (T a) where
   rnf bm =
      case rnf (bend bm) of () -> rnf (depth bm)


instance Functor T where
   {-# INLINE fmap #-}
   fmap f (Cons b m) = Cons (f b) (f m)

-- useful for defining 'peek' instance
instance Applicative T where
   {-# INLINE pure #-}
   pure a = Cons a a
   {-# INLINE (<*>) #-}
   (Cons fb fm) <*> (Cons b m) =
      Cons (fb b) (fm m)

instance Fold.Foldable T where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

-- this allows for kinds of generic programming
instance Trav.Traversable T where
   {-# INLINE sequenceA #-}
   sequenceA (Cons b m) =
      liftA2 Cons b m


force :: T a -> T a
force ~(Cons a b) = (Cons a b)

instance (Storable a) => Storable (T a) where
   {-# INLINE sizeOf #-}
   sizeOf = Store.sizeOf . force
   {-# INLINE alignment #-}
   alignment = Store.alignment . force
   {-# INLINE peek #-}
   peek = Store.peekApplicative
   {-# INLINE poke #-}
   poke = Store.poke



{- |
Multiply the pitch bend by a given factor.
This way you can e.g. shift the pitch bend from around 1
to the actual frequency.
-}
shift ::
   (Ring.C a) =>
   a -> T a -> T a
shift k (Cons b d) = Cons (k*b) d

fromBendWheelPressure ::
   (RealRing.C a, Trans.C a) =>
   Int -> a -> a ->
   BWP.T -> T a
fromBendWheelPressure
      pitchRange wheelDepth pressDepth bwp =
   Cons
      (MV.pitchBend (2^?(fromIntegral pitchRange/12)) 1 (BWP.bend_ bwp))
      (MV.controllerLinear (0,wheelDepth) (BWP.wheel_ bwp) +
       MV.controllerLinear (0,pressDepth) (BWP.pressure_ bwp))
