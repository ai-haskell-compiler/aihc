module Reactive.Banana.MIDI.Time where

import qualified Reactive.Banana.Bunch.Combinators as RB

import qualified Numeric.NonNegative.Class as NonNeg

import Control.Applicative (Const(Const), )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )
import Data.Tuple.HT (mapPair, mapSnd, )
import Data.Ord.HT (comparing, )
import Data.Eq.HT (equating, )

import Prelude hiding (div, )

{- |
The 'T' types are used instead of floating point types,
because the latter ones caused unpredictable 'negative number' errors.
There should be a common denominator to all involved numbers.
This way we can prevent unlimited growth of denominators.
-}
-- the Const type helps us to avoid explicit kind signature extension
newtype T m t a = Cons (Const a (m t))

instance Show a => Show (T m t a) where
   showsPrec n x =
      showParen (n>10) $
         showString "Time.cons " . shows (decons x)

instance Eq a => Eq (T m t a) where
   (==)  =  equating decons

instance Ord a => Ord (T m t a) where
   compare  =  comparing decons

cons :: a -> T m t a
cons = Cons . Const

decons :: T m t a -> a
decons (Cons (Const a)) = a

relative ::
   (Ord a, Monoid a) =>
   String -> a -> T m Relative a
relative name t =
   if t>=mempty
     then cons t
     else error $ name ++ ": negative time"


data Absolute = Absolute
data Relative = Relative

newtype Seconds = Seconds {unSeconds :: Rational}
   deriving (Show, Eq, Ord)

newtype Ticks = Ticks {unTicks :: Integer}
   deriving (Show, Eq, Ord)

instance Semigroup Seconds where
   Seconds x <> Seconds y = Seconds $ x+y

instance Monoid Seconds where
   mempty = Seconds 0
   mappend = (<>)

instance Semigroup Ticks where
   Ticks x <> Ticks y = Ticks $ x+y

instance Monoid Ticks where
   mempty = Ticks 0
   mappend = (<>)


instance (Semigroup a) => Semigroup (T m t a) where
   x <> y = cons $ decons x <> decons y

instance (Monoid a) => Monoid (T m t a) where
   mempty = cons mempty
   mappend x y = cons $ mappend (decons x) (decons y)


class RelativeC t where
instance RelativeC Relative where

{- |
Technically identical to NonNeg.C
but without connotation of non-negativity.
-}
class (Ord a, Monoid a) => Split a where
   split :: a -> a -> (a, (Bool, a))

instance Split Seconds where
   split = NonNeg.splitDefault unSeconds Seconds

instance Split Ticks where
   split = NonNeg.splitDefault unTicks Ticks


instance (RelativeC t, Split a) => NonNeg.C (T m t a) where
   split x y =
      mapPair (cons, mapSnd cons) $ split (decons x) (decons y)


class RB.MonadMoment m => Timed m where
   ticksFromSeconds :: T m t Seconds -> m (T m t Ticks)

class Quantity a where
   ticksFromAny :: (Timed m) => T m t a -> m (T m t Ticks)

instance Quantity Seconds where
   ticksFromAny = ticksFromSeconds

instance Quantity Ticks where
   ticksFromAny = return


consRel :: String -> Rational -> T m Relative Seconds
consRel msg x =
   if x>=0
     then cons $ Seconds x
     else error $ msg ++ ": negative number"

inc ::
   (Monoid a) =>
   T m Relative a -> T m t a -> T m t a
inc dt t = cons $ mappend (decons t) (decons dt)

subSat ::
   Split a => T m t a -> T m t a -> T m Relative a
subSat t1 t0 =
   let (b,d) = snd $ split (decons t0) (decons t1)
   in  cons $ if b then d else mempty

{- |
'scale' could also be defined for 'Seconds',
however, repeated application of 'scale'
would yield unlimited growth of denominator.
This applies e.g. to controlled beat generation.
-}
scale, scaleCeiling :: Double -> T m Relative Ticks -> T m Relative Ticks
scale k t =
   cons $ Ticks $ round $ toRational k * getTicks t

scaleCeiling k t =
   cons $ Ticks $ ceiling $ toRational k * getTicks t

scaleInt :: Integral i => i -> T m Relative Ticks -> T m Relative Ticks
scaleInt k t =
   cons $ Ticks $ getTicks t * fromIntegral k

div :: T m Relative Ticks -> T m Relative Ticks -> Double
div dt1 dt0  =  getTicks dt1 / getTicks dt0

getTicks :: Num a => T m Relative Ticks -> a
getTicks = fromInteger . unTicks . decons
