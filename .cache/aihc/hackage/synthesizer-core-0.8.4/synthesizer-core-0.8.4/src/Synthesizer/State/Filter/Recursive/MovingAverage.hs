{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

-}
module Synthesizer.State.Filter.Recursive.MovingAverage (
   sumsStaticInt,
   modulatedFrac,
   ) where

import qualified Synthesizer.State.Signal  as Sig
import qualified Synthesizer.State.Filter.Recursive.Integration as Integration

import qualified Synthesizer.State.Filter.Delay as Delay

import qualified Algebra.Module                as Module
import qualified Algebra.RealField             as RealField
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Like 'Synthesizer.State.Filter.NonRecursive.sums' but in a recursive form.
This needs only linear time (independent of the window size)
but may accumulate rounding errors.

@
ys = xs * (1,0,0,0,-1) \/ (1,-1)
ys * (1,-1) = xs * (1,0,0,0,-1)
ys = xs * (1,0,0,0,-1) + ys * (0,1)
@
-}
{-# INLINE sumsStaticInt #-}
sumsStaticInt :: (Additive.C v) => Int -> Sig.T v -> Sig.T v
sumsStaticInt n xs =
   Integration.run (xs - Delay.staticPos n xs)

{-
staticInt :: (Module.C a v, Additive.C v) => Int -> Sig.T v -> Sig.T v
staticInt n xs =
-}


{-
Sum of a part of a vector with negative sign for reverse order.
It adds from @from@ (inclusively) to @to@ (exclusively),
that is, it sums up @abs (to-from)@ values

sumFromTo :: (Additive.C v) => Int -> Int -> Sig.T v -> v
sumFromTo from to =
   if from <= to
     then          Sig.sum . Sig.take (to-from) . Sig.drop from
     else negate . Sig.sum . Sig.take (from-to) . Sig.drop to
-}

{-# INLINE sumFromToFrac #-}
sumFromToFrac :: (RealField.C a, Module.C a v) => a -> a -> Sig.T v -> v
sumFromToFrac from to xs =
   let (fromInt, fromFrac) = splitFraction from
       (toInt,   toFrac)   = splitFraction to
   in  case compare fromInt toInt of
          EQ -> (to-from) *> Sig.index fromInt xs
          LT ->
            Sig.sum $
            Sig.zipWith id
               (((1-fromFrac) *>) `Sig.cons`
                Sig.replicate (toInt-fromInt-1) id `Sig.append`
                Sig.singleton (toFrac *>)) $
            Sig.drop fromInt xs
          GT ->
            negate $ Sig.sum $
            Sig.zipWith id
               (((1-toFrac) *>) `Sig.cons`
                Sig.replicate (fromInt-toInt-1) id `Sig.append`
                Sig.singleton (fromFrac *>)) $
            Sig.drop toInt xs


{-
            run $
               addNextWeighted (1-toFrac) >>
               replicateM_ (fromInt-toInt-1) addNext >>
               addNextWeighted (fromFrac)

type Accumulator v a =
   WriterT (Dual (Endo v)) (StateT (Sig.T v) Maybe a)

getNext :: Accumulator v a
getNext =
   lift $ StateT $ ListHT.viewL

addAccum :: Additive.C v => v -> Accumulator v ()
addAccum x = tell ((x+) $!)

addNext :: Additive.C v => Accumulator v ()
addNext w =
   addAccum =<< getNext

addNextWeighted :: Module.C a v => a -> Accumulator v ()
addNextWeighted w =
   addAccum . (w *>) =<< getNext
-}

{-
newtype Accumulator v =
   Accumulator ((v, Sig.T v) -> v -> (Sig.T v, v))

addNext :: Additive.C v => Accumulator v
addNext =
   Accumulator $ \(x,xs) s -> (xs, x+s)

addNextWeighted :: Module.C a v => a -> Accumulator v
addNextWeighted a =
   Accumulator $ \(x,xs) s -> (xs, a*>x + s)

bindAccum :: Accumulator v -> Accumulator v -> Accumulator v
bindAccum (Accumulator f) (Accumulator g) =
   Accumulator $ \x s0 ->
      let (ys,s1) = f x s0
      in  maybe s1 () (ListHT.viewL ys)
-}


{- |
Sig.T a must contain only non-negative elements.
-}
{-# INLINE sumDiffsModulated #-}
sumDiffsModulated :: (RealField.C a, Module.C a v) =>
   a -> Sig.T a -> Sig.T v -> Sig.T v
sumDiffsModulated d ds =
   Sig.init .
   -- prevent negative d's since 'drop' cannot restore past values
   Sig.zipWithTails (uncurry sumFromToFrac)
       (Sig.zip (Sig.cons (d+1) ds) (Sig.map (1+) ds)) .
   Sig.cons zero
{-
   Sig.zipWithTails (uncurry sumFromToFrac)
      (Sig.zip (Sig.cons d (Sig.map (subtract 1) ds)) ds)
-}

{-
sumsModulated :: (RealField.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T v -> Sig.T v
sumsModulated maxDInt ds xs =
   let maxD  = fromIntegral maxDInt
       posXs = sumDiffsModulated 0 ds xs
       negXs = sumDiffsModulated maxD (Sig.map (maxD-) ds) (Delay.static maxDInt xs)
   in  Integration.run (posXs - negXs)
-}

{- |
Shift sampling points by a half sample period
in order to preserve signals for window widths below 1.
-}
{-# INLINE sumsModulatedHalf #-}
sumsModulatedHalf :: (RealField.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T v -> Sig.T v
sumsModulatedHalf maxDInt ds xs =
   let maxD  = fromIntegral maxDInt
       d0    = maxD+0.5
       delXs = Delay.staticPos maxDInt xs
       posXs = sumDiffsModulated d0 (Sig.map (d0+) ds) delXs
       negXs = sumDiffsModulated d0 (Sig.map (d0-) ds) delXs
   in  Integration.run (posXs - negXs)

{-# INLINE modulatedFrac #-}
modulatedFrac :: (RealField.C a, Module.C a v) =>
   Int -> Sig.T a -> Sig.T v -> Sig.T v
modulatedFrac maxDInt ds xs =
   Sig.zipWith (\d y -> recip (2*d) *> y) ds $
   sumsModulatedHalf maxDInt ds xs

