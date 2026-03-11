{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Plain.Cut (
   {- * dissection -}
   takeUntilPause,
   takeUntilInterval,

   {- * glueing -}
   selectBool,
   select,
   arrange,
   ) where

import qualified Synthesizer.Plain.Signal as Sig

import qualified Data.EventList.Relative.TimeBody as EventList

import Data.Array (Array, Ix, (!))

import qualified MathObj.LaurentPolynomial as Laurent
import qualified Algebra.RealRing as RealRing
import qualified Algebra.Additive as Additive

import qualified Number.NonNegative as NonNeg

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Take signal until it falls short of a certain amplitude for a given time.
-}
takeUntilPause :: (RealRing.C a) => a -> Int -> Sig.T a -> Sig.T a
takeUntilPause y =
   takeUntilInterval ((<=y) . abs)

{- |
Take values until the predicate p holds for n successive values.
The list is truncated at the beginning of the interval of matching values.
-}
takeUntilInterval :: (a -> Bool) -> Int -> Sig.T a -> Sig.T a
takeUntilInterval p n xs =
   map fst $
   takeWhile ((<n) . snd) $
   zip xs $
   drop n $
   scanl (\acc x -> if p x then succ acc else 0) 0 xs
      ++ repeat 0



-- Better use zipWithMatch from NumericPrelude.Numeric?
selectBool :: (Sig.T a, Sig.T a) -> Sig.T Bool -> Sig.T a
selectBool =
   uncurry (zipWith3 (\xf xt c -> if c then xt else xf))
{-
   zipWithMatch (\(xf,xt) c -> if c then xt else xf) .
   uncurry (zipWithMatch (,))
-}


select :: Ix i => Array i (Sig.T a) -> Sig.T i -> Sig.T a
select arr =
   zipWith (!)
      (map (fmap head) $ iterate (fmap tail) arr)



{- |
Given a list of signals with time stamps,
mix them into one signal as they occur in time.
Ideally for composing music.

Cf. 'MathObj.LaurentPolynomial.series'
-}
arrange :: (Additive.C v) =>
       EventList.T NonNeg.Int (Sig.T v)
            {-^ A list of pairs: (relative start time, signal part),
                The start time is relative to the start time
                of the previous event. -}
    -> Sig.T v
            {-^ The mixed signal. -}
arrange evs =
   let xs = EventList.getBodies evs
   in  case map NonNeg.toNumber (EventList.getTimes evs) of
          t:ts -> replicate t zero ++ Laurent.addShiftedMany ts xs
          []   -> []
