module Data.EventList.Utility where

-- State monad could be avoided by mapAccumL
import Control.Monad.Trans.State (State, state, modify, gets, )
import qualified Data.List as List
import Data.Tuple.HT (mapPair, )

{- |
Given the time fraction that remains from the preceding event
and the current time difference,
evaluate an integer time difference and
the remaining fractional part.
If we would simply map Time to integer values
with respect to the sampling rate,
then rounding errors would accumulate.
-}

roundDiff' :: (RealFrac t, Integral i) => t -> t -> (i, t)
roundDiff' time frac =
   let x = time+frac
       n = round x
   in  (n, x - fromIntegral n)

roundDiff :: (RealFrac t, Integral i) => t -> State t i
roundDiff = state . roundDiff'

{-
We could use 'properFraction' but this is inconsistent for negative values.
-}
floorDiff :: (RealFrac t, Integral i) => t -> State t i
floorDiff t =
   do modify (t+)
      n <- gets floor
      modify (subtract (fromIntegral n))
      return n


beforeBy :: (Ord time) =>
   (body -> body -> Bool) ->
   (time, body) -> (time, body) -> Bool
beforeBy before (t0, me0) (t1, me1) =
   case compare t0 t1 of
      LT -> True
      EQ -> before me0 me1
      GT -> False


slice :: (Eq a) =>
   (eventlist -> Maybe body) ->
   ((body -> Bool) -> eventlist -> (eventlist, eventlist)) ->
   (body -> a) -> eventlist -> [(a, eventlist)]
slice hd partition f =
   List.unfoldr (\ pf ->
      fmap
         ((\ i ->
            mapPair
               ((,) i, id)
               (partition ((i==) . f) pf)) . f)
         (hd pf))
