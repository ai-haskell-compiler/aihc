module Synthesizer.PiecewiseConstant.Private where

import qualified Synthesizer.Generic.Signal as SigG

import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.TimeBody  as EventList

import qualified Numeric.NonNegative.Wrapper as NonNegW

import Control.Monad.Trans.State (evalState, get, put, )

import qualified Data.List as List
import Data.Traversable (traverse, )
import Data.Foldable (traverse_, )



type StrictTime = NonNegW.Integer
type ShortStrictTime = NonNegW.Int


{-# INLINE toSignal #-}
toSignal ::
   (SigG.Transform sig y) =>
   (StrictTime -> y -> sig y) ->
   EventListBT.T StrictTime y -> sig y
toSignal replicateLong =
   EventListBT.foldrPair
      (\y t -> SigG.append (replicateLong t y))
      SigG.empty

{-# INLINE toSignalInit #-}
toSignalInit ::
   (SigG.Transform sig y) =>
   (StrictTime -> y -> sig y) ->
   y -> EventList.T StrictTime y -> sig y
toSignalInit replicateLong initial =
   (\ ~(t,rest) -> SigG.append (replicateLong t initial) rest)
   .
   EventList.foldr
      (,)
      (\y ~(t,rest) -> SigG.append (replicateLong t y) rest)
      (0, SigG.empty)
{-
   toSignal .
--   EventListBM.switchBodyR const .
--   EventListBM.snocTime NonNeg.zero .
--   EventListMB.consBody initial .
   -- switchBodyR causes a space leak
   EventListTM.switchBodyR EventListBT.empty
      (\xs _ -> EventListMT.consBody initial xs)
-}

{-# INLINE toSignalInitWith #-}
toSignalInitWith ::
   (SigG.Transform sig c) =>
   (StrictTime -> c -> sig c) ->
   (y -> c) -> c -> EventList.T StrictTime [y] -> sig c
toSignalInitWith replicateLong f initial =
   toSignalInit replicateLong initial .
   flip evalState initial .
   traverse (\evs -> traverse_ (put . f) evs >> get)


{- |
Returns a list of non-zero times.
-}
{-# INLINE chopLongTime #-}
chopLongTime :: StrictTime -> [ShortStrictTime]
chopLongTime n =
   let d = fromIntegral (maxBound :: Int)
       (q,r) = divMod (NonNegW.toNumber n) d
   in  map (NonNegW.fromNumberMsg "chopLongTime" . fromInteger) $
       List.genericReplicate q d ++
       if r/=0 then [r] else []
