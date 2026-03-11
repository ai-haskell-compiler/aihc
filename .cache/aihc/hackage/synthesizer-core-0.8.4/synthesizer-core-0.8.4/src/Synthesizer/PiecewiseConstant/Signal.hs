{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.PiecewiseConstant.Signal (
   T,
   StrictTime,
   ShortStrictTime,
   LazyTime,
   subdivideLazy,
   subdivideLazyToShort,
   subdivideLongStrict,
   chopLongTime,
   longFromShortTime,
   zipWith,
   ) where

import Synthesizer.PiecewiseConstant.Private
         (StrictTime, ShortStrictTime, chopLongTime)

import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.BodyTime  as EventListBT

import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Chunky as NonNegChunky
import Numeric.NonNegative.Class ((-|), )

import Control.Monad.Trans.State (evalState, get, put, )
import Data.Traversable (traverse, )

import qualified Data.List as List
import Data.Maybe.HT (toMaybe, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (zipWith, )
import qualified Prelude as P


type LazyTime = NonNegChunky.T StrictTime

type T = EventListBT.T StrictTime


{-# INLINE subdivideLazy #-}
subdivideLazy ::
   (NonNeg.C time) =>
   EventListBT.T (NonNegChunky.T time) body ->
   EventListBT.T time body
subdivideLazy =
   EventListBT.foldrPair
      (\y lt r ->
         List.foldr
            (\dt ->
               EventListMT.consBody y .
               EventListMT.consTime dt) r $
         NonNegChunky.toChunks (NonNegChunky.normalize lt))
      EventListBT.empty

{- |
Subdivide lazy times into chunks that fit into the number range
representable by @Int@.
-}
{-# INLINE subdivideLazyToShort #-}
subdivideLazyToShort ::
   EventListBT.T LazyTime y -> EventListBT.T ShortStrictTime y
subdivideLazyToShort =
   subdivideLazy .
   EventListBT.mapTime
      (NonNegChunky.fromChunks .
       List.concatMap chopLongTime .
       NonNegChunky.toChunks)

{-# INLINE longFromShortTime #-}
longFromShortTime :: ShortStrictTime -> StrictTime
longFromShortTime =
   NonNegW.fromNumberMsg "longFromShortTime" .
   fromIntegral .
   NonNegW.toNumber


{-# INLINE subdivideLongStrict #-}
subdivideLongStrict ::
   EventListBT.T StrictTime y -> EventListBT.T ShortStrictTime y
subdivideLongStrict =
   subdivideLazy .
   EventListBT.mapTime
      (NonNegChunky.fromChunks . chopLongTime)


_subdivideMaybe ::
   EventListBT.T LazyTime y -> EventListBT.T StrictTime (Maybe y)
_subdivideMaybe =
   EventListBT.foldrPair
      (\y lt r ->
         case NonNegChunky.toChunks (NonNegChunky.normalize lt) of
            [] -> r
            (t:ts) ->
               EventListBT.cons (Just y) t $
               List.foldr (EventListBT.cons Nothing) r ts)
      EventListBT.empty

{- |
When a lazy time value is split into chunks
then do not just replicate the sample for the whole time,
but insert 'Nothing's.
-}
{-# INLINE subdivideMaybe #-}
subdivideMaybe ::
   EventListTT.T LazyTime y ->
   EventListTT.T StrictTime (Maybe y)
subdivideMaybe =
   EventListTT.foldr
      (\lt r ->
         uncurry EventListMT.consTime $
         case NonNegChunky.toChunks (NonNegChunky.normalize lt) of
            [] ->
               (NonNegW.fromNumber zero, r)
            (t:ts) ->
               (t, List.foldr (EventListBT.cons Nothing) r ts))
      (\y r -> EventListMT.consBody (Just y) r)
      EventListBT.empty

{-# INLINE unionMaybe #-}
unionMaybe ::
   EventListTT.T StrictTime (Maybe y) ->
   EventListTT.T LazyTime y
unionMaybe =
   EventListTT.foldr
      (\t ->
         EventListMT.mapTimeHead
            (NonNegChunky.fromChunks . (t:) . NonNegChunky.toChunks))
      (\my ->
         case my of
            Nothing -> id
            Just y ->
               EventListMT.consTime NonNegChunky.zero .
               EventListMT.consBody y)
      (EventListTT.pause NonNegChunky.zero)

zipWithCore ::
   (NonNeg.C time) =>
   (a -> b -> c) ->
   a -> b ->
   EventListTT.T time (Maybe a) ->
   EventListTT.T time (Maybe b) ->
   EventListTT.T time (Maybe c)
zipWithCore f =
   let switch ac ar g =
          flip (EventListMT.switchBodyL EventListBT.empty) ar $ \am ar1 ->
          g (maybe (False,ac) ((,) True) am) ar1
       cont j ac bc as bs =
          EventListMT.consBody (toMaybe j $ f ac bc) $
          recourse ac bc as bs
       recourse ac bc as bs =
          flip EventListMT.switchTimeL as $ \at ar ->
          flip EventListMT.switchTimeL bs $ \bt br ->
          let ct = min at bt
          in  -- ToDo: redundant comparison of 'at' and 'bt'
              EventListMT.consTime ct $
              case compare at bt of
                 LT ->
                    switch ac ar $ \(ab,a) ar1 ->
                       cont ab a bc ar1 (EventListMT.consTime (bt-|ct) br)
                 GT ->
                    switch bc br $ \(bb,b) br1 ->
                       cont bb ac b (EventListMT.consTime (at-|ct) ar) br1
                 EQ ->
                    switch ac ar $ \(ab,a) ar1 ->
                    switch bc br $ \(bb,b) br1 ->
                       cont (ab||bb) a b ar1 br1
   in  recourse

zipWith ::
   (NonNeg.C time) =>
   (a -> b -> c) ->
   EventListBT.T time a ->
   EventListBT.T time b ->
   EventListBT.T time c
zipWith f as0 bs0 =
   flip (EventListMT.switchBodyL EventListBT.empty) as0 $ \a0 as1 ->
   flip (EventListMT.switchBodyL EventListBT.empty) bs0 $ \b0 bs1 ->
   let c0 = f a0 b0
   in  EventListMT.consBody c0 $
       flip evalState c0 $
       traverse (\mc -> maybe (return ()) put mc >> get) $
       zipWithCore f a0 b0 (fmap Just as1) (fmap Just bs1)

_zipWithLazy ::
   (a -> b -> c) ->
   EventListBT.T LazyTime a ->
   EventListBT.T LazyTime b ->
   EventListBT.T LazyTime c
_zipWithLazy f as0 bs0 =
   flip (EventListMT.switchBodyL EventListBT.empty) as0 $ \a0 as1 ->
   flip (EventListMT.switchBodyL EventListBT.empty) bs0 $ \b0 bs1 ->
   EventListMT.consBody (f a0 b0) $ unionMaybe $
   zipWithCore f a0 b0 (subdivideMaybe as1) (subdivideMaybe bs1)
{-
*Synthesizer.PiecewiseConstant.ALSA.MIDI Data.EventList.Relative.MixedTime> zipWithLazy (,) ('a' ./ 2 /. 'b' ./ 7 /. EventListBT.empty) ('c' ./ (1 P.+ 1) /. 'd' ./ 1 /. EventListBT.empty)
-}
