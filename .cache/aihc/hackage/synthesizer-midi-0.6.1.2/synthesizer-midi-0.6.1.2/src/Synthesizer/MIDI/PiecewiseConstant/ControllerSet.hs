{- |
Treat a stream of MIDI events as parallel streams of MIDI controller events.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.MIDI.PiecewiseConstant.ControllerSet (
   T(Cons),
   mapStream,

   Controller(Controller,PitchBend,Pressure),
   fromChannel,
   maybeController,
   controllerLinear,
   controllerExponential,
   pitchBend,
   channelPressure,
   bendWheelPressure,
   checkBendWheelPressure,
   bendWheelPressureZip,

   -- * internal data needed in synthesizer-llvm
   initial, stream,
   ) where

import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.MIDI.EventList as Ev
import Synthesizer.MIDI.EventList (StrictTime, Channel, )

import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Synthesizer.MIDI.Value.BendModulation as BM
import qualified Synthesizer.MIDI.Value.BendWheelPressure as BWP
import qualified Synthesizer.MIDI.Value as MV

import qualified Synthesizer.Generic.Cut as CutG
import Control.DeepSeq (NFData, rnf, )

import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.BodyTime  as EventListBT
-- import qualified Data.EventList.Relative.TimeBody  as EventListTB

import qualified Numeric.NonNegative.Class   as NonNeg98
-- import qualified Numeric.NonNegative.Wrapper as NonNegW
-- import qualified Numeric.NonNegative.Chunky as NonNegChunky
-- import Numeric.NonNegative.Class ((-|), )

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Additive       as Additive

import qualified Data.Map as Map
import Data.Map (Map, )

import qualified Data.Accessor.Monad.Trans.State as AccState
import qualified Data.Accessor.Basic as Acc
import Control.Monad.Trans.State (State, evalState, state, get, put, )
import Control.Monad (liftM2, msum, )
import Data.Traversable (traverse, )
import Data.Foldable (traverse_, )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

import Data.Maybe.HT (toMaybe, )
import Data.Tuple.HT (mapFst, mapPair, )
import qualified Data.List.HT as ListHT
import qualified Data.List as List

import NumericPrelude.Numeric
import NumericPrelude.Base
import qualified Prelude as P


{-
This data structure stores the initial values of all supported controllers
and an event list of all changes of individal controllers.
-}
data T key a =
   Cons {
      initial :: Map key a,
      stream :: EventListTT.T StrictTime [(key, a)]
   }
   deriving Show


mapStream ::
   (EventListTT.T StrictTime [(key, a)] ->
    EventListTT.T StrictTime [(key, a)]) ->
   T key a -> T key a
mapStream f s = Cons (initial s) (f (stream s))


data Controller =
     Controller VoiceMsg.Controller
   | PitchBend
   | Pressure
   deriving (Show, Eq, Ord)

instance NFData Controller where
   rnf (Controller c) =
      rnf (VoiceMsg.fromController c)
   rnf _ = ()


fromChannel ::
   (Check.C event) =>
   Channel ->
   Ev.Filter event (T Controller Int)
fromChannel chan =
   fmap (Cons Map.empty) $
   fmap (flip EventListTM.snocTime NonNeg98.zero) $
   Ev.getSlice (maybeController chan)

maybeController ::
   (Check.C event) =>
   Channel -> event -> Maybe (Controller, Int)
maybeController chan e = msum $
   (fmap (mapFst Controller) $ Check.anyController chan e) :
   (fmap ((,) PitchBend) $ Check.pitchBend chan e) :
   (fmap ((,) Pressure) $ Check.channelPressure chan e) :
   []


instance CutG.Read (T key a) where
   null =
      List.null . List.filter (> P.fromInteger 0) .
      EventListTT.getTimes . stream
   length =
      fromIntegral . P.toInteger .
      P.sum . EventListTT.getTimes . stream

instance Semigroup (T key y) where
   x <> y =
      Cons
         (initial x)
         (EventListTT.append (stream x) (flatten y))

instance Monoid (T key y) where
   mempty = Cons Map.empty (EventListTT.pause mempty)
   mappend = (<>)

instance (NFData key, NFData a) => CutG.NormalForm (T key a) where
   evaluateHead xs = rnf (initial xs)

{- |
Prepend the initial values as events to the event-list.
-}
flatten ::
   T key a -> EventListTT.T StrictTime [(key, a)]
flatten xs =
   EventListTT.cons
      mempty (Map.toList $ initial xs)
      (stream xs)


mapInsertMany ::
   (Ord key) =>
   [(key,a)] -> Map key a -> Map key a
mapInsertMany assignments inits =
   foldl (flip (uncurry Map.insert)) inits assignments


reverseList ::
   (Ord key) =>
   (Map key a, [(key,a)]) ->
   (Map key a, [(key,a)])
reverseList (inits,xs) =
   foldl
      (\(inits0,ys) (key,a) ->
         let (ma,inits1) =
                Map.insertLookupWithKey
                   (\ _k new _old -> new) key a inits0
         in  (inits1,
              maybe
                 (error "MIDIControllerSet.reverse: uninitialized controller")
                 ((,) key) ma
                 : ys))
      (inits, [])
      xs

{- |
For reverse you must make sure,
that all controller events have an corresponding initial value.
Controllers that miss an initial value
their last constant piece will be undefined.
-}
instance (Ord key) => CutG.Transform (T key y) where
   take n =
      mapStream (EventListTT.takeTime (P.fromIntegral n))

   drop n0 xs =
      let recourse n inits =
             EventListMT.switchTimeL $ \t xs1 ->
             let (b,d) = snd $ NonNeg98.split t n
             in  mapStream (EventListTT.forceTimeHead) $
                 if not b
                   then Cons inits (EventListMT.consTime d xs1)
                   else
                     flip (EventListMT.switchBodyL
                        (Cons inits (EventListTT.pause mempty))) xs1 $ \assignments xs2 ->
                     recourse d (mapInsertMany assignments inits) xs2
      in  recourse (P.fromIntegral n0) (initial xs) (stream xs)

   -- cf. ChunkySize.dropMarginRem
   dropMarginRem n m xs =
      List.foldl'
         (\(mi,xsi) k -> (mi-k, CutG.drop k xsi))
         (m, xs)
         (List.map P.fromIntegral $ EventListTT.getTimes $
          EventListTT.takeTime (P.fromIntegral m) $
          EventListTT.dropTime (P.fromIntegral n) $
          stream xs)

   -- cf. StorableVector.Lazy.splitAt
   splitAt n0 xs =
      let recourse n inits =
             EventListMT.switchTimeL $ \t xs1 ->
             let (m, ~(b,d)) = NonNeg98.split t n
             in  mapPair
                    (EventListMT.consTime m,
                     mapStream (EventListTT.forceTimeHead)) $
                 if not b
                   then
                     (EventListBT.empty,
                      Cons inits (EventListMT.consTime d xs1))
                   else
                     flip (EventListMT.switchBodyL
                        (EventListBT.empty,
                         Cons inits (EventListTT.pause mempty))) xs1 $ \keyAs xs2 ->
                     mapFst (EventListMT.consBody keyAs) $
                     recourse d (mapInsertMany keyAs inits) xs2
      in  mapFst (Cons (initial xs)) $
          recourse (P.fromIntegral n0) (initial xs) (stream xs)

   reverse xs =
      EventListTT.foldl
         (\(inits,ys) t -> Cons inits $ EventListMT.consTime t ys)
         (\(Cons inits0 ys) evs0 ->
            let (inits1, evs1) = reverseList (inits0, evs0)
            in  (inits1, EventListMT.consBody evs1 ys))
         (initial xs, EventListBT.empty)
         (stream xs)
{-
*Synthesizer.MIDI.PiecewiseConstant.ControllerSet Data.EventList.Relative.MixedTime> CutG.reverse $ Cons (Map.singleton 'a' GT) (2 /. [('a',EQ)] ./ 3 /. empty) :: T Char Ordering
-}



type Filter = State (T Controller Int)


_errorUninitialized :: Controller -> Int
_errorUninitialized c =
   error $
   "getSlice: uninitialized controller " ++ show c

{-# INLINE getSlice #-}
getSlice ::
   Controller ->
   (Int -> a) ->
   a -> Filter (PC.T a)
getSlice c f deflt =
   state $ \xs ->
      let (ys,zs) =
             EventListTT.unzip $
             fmap
                (ListHT.partitionMaybe
                   (\(ci,a) -> toMaybe (c==ci) a))
                (stream xs)
          (yin0,zis) =
             Map.updateLookupWithKey
                (\ _k _a -> Nothing) c
                (initial xs)
          yin1 = maybe deflt f yin0
          fill =
             flip evalState yin1 .
             traverse
                (\ys0 -> traverse_ (put . f) ys0 >> get)
      in  (EventListMT.consBody yin1 (fill ys),
           Cons zis zs)


{-# INLINE controllerLinear #-}
controllerLinear ::
   (Field.C y) =>
   Ev.Controller -> (y,y) -> y -> Filter (PC.T y)
controllerLinear ctrl bnd =
   getSlice (Controller ctrl) (MV.controllerLinear bnd)


{-# INLINE controllerExponential #-}
controllerExponential ::
   (Trans.C y) =>
   Ev.Controller -> (y,y) -> y -> Filter (PC.T y)
controllerExponential ctrl bnd =
   getSlice (Controller ctrl) (MV.controllerExponential bnd)



{- |
@pitchBend channel range center@:
emits frequencies on an exponential scale from
@center/range@ to @center*range@.
-}
{-# INLINE pitchBend #-}
pitchBend ::
   (Trans.C y) =>
   y -> y ->
   Filter (PC.T y)
pitchBend range center =
   getSlice PitchBend (MV.pitchBend range center) center

{-# INLINE channelPressure #-}
channelPressure ::
   (Trans.C y) =>
   y -> y ->
   Filter (PC.T y)
channelPressure maxVal =
   getSlice Pressure (MV.controllerLinear (Additive.zero,maxVal))



-- adapted from getSlice
{-# INLINE bendWheelPressure #-}
bendWheelPressure ::
   (RealRing.C y, Trans.C y) =>
   Int -> y -> y ->
   Filter (PC.T (BM.T y))
bendWheelPressure pitchRange wheelDepth pressDepth =
   state $ \xs ->
      let (ys,zs) =
             EventListTT.unzip $
             fmap ListHT.unzipEithers $
             flip evalState BWP.deflt $
             traverse (traverse separateBWP) (stream xs)
          move key field (bwp,mp) =
             mapFst (maybe bwp (\y -> Acc.set field y bwp)) $
             Map.updateLookupWithKey
                (\ _k _a -> Nothing) key mp
          (yin,zis) =
             move PitchBend BWP.bend $
             move (Controller VoiceMsg.modulation) BWP.wheel $
             move Pressure BWP.pressure $
             (BWP.deflt, initial xs)
          fill =
             flip evalState yin .
             traverse
                (\ys0 -> traverse_ put ys0 >> get)
      in  (fmap (BM.fromBendWheelPressure pitchRange wheelDepth pressDepth) $
           EventListMT.consBody yin (fill ys),
           Cons zis zs)

separateBWP ::
   (Controller, Int) -> State BWP.T (Either BWP.T (Controller, Int))
separateBWP ev =
   fmap (maybe (Right ev) Left) $
   checkBendWheelPressure ev

checkBendWheelPressure ::
   (Controller, Int) -> State BWP.T (Maybe BWP.T)
checkBendWheelPressure (ctrl,val) =
   let update field = AccState.set field val >> fmap Just get
   in  case ctrl of
          PitchBend -> update BWP.bend
          Pressure -> update BWP.pressure
          Controller cc ->
             if cc == VoiceMsg.modulation
               then update BWP.wheel
               else return $ Nothing


{-# INLINE bendWheelPressureZip #-}
bendWheelPressureZip ::
   (RealRing.C y, Trans.C y) =>
   Int -> y -> y ->
   Filter (PC.T (BM.T y))
bendWheelPressureZip pitchRange wheelDepth pressDepth =
   liftM2 (PC.zipWith BM.Cons)
      (pitchBend (2 ^? (fromIntegral pitchRange / 12)) 1)
      (liftM2 (PC.zipWith (+))
         (controllerLinear VoiceMsg.modulation (0,wheelDepth) 0)
         (channelPressure pressDepth 0))
