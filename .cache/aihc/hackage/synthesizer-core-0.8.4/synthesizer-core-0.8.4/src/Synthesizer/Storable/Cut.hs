module Synthesizer.Storable.Cut (
   arrange,

   -- for MIDI.CausalIO.Process
   addChunkToBuffer,

   -- for testing
   arrangeEquidist,
   arrangeAdaptive,
   arrangeList,
   ) where

import qualified Synthesizer.Storable.Signal as Sig

import qualified Data.StorableVector           as SV
import qualified Data.StorableVector.Lazy      as SVL
import qualified Data.StorableVector.ST.Strict as SVST
import Foreign.Storable (Storable)

import Control.Monad.ST.Strict (ST, runST, )
import Control.Monad.Trans.State (runState, modify, gets, put, )

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Absolute.TimeBody  as AbsEventList
import Data.Tuple.HT (mapSnd, )

import qualified Algebra.Additive as Additive
import qualified Number.NonNegative as NonNeg

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{-# INLINE arrange #-}
arrange :: (Storable v, Additive.C v) =>
       Sig.ChunkSize
    -> EventList.T NonNeg.Int (Sig.T v)
            {-^ A list of pairs: (relative start time, signal part),
                The start time is relative to the start time
                of the previous event. -}
    -> Sig.T v
            {-^ The mixed signal. -}
arrange =
   arrangeEquidist

{- |
Chunk sizes are adapted to the time differences.
Explicit ChunkSize parameter is only required for zero padding.
Since no ST monad is needed, this can be generalized to Generic.Signal.Transform class.
-}
arrangeAdaptive :: (Storable v, Additive.C v) =>
       Sig.ChunkSize
    -> EventList.T NonNeg.Int (Sig.T v)
            {-^ A list of pairs: (relative start time, signal part),
                The start time is relative to the start time
                of the previous event. -}
    -> Sig.T v
            {-^ The mixed signal. -}
arrangeAdaptive size =
   uncurry Sig.append .
   flip runState Sig.empty .
   fmap (Sig.concat . EventList.getTimes) .
   EventList.mapM
      (\timeNN ->
           let time = NonNeg.toNumber timeNN
           in  do (prefix,suffix) <- gets (Sig.splitAtPad size time)
                  put suffix
                  return prefix)
      (\body ->
           modify (Sig.mixSndPattern body))


{- |
This function also uses the time differences as chunk sizes,
but may occasionally use smaller chunk sizes due to the chunk structure
of an input signal until the next signal starts.
-}
arrangeList :: (Storable v, Additive.C v) =>
       Sig.ChunkSize
    -> EventList.T NonNeg.Int (Sig.T v)
            {-^ A list of pairs: (relative start time, signal part),
                The start time is relative to the start time
                of the previous event. -}
    -> Sig.T v
            {-^ The mixed signal. -}
arrangeList size evs =
   let xs = EventList.getBodies evs
   in  case EventList.getTimes evs of
          t:ts -> Sig.replicate size (NonNeg.toNumber t) zero `Sig.append`
                  addShiftedMany size ts xs
          []   -> Sig.empty


addShiftedMany :: (Storable a, Additive.C a) =>
   Sig.ChunkSize -> [NonNeg.Int] -> [Sig.T a] -> Sig.T a
addShiftedMany size ds xss =
   foldr (uncurry (addShifted size)) Sig.empty (zip (ds++[0]) xss)


{-
It is crucial that 'mix' uses the chunk size structure of the second operand.
This way we avoid unnecessary and even infinite look-ahead.
-}
addShifted :: (Storable a, Additive.C a) =>
   Sig.ChunkSize -> NonNeg.Int -> Sig.T a -> Sig.T a -> Sig.T a
addShifted size delNN px py =
   let del = NonNeg.toNumber delNN
   in  uncurry Sig.append $
       mapSnd (flip Sig.mixSndPattern py) $
       Sig.splitAtPad size del px


{-
arrangeEquidist (Sig.chunkSize 2) (EventList.fromPairList [(10, SVL.pack SVL.defaultChunkSize [1..8::Double]), (2, SVL.pack (Sig.chunkSize 2) $ [4,3,2,1::Double] ++ undefined)])
-}
{- |
The result is a Lazy StorableVector with chunks of the given size.
-}
{-# INLINE arrangeEquidist #-}
arrangeEquidist :: (Storable v, Additive.C v) =>
       Sig.ChunkSize
    -> EventList.T NonNeg.Int (Sig.T v)
            {-^ A list of pairs: (relative start time, signal part),
                The start time is relative to the start time
                of the previous event. -}
    -> Sig.T v
            {-^ The mixed signal. -}
arrangeEquidist (SVL.ChunkSize sz) =
   let sznn = NonNeg.fromNumberMsg "arrangeEquidist" sz
       go acc evs =
          let (now,future) = EventListTM.splitAtTime sznn evs
              xs =
                 AbsEventList.toPairList $
                 EventList.toAbsoluteEventList 0 $
                 EventListTM.switchTimeR const now
              (chunk,newAcc) =
                 runST
                    (do v <- SVST.new sz zero
                        newAcc0 <- mapM (addToBuffer v 0) acc
--                        newAcc1 <- AbsEventList.mapM (addToBuffer v) xs
                        newAcc1 <-
                           mapM (\(i,s) -> addToBuffer v (NonNeg.toNumber i) s) xs
                        vf <- SVST.freeze v
                        return (vf, newAcc0++newAcc1))
              (ends, suffixes) = unzip $ newAcc
              prefix =
                 {- if there are more events to come,
                    we must pad with zeros -}
                 if EventList.null future
                   then SV.take (foldl max 0 ends) chunk
                   else chunk
          in  if SV.null prefix
                then []
                else prefix : go (filter (not . Sig.null) suffixes) future
   in  Sig.fromChunks . go []


{-
{-# INLINE addToBuffer #-}
addToBuffer :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> Sig.T a -> ST s (Int, Sig.T a)
addToBuffer v start =
   let n = SVST.length v
       go i [] = return (i, [])
       go i (c:cs) =
          let end = i + SV.length c
          in  addChunkToBuffer v i c >>
              if end<n
                then go end cs
                else return (n, SV.drop (end-n) c : cs)
   in  fmap (mapSnd SigSt.fromChunks) . go start . SigSt.chunks

addChunkToBuffer :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> SV.Vector a -> ST s ()
addChunkToBuffer v start xs =
   let n = SVST.length v
   in  SV.foldr
          (\x continue i ->
             SVST.modify v i (x +) >>
             continue (succ i))
          (\_i -> return ())
          (Sig.take (n Additive.- start) xs)
          start
-}

{-# INLINE addToBuffer #-}
addToBuffer :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> Sig.T a -> ST s (Int, Sig.T a)
addToBuffer v start xs =
   let n = SVST.length v
       (now,future) = Sig.splitAt (n Additive.- start) xs
       go i [] = return i
       go i (c:cs) =
          unsafeAddChunkToBuffer v i c >>
          go (i + SV.length c) cs
   in  fmap (flip (,) future) . go start . Sig.chunks $ now

{-# INLINE addChunkToBuffer #-}
addChunkToBuffer :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> SV.Vector a -> ST s ()
addChunkToBuffer v start xs =
   if start + SV.length xs <= SVST.length v
     then unsafeAddChunkToBuffer v start xs
     else error "Storable.addChunkToBuffer: chunk too large"

{- | chunk must fit into the buffer -}
{- This implementation will be faster as long as 'SV.foldr' is inefficient. -}
{-# INLINE unsafeAddChunkToBuffer #-}
unsafeAddChunkToBuffer :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> SV.Vector a -> ST s ()
unsafeAddChunkToBuffer v start xs =
   let go i j =
          if j >= SV.length xs
            then return ()
            else
              SVST.unsafeModify v i (SV.index xs j +) >>
              go (i + 1) (j + 1)
   in  go start 0

{- | chunk must fit into the buffer -}
{-# INLINE _unsafeAddChunkToBufferFoldr #-}
_unsafeAddChunkToBufferFoldr :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> SV.Vector a -> ST s ()
_unsafeAddChunkToBufferFoldr v start xs =
   SV.foldr
      (\x continue i ->
         SVST.unsafeModify v i (x +) >>
         continue (succ i))
      (\_i -> return ())
      xs start


-- most elegant solution, but slow because StorableVector.foldr is slow
{-# INLINE _addToBufferFoldr #-}
_addToBufferFoldr :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> Sig.T a -> ST s (Int, Sig.T a)
_addToBufferFoldr v start xs =
   let n = SVST.length v
       (now,future) = Sig.splitAt (n Additive.- start) xs
   in  Sig.foldr
          (\x continue i ->
             SVST.modify v i (x +) >>
             continue (succ i))
          (\i -> return (i, future))
          now start


{-
Using @Sig.switchL@ in an inner loop
is slower than using @Sig.foldr@.
Using a StorableVectorPointer would be faster,
but I think still slower than @foldr@.
-}
_addToBufferSwitchL :: (Storable a, Additive.C a) =>
   SVST.Vector s a -> Int -> Sig.T a -> ST s (Int, Sig.T a)
_addToBufferSwitchL v start =
   let n = SVST.length v
       {-# INLINE go #-}
       go i =
          if i>=n
            then return . (,) i
            else
              Sig.switchL
                 (return (i, Sig.empty))
                 (\x xs ->
                     SVST.modify v i (x +) >>
                     go (succ i) xs)
   in  go start
