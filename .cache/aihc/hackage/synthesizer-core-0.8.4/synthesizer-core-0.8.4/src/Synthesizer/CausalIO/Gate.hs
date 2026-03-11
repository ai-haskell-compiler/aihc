module Synthesizer.CausalIO.Gate (
   Chunk(Chunk), chunk,
   allToStorableVector,
   toStorableVector,
   allToChunkySize,
   toChunkySize,
   shorten,
   ) where

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Zip as Zip

import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Generic.Signal as SigG
import Synthesizer.PiecewiseConstant.Signal (StrictTime, )

import qualified Control.Monad.Trans.State as MS
import Control.Arrow (Arrow, arr, (^<<), )
import Control.Monad (when, )

import qualified Data.StorableVector as SV
import qualified Data.Monoid as Mn
import qualified Data.Semigroup as Sg
import Data.Maybe.HT (toMaybe, )

import qualified Numeric.NonNegative.Class as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{- |
Chunk represents a chunk of a Gate signal.

It means (Chunk chunkDuration sustainDuration).

sustainDuration means:
Just (t,a) -
   key is released at time t with attribute a,
   e.g. the note-off-velocity,
   t must be smaller than chunkDuration!
Nothing - key is in pressed or released state over the whole chunk
-}
data Chunk a = Chunk StrictTime (Maybe (StrictTime, a))
   deriving (Show)

-- | smart constructor that checks the time constraints
chunk :: StrictTime -> Maybe (StrictTime, a) -> Chunk a
chunk dur mrel =
   if maybe True (\(rel,_attr) -> rel<dur) mrel
     then Chunk dur mrel
     else error "release time must be strictly before chunk end"


instance CutG.Read (Chunk a) where
   null (Chunk dur _) = isZero dur
   length (Chunk dur _) = fromIntegral dur

instance CutG.NormalForm (Chunk a) where
   evaluateHead (Chunk dur rel) =
      dur `seq` rel `seq` ()

instance Sg.Semigroup (Chunk a) where
   (<>) = error "Gate.mappend cannot be defined"

instance Mn.Monoid (Chunk a) where
   mempty = error "Gate.mempty cannot be defined"
   mappend = (Sg.<>)

instance CutG.Transform (Chunk a) where
   take n (Chunk dur mrel) =
      let nn = NonNegW.fromNumberClip $ fromIntegral n
      in  Chunk (min nn dur) $
          mrel >>= \rel -> toMaybe (fst rel < nn) rel
   drop n (Chunk dur mrel) =
      let nn = NonNegW.fromNumberClip $ fromIntegral n
      in  Chunk (dur NonNeg.-| nn) $
          mrel >>= \(rel,attr) -> toMaybe (nn <= rel) (rel-nn, attr)
   splitAt n c = (CutG.take n c, CutG.drop n c)
   dropMarginRem = error "Gate.dropMarginRem is not defined"
   reverse = error "Gate.reverse cannot be defined"


allToStorableVector ::
   (Arrow arrow) =>
   arrow (Chunk a) (SV.Vector ())
allToStorableVector = arr $
   (\(SigG.LazySize n) -> SV.replicate n ())
   ^<<
   allToChunkySize

toStorableVector ::
   PIO.T (Chunk a) (SV.Vector ())
toStorableVector =
   (\(SigG.LazySize n) -> SV.replicate n ())
   ^<<
   toChunkySize


allToChunkySize ::
   (Arrow arrow) =>
   arrow (Chunk a) SigG.LazySize
allToChunkySize = arr $
   \(Chunk time _) -> SigG.LazySize (fromIntegral time)

toChunkySize ::
   PIO.T (Chunk a) SigG.LazySize
toChunkySize =
   PIO.traverse True $
   \(Chunk time mRelease) -> do
      running <- MS.get
      if not running
        then return $ SigG.LazySize 0
        else
          case mRelease of
             Nothing ->
                return $ SigG.LazySize (fromIntegral time)
             Just (relTime, _) -> do
                MS.put False
                return $ SigG.LazySize (fromIntegral relTime)


{- |
Pass the second signal while the gate is open.

For completeness we would need a data type analogously to ChunkySize,
that measures signal duration in CausalIO processes.
'shorten' could then be written as

> shorten = Zip.second ^<< Zip.arrowFirstShort Gate.toChunkySize
-}
shorten ::
   (CutG.Transform signal) =>
   PIO.T (Zip.T (Chunk a) signal) signal
shorten =
   PIO.traverse True $
   \(Zip.Cons (Chunk time mRelease) sig) -> do
      when (NonNegW.toNumber time /= fromIntegral (CutG.length sig))
         (error "Gate.shorten: durations mismatch")
      running <- MS.get
      if not running
        then return CutG.empty
        else
          case mRelease of
             Nothing ->
                return $ sig
             Just (relTime, _) -> do
                MS.put False
                return $ CutG.take (fromIntegral relTime) sig
