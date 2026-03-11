module Reactive.Banana.MIDI.Training (
   all,
   intervals,
   twoNotes,
   threeNotes,
   reverseThreeNotes,
   sortThreeNotes,
   transposeTwoNotes,
   ) where

import qualified Reactive.Banana.MIDI.Pitch as Pitch
import Reactive.Banana.MIDI.Common (pitch, )
import Sound.MIDI.Message.Channel.Voice (Pitch, )

import System.Random (RandomGen, Random, randomR, )
import Control.Monad.Trans.State (State, state, evalState, )
import Control.Monad (liftM2, )
import Data.Maybe (mapMaybe, )
import qualified Data.List as List
import Prelude hiding (all, )


{- | chose a random item from a list -}
-- from htam
randomItem :: (RandomGen g) => [a] -> State g a
randomItem x = fmap (x!!) (randomRState (length x - 1))

randomRState :: (RandomGen g, Random a, Num a) => a -> State g a
randomRState upper = state (randomR (0, upper))


baseKey :: Pitch
baseKey = pitch 60

notes :: [Pitch]
notes =
   mapMaybe (flip Pitch.increase baseKey)
   [0, 12, 7, 5, 4, 2, 9, 11, 3, 10, 1, 6, 8]


all :: RandomGen g => g -> [([Pitch], [Pitch])]
all g =
   intervals g ++ twoNotes g ++ threeNotes g ++
   reverseThreeNotes g ++ sortThreeNotes g ++
   transposeTwoNotes g

-- | intervals within an octave, all starting with a C
intervals :: RandomGen g => g -> [([Pitch], [Pitch])]
intervals g =
   flip evalState g $
   mapM randomItem $
   concat $ zipWith replicate [3,6..] $
   drop 3 $ List.inits $
   map (\p -> let ps = [baseKey, p] in (ps, ps)) $
   notes

-- | choose two arbitrary notes from an increasing set of notes
twoNotes :: RandomGen g => g -> [([Pitch], [Pitch])]
twoNotes g =
   flip evalState g $
   mapM (\ps ->
      fmap (\pso -> (pso,pso)) $
      mapM randomItem [ps,ps]) $
   concat $ zipWith replicate [3,6..] $
   drop 3 $ List.inits $
   notes

-- | choose three arbitrary notes from an increasing set of notes
threeNotes :: RandomGen g => g -> [([Pitch], [Pitch])]
threeNotes g =
   flip evalState g $
   mapM (\ps ->
      fmap (\pso -> (pso,pso)) $
      mapM randomItem [ps,ps,ps]) $
   concat $ zipWith replicate [3,6..] $
   drop 3 $ List.inits $
   notes

reverseThreeNotes :: RandomGen g => g -> [([Pitch], [Pitch])]
reverseThreeNotes g =
   flip evalState g $
   mapM (\ps ->
      fmap (\pso -> (pso, reverse pso)) $
      mapM randomItem [ps,ps,ps]) $
   concat $ zipWith replicate [3,6..] $
   drop 3 $ List.inits $
   notes

sortThreeNotes :: RandomGen g => g -> [([Pitch], [Pitch])]
sortThreeNotes g =
   flip evalState g $
   mapM (\ps ->
      fmap (\pso -> (pso, List.sort pso)) $
      mapM randomItem [ps,ps,ps]) $
   concat $ zipWith replicate [3,6..] $
   drop 3 $ List.inits $
   notes

-- | transpose an interval to begin with C
transposeTwoNotes :: RandomGen g => g -> [([Pitch], [Pitch])]
transposeTwoNotes g =
   flip evalState g $
   mapM (\ps ->
      liftM2
         (\p0 p1 ->
            let pso = [p0,p1]
            in  (pso, mapMaybe (Pitch.increase (Pitch.subtract p0 baseKey)) pso))
         (randomItem ps) (randomItem ps)) $
   concat $ zipWith replicate [3,6..] $
   drop 3 $ List.inits $
   notes
