{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Causal.Analysis where

import qualified Synthesizer.Causal.Filter.Recursive.Integration as Integration

import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Plain.Analysis as Ana

import qualified Algebra.RealRing              as RealRing

import Control.Arrow (second, (^<<), (<<^), )

import qualified Data.Map as Map

import NumericPrelude.Numeric
import NumericPrelude.Base


flipFlopHysteresis ::
   (Ord y) => (y,y) -> Ana.BinaryLevel -> Causal.T y Ana.BinaryLevel
flipFlopHysteresis bnds = Causal.scanL (Ana.flipFlopHysteresisStep bnds)

deltaSigmaModulation ::
   RealRing.C y => Causal.T y Ana.BinaryLevel
deltaSigmaModulation =
   Causal.feedback
      ((Ana.binaryLevelFromBool . (zero <=)) ^<<
       Integration.run <<^
       uncurry (-))
      (Causal.consInit zero <<^ Ana.binaryLevelToNumber)

deltaSigmaModulationPositive ::
   RealRing.C y => Causal.T (y, y) y
deltaSigmaModulationPositive =
   Causal.feedback
      ((\(threshold,xi) -> if threshold<=xi then threshold else zero) ^<<
       second Integration.run <<^
       (\((threshold,xi),cum) -> (threshold,xi-cum)))
      (Causal.consInit zero)


{-
Abuse (Map a ()) as (Set a),
because in GHC-7.4.2 there is no Set.elemAt function.
-}
movingMedian :: (Ord a) => Int -> Causal.T a a
movingMedian n =
   Causal.mapAccumL
      (\new (k,queue,oldSet) ->
         let set =
               Map.insert (new,k) () $
               maybe id (\old -> Map.delete (old,k)) (Map.lookup k queue) oldSet
         in  (fst $ fst $ Map.elemAt (div (Map.size set) 2) set,
              (mod (k+1) n, Map.insert k new queue, set)))
      (0, Map.empty, Map.empty)
