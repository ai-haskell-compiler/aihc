module Test.Sound.Synthesizer.Plain.Interpolation (
   T, ip,
   LinePreserving, lpIp,
   tests,
   use, useLP, use2,
   -- only for debugging
   frequencyModulationBackCompare,
   frequencyModulationForth0Compare,
   frequencyModulationStorableChunkSizeCompare,
   frequencyModulationStorableCompare,
   ) where

import qualified Synthesizer.Plain.Interpolation as Interpolation
import qualified Synthesizer.Interpolation.Class as Interpol
import qualified Synthesizer.Interpolation.Custom as ExampleCustom
import qualified Synthesizer.Interpolation.Module as ExampleModule

import qualified Synthesizer.Causal.Interpolation as InterpolC
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Filter.NonRecursive as FiltS
import qualified Synthesizer.State.Signal as SigS

import qualified Synthesizer.Storable.Filter.NonRecursive as FiltSt
import qualified Synthesizer.Storable.Signal as SigSt

import Test.QuickCheck (quickCheck, Arbitrary(arbitrary), elements, Testable, )

import Foreign.Storable (Storable, )

import qualified Algebra.VectorSpace           as VectorSpace
import qualified Algebra.Module                as Module
import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing

import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapSnd, )

import qualified Test.Sound.Synthesizer.Plain.NonEmpty as NonEmpty
import Test.Utility (ArbChar, unpackArbString)


import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



use ::
   (Interpolation.T a v -> x) ->
   (T a v -> x)
use f ipt =
   f (ip ipt)

useLP ::
   (Interpolation.T a v -> x) ->
   (LinePreserving a v -> x)
useLP f ipt =
   f (lpIp ipt)

use2 ::
   (Interpolation.T a v ->
    Interpolation.T a v -> x) ->
   (T a v ->
    T a v -> x)
use2 f =
   use $ \ ipLeap ->
   use $ \ ipStep ->
      f ipLeap ipStep



data T a v = Cons {name :: String, ip :: Interpolation.T a v}

instance Show (T a v) where
   show x = name x

instance (Field.C a, Interpol.C a v) => Arbitrary (T a v) where
   arbitrary = elements $
      Cons "constant" ExampleCustom.constant :
      Cons "linear"   ExampleCustom.linear :
      Cons "cubic"    ExampleCustom.cubic :
      []



data LinePreserving a v =
   LPCons {lpName :: String, lpIp :: Interpolation.T a v}

instance Show (LinePreserving a v) where
   show x = lpName x

instance (Field.C a, Interpol.C a v) => Arbitrary (LinePreserving a v) where
   arbitrary = elements $
      LPCons "linear"   ExampleCustom.linear :
      LPCons "cubic"    ExampleCustom.cubic :
      []



constant ::
   (Interpol.C a v, Module.C a v, Eq v) =>
   a -> v -> [v] -> Bool
constant t x0 xs =
   ListHT.allEqual $ map ($ (x0:xs)) $ map ($ t) $
      Interpolation.func ExampleCustom.constant :
      Interpolation.func ExampleCustom.piecewiseConstant :
      Interpolation.func ExampleModule.constant :
      Interpolation.func ExampleModule.piecewiseConstant :
      []

linear ::
   (Interpol.C a v, Module.C a v, Eq v) =>
   a -> v -> v -> [v] -> Bool
linear t x0 x1 xs =
   ListHT.allEqual $ map ($ (x0:x1:xs)) $ map ($ t) $
      Interpolation.func ExampleCustom.linear :
      Interpolation.func ExampleCustom.piecewiseLinear :
      Interpolation.func ExampleModule.linear :
      Interpolation.func ExampleModule.piecewiseLinear :
      []

cubic ::
   (Interpol.C a v, VectorSpace.C a v, Eq v) =>
   a -> v -> v -> v -> v -> [v] -> Bool
cubic t x0 x1 x2 x3 xs =
   ListHT.allEqual $ map ($ (x0:x1:x2:x3:xs)) $ map ($ t) $
      Interpolation.func ExampleCustom.cubic :
      Interpolation.func ExampleCustom.piecewiseCubic :
      Interpolation.func ExampleModule.cubic :
      Interpolation.func ExampleModule.cubicAlt :
      Interpolation.func ExampleModule.piecewiseCubic :
      []


controlAboveOne :: (RealRing.C t) => [t] -> [t]
controlAboveOne =
   map ((one+) . abs)

frequencyModulationForth0 ::
   (RealField.C t, Eq v) =>
   [t] -> [v] -> Bool
frequencyModulationForth0 cs0 xs =
   let cs = controlAboveOne cs0
   in  Causal.apply
          (InterpolC.relative ExampleModule.constant zero
             (FiltS.inverseFrequencyModulationFloor
                (SigS.fromList cs) (SigS.fromList xs)))
          (Match.take xs cs)
        == Match.take cs xs

frequencyModulationForth0Compare ::
   (RealField.C t, Eq v) =>
   [t] -> [v] -> ([v], [v], [v])
frequencyModulationForth0Compare cs0 xs =
   let cs = controlAboveOne cs0
   in  (Match.take cs
          (Causal.apply
             (InterpolC.relative ExampleModule.constant zero
                (FiltS.inverseFrequencyModulationFloor
                   (SigS.fromList cs) (SigS.fromList xs)))
             (Match.take xs cs)),
        SigS.toList
           (FiltS.inverseFrequencyModulationFloor
              (SigS.fromList cs) (SigS.fromList xs)),
        Match.take cs xs)


frequencyModulationForth1 ::
   (RealField.C t, Eq v) =>
   [t] -> [v] -> Bool
frequencyModulationForth1 cs0 xs =
   case controlAboveOne cs0 of
      [] -> True
      (c:cs) ->
         Causal.apply
            (InterpolC.relative ExampleModule.constant c
               (FiltS.inverseFrequencyModulationFloor
                  (SigS.fromList ((c+one):cs)) (SigS.fromList xs)))
            (Match.take xs cs)
          == Match.take cs xs



controlBelowOne :: (RealField.C t) => [t] -> [t]
controlBelowOne =
   map fraction


frequencyModulationBack ::
   (RealField.C t, Eq v) =>
   [t] -> NonEmpty.T v -> Bool
frequencyModulationBack cs0 xs0 =
   let cs = controlBelowOne cs0
       xs = NonEmpty.toInfiniteList xs0
   in  take (floor (sum cs)) xs ==
          (SigS.toList $
           FiltS.inverseFrequencyModulationFloor
             (SigS.fromList cs)
             (SigS.fromList $
              Causal.apply
                 (InterpolC.relative ExampleModule.constant zero
                    (SigS.fromList xs))
                 cs))


frequencyModulationBackCompare ::
   (RealField.C t, Eq v) =>
   [t] -> [v] -> (SigS.T v, SigS.T v)
frequencyModulationBackCompare cs0 xs =
   let cs = controlBelowOne cs0
   in  (FiltS.inverseFrequencyModulationFloor
          (SigS.fromList cs)
          (SigS.fromList $
           Causal.apply
              (InterpolC.relative ExampleModule.constant zero
                 (SigS.fromList (cycle xs)))
              cs),
        SigS.fromList $
        Causal.apply
           (InterpolC.relative ExampleModule.constant zero
              (SigS.fromList (cycle xs)))
           cs)

frequencyModulationGeneric ::
   (RealField.C t, Eq v) =>
   [t] -> [v] -> Bool
frequencyModulationGeneric cs xs =
   SigS.toList
      (FiltS.inverseFrequencyModulationFloor
         (SigS.fromList cs) (SigS.fromList xs))
    == FiltG.inverseFrequencyModulationFloor
          SigG.defaultLazySize cs xs


{-
makeExactFraction :: (Int,Int) -> Double
makeExactFraction (n,d) =
   fromIntegral n * 2 ^- (- mod (fromIntegral d) 4)
-}

frequencyModulationStorableChunkSize ::
   (Storable v, RealField.C t, Eq v) =>
   SigSt.ChunkSize -> SigSt.ChunkSize ->
   SigSt.ChunkSize -> SigSt.ChunkSize ->
   [t] -> [v] ->
   Bool
frequencyModulationStorableChunkSize size0 size1 xsize0 xsize1 cs xs =
   uncurry (==) $
   frequencyModulationStorableChunkSizeCompare size0 size1 xsize0 xsize1 cs xs


frequencyModulationStorableChunkSizeCompare ::
   (Storable v, RealField.C t, Eq v) =>
   SigSt.ChunkSize -> SigSt.ChunkSize ->
   SigSt.ChunkSize -> SigSt.ChunkSize ->
   [t] -> [v] ->
   (SigSt.T v, SigSt.T v)
frequencyModulationStorableChunkSizeCompare size0 size1 xsize0 xsize1 cs xs =
   (FiltSt.inverseFrequencyModulationFloor size0 cs (SigSt.fromList xsize0 xs),
    FiltSt.inverseFrequencyModulationFloor size1 cs (SigSt.fromList xsize1 xs))


frequencyModulationStorable ::
   (Storable v, RealField.C t, Eq v) =>
   SigSt.ChunkSize -> SigSt.ChunkSize ->
   [t] -> [v] ->
   Bool
frequencyModulationStorable size xsize cs xs =
   uncurry (==) $ mapSnd SigSt.toList $
   frequencyModulationStorableCompare size xsize cs xs

frequencyModulationStorableCompare ::
   (Storable v, RealField.C t, Eq v) =>
   SigSt.ChunkSize -> SigSt.ChunkSize ->
   [t] -> [v] ->
   ([v], SigSt.T v)
frequencyModulationStorableCompare size xsize cs xs =
   (FiltG.inverseFrequencyModulationFloor
       SigG.defaultLazySize cs xs,
    FiltSt.inverseFrequencyModulationFloor size cs
       (SigSt.fromList xsize xs))



testRational ::
   (Testable t) =>
   (Rational -> Rational -> t) -> IO ()
testRational = quickCheck

testFM ::
   (Testable t, Arbitrary (sigX ArbChar), Show (sigX ArbChar)) =>
   ([Rational] -> sigX ArbChar -> t) -> IO ()
testFM = quickCheck

tests :: [(String, IO ())]
tests =
   ("constant", testRational constant) :
   ("linear",   testRational linear  ) :
   ("cubic",    testRational cubic   ) :
   ("frequencyModulationForth0",  testFM frequencyModulationForth0) :
   ("frequencyModulationForth1",  testFM frequencyModulationForth1) :
   ("frequencyModulationBack",    testFM frequencyModulationBack) :
   ("frequencyModulationGeneric", testFM frequencyModulationGeneric) :
   ("frequencyModulationStorableChunkSize",
      quickCheck (\size0 size1 xsize0 xsize1 cs xs ->
         frequencyModulationStorableChunkSize size0 size1 xsize0 xsize1
            (cs::[Rational]) (unpackArbString xs))) :
   ("frequencyModulationStorable",
      quickCheck (\size xsize cs xs ->
         frequencyModulationStorable size xsize
            (cs::[Rational]) (unpackArbString xs))) :
   []
