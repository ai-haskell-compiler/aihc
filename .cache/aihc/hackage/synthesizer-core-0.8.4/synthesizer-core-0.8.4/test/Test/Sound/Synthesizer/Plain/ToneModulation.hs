module Test.Sound.Synthesizer.Plain.ToneModulation (tests, ) where

import Test.Sound.Synthesizer.Basic.ToneModulation (
   minLength,
   minLengthMargin,
   shapeLimits,
   testRationalLineIp,
   testRationalIp,
   )

import qualified Synthesizer.Plain.Oscillator     as Osci
import qualified Synthesizer.Plain.Interpolation  as Interpolation
import qualified Synthesizer.Plain.ToneModulation as ToneModL
import qualified Synthesizer.Plain.Wave           as WaveL
import Synthesizer.Interpolation (marginNumber, )

import qualified Synthesizer.Basic.Wave           as Wave
import qualified Synthesizer.Basic.Phase          as Phase

import qualified Test.Sound.Synthesizer.Plain.NonEmpty as NonEmpty
import qualified Test.Sound.Synthesizer.Plain.Interpolation as InterpolationTest

import qualified Test.QuickCheck as QC
import Test.QuickCheck (quickCheck, Property, (==>), )
import Test.Utility (ArbChar, )

import qualified Number.NonNegative       as NonNeg
import qualified Number.NonNegativeChunky as Chunky

import qualified Algebra.RealTranscendental    as RealTrans
import qualified Algebra.Module                as Module
import qualified Algebra.RealField             as RealField
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive
import qualified Algebra.ZeroTestable          as ZeroTestable

import Data.List.HT (isAscending, )
import Data.Ord.HT (limit, )
import Data.Tuple.HT (mapPair, mapSnd, )
import qualified Data.List as List

import System.Random (Random, )


import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{-
Properties that do not hold:
  commutativity of limitRelativeShapes and integrateFractional:
    Does not hold because when you clip the integral skips at the end,
    you would have to clear the fractional part, too.
-}



absolutize :: (Additive.C a) => a -> [a] -> [a]
absolutize = scanl (+)

limitMinRelativeValues ::
   Int -> Int -> [NonNeg.Int] -> Bool
limitMinRelativeValues xMin x0 xsnn =
   let xs = map NonNeg.toNumber xsnn
   in  map (max xMin) (absolutize x0 xs) ==
          uncurry absolutize (ToneModL.limitMinRelativeValues xMin x0 xs)

limitMaxRelativeValues ::
   Int -> Int -> [NonNeg.Int] -> Bool
limitMaxRelativeValues xMax x0 xsnn =
   let xs = map NonNeg.toNumber xsnn
   in  map (min xMax) (absolutize x0 xs) ==
          uncurry absolutize (ToneModL.limitMaxRelativeValues xMax x0 xs)

limitMaxRelativeValuesNonNeg ::
   Int -> Int -> [NonNeg.Int] -> Bool
limitMaxRelativeValuesNonNeg xMax x0 xsnn =
   let xs = map NonNeg.toNumber xsnn
   in  map (min xMax) (absolutize x0 xs) ==
          uncurry absolutize (ToneModL.limitMaxRelativeValuesNonNeg xMax x0 xs)

-- chunky type is not necessary here but testing it a little is not wrong
limitMinRelativeValuesIdentity ::
   Chunky.T NonNeg.Int -> [Chunky.T NonNeg.Int] -> Bool
limitMinRelativeValuesIdentity x0 xs =
   (x0,xs) == ToneModL.limitMinRelativeValues 0 x0 xs

limitMaxRelativeValuesIdentity ::
   Chunky.T NonNeg.Int -> [Chunky.T NonNeg.Int] -> Bool
limitMaxRelativeValuesIdentity x0 xs =
   let inf = 1 + inf
   in  (x0,xs) == ToneModL.limitMaxRelativeValues inf x0 xs

limitMaxRelativeValuesNonNegIdentity ::
   Chunky.T NonNeg.Int -> [Chunky.T NonNeg.Int] -> Bool
limitMaxRelativeValuesNonNegIdentity x0 xs =
   let inf = 1 + inf
   in  (x0,xs) == ToneModL.limitMaxRelativeValuesNonNeg inf x0 xs

limitMaxRelativeValuesInfinity ::
   Chunky.T NonNeg.Int -> NonEmpty.T (Chunky.T NonNeg.Int) -> Bool
limitMaxRelativeValuesInfinity x0 ixs =
   let inf = 1 + inf
       ys = NonEmpty.toInfiniteList ixs
       (z0,zs) = ToneModL.limitMaxRelativeValues inf x0 ys
   in  (x0, take 100 ys) == (z0, take 100 zs)

limitMaxRelativeValuesNonNegInfinity ::
   Chunky.T NonNeg.Int -> NonEmpty.T (Chunky.T NonNeg.Int) -> Bool
limitMaxRelativeValuesNonNegInfinity x0 ixs =
   let inf = 1 + inf
       ys = NonEmpty.toInfiniteList ixs
       (z0,zs) = ToneModL.limitMaxRelativeValuesNonNeg inf x0 ys
   in  (x0, take 100 ys) == (z0, take 100 zs)


dropRem :: Eq a => NonNeg.Int -> [a] -> Bool
dropRem nn xs =
   let n = NonNeg.toNumber nn
   in  map (flip ToneModL.dropRem xs) [0 .. n + length xs] ==
       map ((,) 0) (List.tails xs) ++ map (flip (,) []) [1..n]

ten, hundred :: (Ring.C a) => a
ten = fromInteger 10; hundred = fromInteger 100

sampledToneSine :: (RealTrans.C a, Module.C a a, Show a, Random a) =>
   NonNeg.Int -> a -> a -> a -> Property
sampledToneSine ext phase0 shape phase =
   QC.forAll (QC.choose (ten,hundred)) $ \period ->
   let ipLeap = Interpolation.cubic
       ipStep = Interpolation.cubic
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (Osci.staticSine phase0 (recip period))
   in  abs (WaveL.sampledTone ipLeap ipStep period tone shape `Wave.apply` (Phase.fromRepresentative phase) -
            head (Osci.staticSine (phase0+phase) zero)) < ten ^- (-2)


sampledToneSineList :: (RealTrans.C a, Module.C a a, Show a, Random a) =>
   NonNeg.Int -> a -> a -> [a] -> [a] -> Property
sampledToneSineList ext origPhase phase shapes freqs =
   QC.forAll (QC.choose (ten,hundred)) $ \period ->
   let ipLeap = Interpolation.cubic
       ipStep = Interpolation.cubic
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (Osci.staticSine origPhase (recip period))
   in  all ((< ten ^- (-2)) . abs) $
       zipWith (-)
          (Osci.shapeFreqMod (WaveL.sampledTone ipLeap ipStep period tone)
               phase shapes freqs)
          (Osci.freqModSine (origPhase+phase) freqs)


sampledToneLinear :: (RealField.C a, Module.C a v, Eq v) =>
   InterpolationTest.LinePreserving a v ->
   InterpolationTest.LinePreserving a v ->
   NonNeg.T a -> NonNeg.Int -> (v,v) -> a -> Phase.T a -> Property
sampledToneLinear =
   InterpolationTest.useLP $ \ ipLeap ->
   InterpolationTest.useLP $ \ ipStep ->
         \ periodNN ext (i,d) shape phase ->
   let period = NonNeg.toNumber periodNN
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       ramp = take len (List.iterate (d+) i)
       limits =
          mapPair (fromIntegral, fromIntegral) $
             shapeLimits ipLeap ipStep periodInt len
   in  period /= zero ==>
          -- should be (fraction phase), right?
          WaveL.sampledTone ipLeap ipStep period ramp shape `Wave.apply` phase ==
             i + limit limits shape *> d
{-
let len=100; period=1/0.06::Double; ip = Interpolation.linear in GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (0,fromIntegral len)) [\s -> WaveL.sampledTone ip ip period (take len $ iterate (1+) (0::Double)) s 0, limit (mapPair (fromIntegral, fromIntegral) $ shapeLimits ip ip (round period::Int) len)]
-}

sampledToneStair :: (RealField.C a, Module.C a v, Eq v) =>
   InterpolationTest.LinePreserving a v ->
   NonNeg.Int -> NonNeg.Int -> (v,v) -> a -> Property
sampledToneStair =
   InterpolationTest.useLP $ \ ipLeap
         periodIntNN ext (i,d) shape ->
   let ipStep = Interpolation.constant
       periodInt = NonNeg.toNumber periodIntNN
       period    = fromIntegral periodInt
       len0 = minLength ipLeap ipStep periodInt ext
       (rep,rm) = divMod (negate len0) periodInt
       len   = len0 + rm
       stair =
          concatMap (replicate periodInt) $
          take (negate rep) (List.iterate (period*>d+) i)
       limits =
          mapPair (fromIntegral, fromIntegral) $
             shapeLimits ipLeap ipStep periodInt len
   in  periodInt /= zero ==>
          WaveL.sampledTone ipLeap ipStep period stair shape `Wave.apply` zero ==
             i + limit limits shape *> d
{-
let len=periodInt*rep; rep=10; periodInt = 14::Int; period=fromIntegral periodInt; ipl = Interpolation.linear; ipc = Interpolation.constant in GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (-10,10+fromIntegral len)) [\s -> WaveL.sampledTone ipl ipc period (concatMap (replicate periodInt) $ take rep $ iterate (period+) (0::Double)) s 0, limit (mapPair (fromIntegral, fromIntegral) $ shapeLimits ipl ipc periodInt len)]
-}

{-
sampledToneSaw :: (RealField.C a, Module.C a v, Eq v) =>
   InterpolationTest.LinePreserving a v ->
   InterpolationTest.T a v ->
   NonNeg.Int -> NonNeg.Int -> (v,v) -> a -> a -> Property
sampledToneSaw iptLeap iptStep periodIntNN ext (i,d) shape phase =
   let ipLeap = InterpolationTest.lpIp iptLeap
       ipStep = InterpolationTest.ip   iptStep
       periodInt = NonNeg.toNumber periodIntNN
       period    = fromIntegral periodInt
       len0 = minLength ipLeap ipStep periodInt ext
       rep = negate $ div (negate len0) periodInt
       saw =
          concat $ replicate rep $
          take periodInt $ List.iterate (d+) i
   in  periodInt /= zero ==>
          WaveL.sampledTone ipLeap ipStep period saw shape phase ==
             i + fraction phase *> d
-}

sampledToneStatic :: (RealField.C a, Eq v) =>
   InterpolationTest.T a v ->
   InterpolationTest.T a v ->
   NonNeg.Int -> (v,[v]) -> a -> a -> Property
sampledToneStatic =
   InterpolationTest.use2 $ \ ipLeap ipStep
         ext (x,xs) shape phase ->
   let wave = x:xs
       periodInt = length wave
       period    = fromIntegral periodInt
       len = minLength ipLeap ipStep periodInt ext
       rep = negate $ div (negate len) periodInt
       tone = concat $ replicate rep wave
   in  period /= zero ==>
          WaveL.sampledTone ipLeap ipStep period tone shape `Wave.apply` (Phase.fromRepresentative phase) ==
          Interpolation.cyclicPad Interpolation.single ipStep (phase*period) wave
{-
let wave = [1,-1,0.5,-0.5::Double]; period = fromIntegral (length wave) :: Double; ip = Interpolation.linear in GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (-1,3)) [WaveL.sampledTone ip ip period (concat $ replicate 3 wave) 0.3, \phase -> Interpolation.cyclicPad Interpolation.single Interpolation.linear (phase*period) wave]
-}



shapeFreqModFromSampledToneLimitIdentity :: (RealField.C t) =>
   Interpolation.Margin ->
   Interpolation.Margin ->
   NonNeg.Int -> NonEmpty.T y -> (t, NonEmpty.T (NonNeg.T t)) -> Bool
shapeFreqModFromSampledToneLimitIdentity
      marginLeap marginStep periodIntNN ixs (shape0,shapesNN) =
   let periodInt = NonNeg.toNumber periodIntNN
       shapes = fmap NonNeg.toNumber shapesNN
       a = snd
          (ToneModL.limitRelativeShapes
             marginLeap marginStep
             periodInt (NonEmpty.toInfiniteList ixs)
             (shape0, NonEmpty.toInfiniteList shapes)) !! 100
   in  a == a


oscillatorCoords :: (RealField.C t) =>
   NonNeg.Int -> NonNeg.T t -> t -> Phase.T t -> [NonNeg.T t] -> [t] -> Property
oscillatorCoords
     periodIntNN periodNN shape0 phase shapesNN freqs =
   let shapes = map NonNeg.toNumber shapesNN
       period    = NonNeg.toNumber periodNN
       periodInt = NonNeg.toNumber periodIntNN
       periodRound = fromIntegral periodInt
       coords =
          ToneModL.oscillatorCoords
             periodInt period
             (shape0, shapes) (phase, freqs)
   in  period /= zero  &&  periodInt /= zero  ==>
          all
             (\(skip,(k,(qShape,qWave))) ->
                  skip >= zero &&
                  isAscending [negate periodInt, k, zero] &&
                  isAscending [zero, qShape, one] &&
                  isAscending [zero, qWave, periodRound])
             (tail coords)


shapeFreqModFromSampledToneCoordsIdentity ::
   (RealField.C t, ZeroTestable.C t) =>
   NonNeg.Int -> NonNeg.T t -> (t, [NonNeg.T t]) -> Property
shapeFreqModFromSampledToneCoordsIdentity
      periodIntNN periodNN (shape0,shapesNN) =
   let period    = NonNeg.toNumber periodNN
       periodInt = NonNeg.toNumber periodIntNN
       shapes = map NonNeg.toNumber shapesNN
       phase  = Phase.fromRepresentative $ shape0 / period
       freqs  = map (/period) shapes
   in  period /= zero  ==>
          all
             (isZero . fst . snd . snd)
             (ToneModL.oscillatorCoords
                 periodInt period (shape0, shapes) (phase, freqs))


shapeFreqModFromSampledTone :: (RealField.C t, Eq v) =>
   InterpolationTest.T t v ->
   InterpolationTest.T t v ->
   NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> t -> [NonNeg.T t] -> [t] ->
   Property
shapeFreqModFromSampledTone =
   InterpolationTest.use2 $ \ ipLeap ipStep
         periodNN ext ixs shape0 phase shapesNN freqs ->
   let shapes = map NonNeg.toNumber shapesNN
       period = NonNeg.toNumber periodNN
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       resampledToneA =
          Osci.shapeFreqModFromSampledTone ipLeap ipStep period tone
             shape0 phase shapes freqs
       resampledToneB =
          Osci.shapeFreqMod
             (WaveL.sampledTone ipLeap ipStep period tone)
             phase (scanl (+) shape0 shapes) freqs
   in  period /= zero  ==>
          resampledToneA == resampledToneB
{-
let len=100; period=1/0.06::Double; ip = Interpolation.linear; tone = take len $ iterate (1+) (0::Double); shape0=0; shapes = replicate 100 1; in GNUPlot.plotLists [] [Osci.shapeFreqMod (WaveL.sampledTone ip ip period tone) 0 (scanl (+) shape0 shapes) (repeat 0), Osci.shapeFreqModFromSampledTone ip ip period tone shape0 0 shapes (repeat 0)]
*Test.Sound.Synthesizer.Plain.Oscillator> let len=100; period=1/0.06::Double; ip = Interpolation.linear; tone = take len $ iterate (1+) (0::Double); shape0=0; shapes = concat $ replicate 50 [1.5,0.5]; in GNUPlot.plotLists [] [Osci.shapeFreqMod (WaveL.sampledTone ip ip period tone) 0 (scanl (+) shape0 shapes) (repeat 0), Osci.shapeFreqModFromSampledTone ip ip period tone shape0 0 shapes (repeat 0)]
*Test.Sound.Synthesizer.Plain.Oscillator> let len=100; period=1/0.06::Rational; ipLeap = Interpolation.linear; ipStep = Interpolation.constant; tone = take len $ iterate (1+) (0::Rational); shape0=0; shapes = concat $ replicate 50 [1.5,0.5]; in GNUPlot.plotLists [] (map (map (\x -> fromRational' x :: Double)) [Osci.shapeFreqMod (WaveL.sampledTone ipLeap ipStep period tone) 0 (scanl (+) shape0 shapes) (repeat 0), Osci.shapeFreqModFromSampledTone ipLeap ipStep period tone shape0 0 shapes (repeat 0)])
-}


shapePhaseFreqModFromSampledTone :: (RealField.C t, Eq v) =>
   InterpolationTest.T t v ->
   InterpolationTest.T t v ->
   NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> t -> [NonNeg.T t] -> [t] -> [t] ->
   Property
shapePhaseFreqModFromSampledTone =
   InterpolationTest.use2 $ \ ipLeap ipStep
         periodNN ext ixs shape0 phase shapesNN phaseDistorts freqs ->
   let shapes = map NonNeg.toNumber shapesNN
       period = NonNeg.toNumber periodNN
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       resampledToneA =
          Osci.shapePhaseFreqModFromSampledTone ipLeap ipStep period tone
             shape0 phase shapes phaseDistorts freqs
       resampledToneB =
          Osci.shapeFreqMod
             (uncurry $
                Wave.phaseOffset .
                WaveL.sampledTone ipLeap ipStep period tone)
             phase (zip (scanl (+) shape0 shapes) phaseDistorts) freqs
   in  period /= zero  ==>
          resampledToneA == resampledToneB


oscillatorCells :: (RealField.C t, Eq v) =>
   Interpolation.Margin ->
   Interpolation.Margin ->
   NonNeg.Int ->
   NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   t -> t -> [NonNeg.T t] -> [t] ->
   Property
oscillatorCells
      marginLeap marginStep periodIntNN periodNN ext ixs shape0 phase shapesNN freqs =
   let shapes = map NonNeg.toNumber shapesNN
       period    = NonNeg.toNumber periodNN
       periodInt = NonNeg.toNumber periodIntNN
       len = minLengthMargin marginLeap marginStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       crop = cropCell marginLeap marginStep
       resampledToneA =
          ToneModL.oscillatorCells
             marginLeap marginStep periodInt period tone
             (shape0, shapes) (Phase.fromRepresentative phase, freqs)
       resampledToneB =
          Osci.shapeFreqMod
             (Wave.Cons . ToneModL.sampledToneCell
                (ToneModL.makePrototype marginLeap marginStep
                    periodInt period tone))
             phase (scanl (+) shape0 shapes) freqs
   in  period /= zero  &&
       periodInt /= zero  &&
       marginNumber marginLeap > zero &&
       marginNumber marginStep > zero  ==>
          map crop resampledToneA == map crop resampledToneB

cropCell ::
   Interpolation.Margin ->
   Interpolation.Margin ->
   ((t,t), ToneModL.Cell v) -> ((t,t), ToneModL.Cell v)
cropCell ipLeap ipStep =
   mapSnd
      (take (marginNumber ipStep) .
       map (take (marginNumber ipLeap)))


shapeFreqModFromSampledToneIdentity :: (RealField.C t, Eq v) =>
   InterpolationTest.T t v ->
   InterpolationTest.T t v ->
   NonNeg.T t ->
   NonNeg.Int -> NonEmpty.T v ->
   Property
shapeFreqModFromSampledToneIdentity =
   InterpolationTest.use2 $ \ ipLeap ipStep
          periodNN ext ixs ->
   let period = NonNeg.toNumber periodNN
       periodInt = round period
       len = minLength ipLeap ipStep periodInt ext
       tone = take len (NonEmpty.toInfiniteList ixs)
       shape0 = zero
       shapes = repeat one
       phase  = zero
       freqs  = repeat (recip period)
       (n0,n1) =
          shapeLimits ipLeap ipStep periodInt len

       resampledTone =
          Osci.shapeFreqModFromSampledTone ipLeap ipStep period tone
             shape0 phase shapes freqs
   in  period /= zero  ==>
          and (drop n0 (take (succ n1) (zipWith (==) resampledTone tone)))


tests :: [(String, IO ())]
tests =
   ("limitMinRelativeValues", quickCheck limitMinRelativeValues) :
   ("limitMaxRelativeValues", quickCheck limitMaxRelativeValues) :
   ("limitMaxRelativeValuesNonNeg",
                              quickCheck limitMaxRelativeValuesNonNeg) :
   ("limitMinRelativeValuesIdentity",
                              quickCheck limitMinRelativeValuesIdentity) :
   ("limitMaxRelativeValuesIdentity",
                              quickCheck limitMaxRelativeValuesIdentity) :
   ("limitMaxRelativeValuesNonNegIdentity",
                              quickCheck limitMaxRelativeValuesNonNegIdentity) :
   ("limitMaxRelativeValuesInfinity",
                              quickCheck limitMaxRelativeValuesInfinity) :
   ("limitMaxRelativeValuesNonNegInfinity",
                              quickCheck limitMaxRelativeValuesNonNegInfinity) :
   ("dropRem",                quickCheck (dropRem :: NonNeg.Int -> [ArbChar] -> Bool)) :
   ("sampledToneSine",
      quickCheck (\ext phase0 -> sampledToneSine ext (phase0 :: Double))) :
   ("sampledToneSineList",
      quickCheck (\ext phase0 -> sampledToneSineList ext (phase0 :: Double))) :
   ("sampledToneLinear",
      testRationalLineIp sampledToneLinear) :
   ("sampledToneStair",
      testRationalLineIp sampledToneStair) :
{-
   ("sampledToneSaw",
      testRationalLineIp sampledToneSaw) :
-}
   ("sampledToneStatic",
      testRationalIp sampledToneStatic) :
   ("shapeFreqModFromSampledToneLimitIdentity",
      quickCheck (\ml ms p ixs (t,ts) ->
          shapeFreqModFromSampledToneLimitIdentity ml ms p
             (ixs::NonEmpty.T Rational) (t::Rational,ts))) :
   ("oscillatorCoords",
      quickCheck (\periodInt period ->
               oscillatorCoords
                  periodInt (period :: NonNeg.Rational))) :
   ("shapeFreqModFromSampledToneCoordsIdentity",
      quickCheck (\periodInt period ->
               shapeFreqModFromSampledToneCoordsIdentity
                  periodInt (period :: NonNeg.Rational))) :
   ("shapeFreqModFromSampledTone",
      testRationalIp shapeFreqModFromSampledTone) :
   ("shapePhaseFreqModFromSampledTone",
      testRationalIp shapePhaseFreqModFromSampledTone) :
   ("oscillatorCells",
      quickCheck (\ml ms periodInt period ext ixs ->
               oscillatorCells ml ms periodInt (period :: NonNeg.Rational)
                  ext (ixs :: NonEmpty.T ArbChar))) :
   ("shapeFreqModFromSampledToneIdentity",
      testRationalIp shapeFreqModFromSampledToneIdentity) :
   []
