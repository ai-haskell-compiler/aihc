module Test.Sound.Synthesizer.Plain.Filter (tests) where

import qualified Synthesizer.Plain.Filter.Recursive.MovingAverage as MA
import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Storable.Filter.NonRecursive as FiltNRSt
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Causal.Filter.NonRecursive as FiltNRC
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Data.StorableVector.Lazy.Pattern as VP

import Foreign.Storable.Tuple ()

import qualified Test.Sound.Synthesizer.Plain.NonEmpty as NonEmpty

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Property, arbitrary, quickCheck, )
import Test.Utility (ArbChar)

import qualified Number.GaloisField2p32m5 as GF
import qualified Number.NonNegative       as NonNeg

import qualified Numeric.NonNegative.Chunky as Chunky

import Control.Applicative (liftA2, (<$>), )

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (sortPair, mapPair, )
import Data.Ix (inRange, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


sums :: NonEmpty.T Rational -> Property
sums xsc =
   ($ NonEmpty.toList xsc) $ \xs ->
   QC.forAll (QC.choose (1, length xs)) $ \n ->
   let naive   =              FiltNR.sums        n xs
       pyramid =              FiltNR.sumsPyramid n xs
       rec     = drop (n-1) $ MA.sumsStaticInt   n xs
   in  -- this checks only for equal prefixes and can easily go wrong,
       -- if one list is empty
       and $ zipWith3 (\x y z -> x==y && y==z) naive rec pyramid
       -- ListHT.allEqual $ naive : pyramid : rec : []

sumRange :: Sig.T Int -> Property
sumRange xs =
   QC.forAll (fmap (succ . NonNeg.toNumber) arbitrary) $ \height ->
   QC.forAll (QC.choose (0, length xs)) $ \nl ->
   QC.forAll (QC.choose (0, length xs)) $ \nr ->
   let rng = (nl, nr)
       pyr = take height (FiltNR.pyramid xs)
       pyrSt =
          FiltNRSt.pyramid (+) height
             (SigSt.fromList SigSt.defaultChunkSize xs)
   in  ListHT.allEqual $
       FiltNR.sumRange xs rng :
       FiltNR.sumRangeFromPyramid pyr rng :
       FiltNR.sumRangeFromPyramidRec pyr rng :
       FiltNR.sumRangeFromPyramidFoldr pyr rng :
       FiltNRG.sumRangeFromPyramid pyrSt rng :
       FiltNRG.sumRangeFromPyramidFoldr pyrSt rng :
       FiltNRG.sumRangeFromPyramidReverse pyrSt rng :
       []

newtype Size = Size {getSize :: Int}
   deriving (Eq, Ord, Show)

sizeRange :: (Int, Int)
sizeRange = (0,1000)

instance QC.Arbitrary Size where
   arbitrary = fmap Size $ QC.choose sizeRange
   shrink (Size n) =
      map Size $ filter (inRange sizeRange) $ QC.shrink n


getRange :: (Size, Size) -> NonEmpty.T (NonEmpty.T ArbChar) -> Bool
getRange nrng pyr0 =
   let rng = sortPair $ mapPair (getSize, getSize) nrng
       pyr = map NonEmpty.toInfiniteList $ NonEmpty.toList pyr0
   in  ListHT.allEqual $
       FiltNR.getRangeFromPyramid pyr rng :
       FiltNRG.consumeRangeFromPyramid (:) [] pyr rng :
       []

sumsPosModulated :: Sig.T (Size, Size) -> NonEmpty.T Int -> Property
sumsPosModulated nctrl xsc =
   QC.forAll (QC.choose (0,10)) $ \height ->
   let ctrl = map (mapPair (getSize, getSize)) nctrl
       xs = NonEmpty.toInfiniteList xsc
   in  ListHT.allEqual $
       FiltNR.sumsPosModulated ctrl xs :
       FiltNR.sumsPosModulatedPyramid height ctrl xs :
       FiltNRG.sumsPosModulatedPyramid height ctrl xs :
       SigSt.toList
          (FiltNRG.sumsPosModulatedPyramid
             height
             (SigSt.fromList SigSt.defaultChunkSize ctrl)
             (SigSt.fromList SigSt.defaultChunkSize xs)) :
       SigSt.toList
          (FiltNRSt.sumsPosModulatedPyramid
             height
             (SigSt.fromList SigSt.defaultChunkSize ctrl)
             (SigSt.fromList SigSt.defaultChunkSize xs)) :
       Causal.apply
          (FiltNRC.sumsPosModulatedFromPyramid $
           FiltNRSt.pyramid (+) height $
           SigSt.fromList SigSt.defaultChunkSize xs)
          ctrl :
       []

minPosModulated :: Sig.T (Size, Size) -> NonEmpty.T Int -> Property
minPosModulated nctrl xsc =
   QC.forAll (QC.choose (0,10)) $ \height ->
   let ctrl =
          map
             ((\(nl,nr) -> (nl, if nl==nr then nr+1 else nr))
              .
              mapPair (getSize, getSize))
             nctrl
       xs = NonEmpty.toInfiniteList xsc
   in  ListHT.allEqual $
       zipWith FiltNR.minRange (List.tails xs) ctrl :
       SigSt.toList
          (FiltNRSt.accumulateBinPosModulatedPyramid min height
             (SigSt.fromList SigSt.defaultChunkSize ctrl)
             (SigSt.fromList SigSt.defaultChunkSize xs)) :
       []


genChunkyVector :: QC.Gen (VP.Vector Int)
genChunkyVector =
   liftA2 VP.pack
      (Chunky.fromChunks <$> arbitrary)
      (NonEmpty.toInfiniteList <$> arbitrary)

downSample2 :: Property
downSample2 =
   QC.forAll genChunkyVector $ \xs ->
   ListHT.allEqual $
      FiltNRG.downsample2 SigG.defaultLazySize xs :
      FiltNRSt.downsample2 xs :
      []

sumsDownSample2 :: Property
sumsDownSample2 =
   QC.forAll genChunkyVector $ \xs ->
   ListHT.allEqual $
      FiltNRG.sumsDownsample2 SigG.defaultLazySize xs :
      FiltNRSt.sumsDownsample2 xs :
      FiltNRSt.sumsDownsample2Alt xs :
      []

{-
sumsDownSample2 :: [VP.ChunkSize] -> NonEmpty.T Int -> Bool
sumsDownSample2 lazySize xsc =
   let len = Chunky.fromChunks $ filter (0/=) lazySize
       xs = VP.pack len $ NonEmpty.toInfiniteList xsc
   in  ListHT.allEqual $
       FiltNRG.sumsDownsample2 SigG.defaultLazySize xs :
       FiltNRSt.sumsDownsample2 xs :
       FiltNRSt.sumsDownsample2Alt xs :
       []
-}

movingAverageModulatedPyramid ::
   Sig.T Size -> NonEmpty.T (Stereo.T GF.T) -> Property
movingAverageModulatedPyramid nctrl xsc =
   QC.forAll (QC.choose (0,10)) $ \height ->
   let ctrl = map getSize nctrl
       xs = NonEmpty.toList xsc
       pack ys = SigSt.fromList SigSt.defaultChunkSize ys
       maxC = maximum ctrl
       onegf :: GF.T
       onegf = one
   in  ListHT.allEqual $
       pack (FiltNR.movingAverageModulatedPyramid onegf
          height maxC ctrl (cycle xs)) :
       FiltNRG.movingAverageModulatedPyramid onegf
          height maxC (pack ctrl) (SigG.cycle $ pack xs) :
       FiltNRSt.movingAverageModulatedPyramid onegf
          height maxC (pack ctrl) (SigG.cycle $ pack xs) :
       []


tests :: [(String, IO ())]
tests =
   ("sums", quickCheck sums) :
   ("sumRange", quickCheck sumRange) :
   ("getRange", quickCheck getRange) :
   ("sumsPosModulated", quickCheck sumsPosModulated) :
   ("minPosModulated", quickCheck minPosModulated):
   ("downSample2", quickCheck downSample2) :
   ("sumsDownSample2", quickCheck sumsDownSample2) :
   ("movingAverageModulatedPyramid", quickCheck movingAverageModulatedPyramid) :
   []
