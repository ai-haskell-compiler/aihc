module Test.Sound.Synthesizer.Generic.Cut (tests) where

import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Storable.Signal as SigSt

import qualified Synthesizer.ChunkySize.Signal as SigChunky
import qualified Synthesizer.ChunkySize as ChunkySize

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Lazy.Pattern as SVP

import qualified Synthesizer.State.Signal as SigS

import qualified Data.EventList.Relative.BodyTime as EventList

import qualified Number.NonNegative as NonNeg
import qualified Number.NonNegativeChunky as Chunky

import qualified Numeric.NonNegative.Wrapper as NonNeg98

import Data.Tuple.HT (mapSnd, )

import Test.QuickCheck (quickCheck, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


dropMarginRemLength :: NonNeg.Int -> NonNeg.Int -> [Int] -> Bool
dropMarginRemLength nn nm xs =
   let n = NonNeg.toNumber nn
       m = NonNeg.toNumber nm
       (k,ys) = CutG.dropMarginRem n m xs
   in  length xs - m == length ys - k

dropMarginRemState :: NonNeg.Int -> NonNeg.Int -> [Int] -> Bool
dropMarginRemState nn nm xs =
   let n = NonNeg.toNumber nn
       m = NonNeg.toNumber nm
   in  CutG.dropMarginRem n m (SigS.fromList xs)
       ==
       mapSnd SigS.fromList (CutG.dropMarginRem n m xs)

dropMarginRemSV :: NonNeg.Int -> NonNeg.Int -> [Int] -> Bool
dropMarginRemSV nn nm xs =
   let n = NonNeg.toNumber nn
       m = NonNeg.toNumber nm
   in  CutG.dropMarginRem n m (SV.pack xs)
       ==
       mapSnd SV.pack (CutG.dropMarginRem n m xs)

dropMarginRemSVL :: NonNeg.Int -> NonNeg.Int -> ChunkySize.T -> [Int] -> Bool
dropMarginRemSVL nn nm pat xs =
   let n = NonNeg.toNumber nn
       m = NonNeg.toNumber nm
   in  CutG.dropMarginRem n m
          (CutG.take (CutG.length pat) xs)
       ==
       mapSnd SigG.toList
          (CutG.dropMarginRem n m
             (SigChunky.fromState pat $
              SigG.toState xs :: SigSt.T Int))

dropMarginRemChunkySize ::
   NonNeg.Int -> NonNeg.Int -> ChunkySize.T -> Int -> Bool
dropMarginRemChunkySize nn nm pat x =
   let n = NonNeg.toNumber nn
       m = NonNeg.toNumber nm
   in  CutG.dropMarginRem n m pat
       ==
       mapSnd
          (ChunkySize.fromStorableVectorSize . SVP.length)
          (CutG.dropMarginRem n m
             (SVP.replicate (ChunkySize.toStorableVectorSize pat) x))

dropMarginRemPiecewise ::
   NonNeg.Int -> NonNeg.Int -> ChunkySize.T -> Int -> Bool
dropMarginRemPiecewise nn nm pat x =
   let n = NonNeg.toNumber nn
       m = NonNeg.toNumber nm
   in  CutG.dropMarginRem n m pat
       ==
       mapSnd
          (Chunky.fromChunks .
           map (\size -> SigG.LazySize $ NonNeg98.toNumber size) .
           EventList.getTimes)
          (CutG.dropMarginRem n m
             (EventList.fromPairList $ map ((,) x) $
              map (\(SigG.LazySize size) -> NonNeg98.fromNumber size) $
              Chunky.toChunks pat))


tests :: [(String, IO ())]
tests =
   ("dropMarginRemLength", quickCheck dropMarginRemLength) :
   ("dropMarginRemState", quickCheck dropMarginRemState) :
   ("dropMarginRemSV", quickCheck dropMarginRemSV) :
   ("dropMarginRemSVL", quickCheck dropMarginRemSVL) :
   ("dropMarginRemChunkySize", quickCheck dropMarginRemChunkySize) :
   ("dropMarginRemPiecewise", quickCheck dropMarginRemPiecewise) :
   []
