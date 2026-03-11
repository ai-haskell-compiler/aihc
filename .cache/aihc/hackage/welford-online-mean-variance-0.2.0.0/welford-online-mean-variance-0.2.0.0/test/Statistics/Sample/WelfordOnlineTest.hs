module Statistics.Sample.WelfordOnlineTest where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                                   (foldl')
import qualified Data.Vector                                 as VB
import           Test.Tasty.QuickCheck

import           Statistics.Sample.WelfordOnlineMeanVariance


-- | Recursively generate candles
generateNValues :: Int -> Gen [Double]
generateNValues n
  | n <= 0 = return []
  | otherwise =  do
      x <- arbitrary
      (x:) <$> generateNValues (n - 1)

epsEqWith :: (Ord a, Fractional a, Show a) => a -> a -> a -> Property
epsEqWith eps a b
  | abs (a - b) <= eps = a === a
  | otherwise = a === b


prop_MeanAndVariance :: Gen Property
prop_MeanAndVariance = do
  vals <- sized $ \n -> generateNValues (10 + 5 * n)
  let n = fromIntegral (length vals)
      mean = sum vals / n
      var = sum (map (\x -> (x - mean) ^ 2) vals) / (n - 1)
      (wMean, _, wVarSample) = snd $ foldl' (nextValue . fst) (WelfordExistingAggregateEmpty, (0, 0, 0)) vals
      eps = min 0.01 $ max 0.001 (0.001 * mean)
  return $ epsEqWith eps wMean mean .&&. epsEqWith eps wVarSample var


prop_MeanAndVarianceVector :: Gen Property
prop_MeanAndVarianceVector = do
  len <- chooseInt (1, 100)
  vecs <- sized $ \n -> replicateM len (VB.fromList <$> generateNValues (10 + 5 * n))
  let mkRes vals =
        let n = fromIntegral (VB.length vals)
            mean = VB.sum vals / n
            var = VB.sum (VB.map (\x -> (x - mean) ^ 2) vals) / (n - 1)
            (wMean, _, wVarSample) = snd $ foldl' (nextValue . fst) (WelfordExistingAggregateEmpty, (0, 0, 0)) (VB.toList vals)
            eps = min 0.01 $ max 0.001 (0.001 * mean)
        in (eps, wMean, mean, wVarSample, var)
      ress = map mkRes vecs
  return $
    foldl1 (.&&.) $ map (\(eps, wMean, mean, wVarSample, var) -> epsEqWith eps wMean mean .&&. epsEqWith eps wVarSample var) ress


prop_readme_example :: Int -> Gen Property
prop_readme_example nr = do
  vals <- generateNValues (abs nr + 2)
  let n = fromIntegral (length vals)
      mean = sum vals / n
      var = sum (map (\x -> (x - mean) ^ 2) vals) / (n - 1)
      (wMean, _, wVarSample) = finalize $ foldl' addValue WelfordExistingAggregateEmpty vals
  -- print (mean, var)
  -- print (wMean, wVarSample)
      eps = min 0.01 $ max 0.001 (0.001 * mean)
  return $ epsEqWith eps wMean mean .&&. epsEqWith eps wVarSample var


prop_Normalise :: Gen Property
prop_Normalise = do
  vals <- sized $ \n -> VB.fromList <$> generateNValues (100 + 5 * n)
  let wel = addValues WelfordExistingAggregateEmpty vals :: WelfordExistingAggregate Double
  testVal <- arbitrary
  return $ epsEqWith 0.002 testVal (denormaliseFromZeroMeanUnitVariance wel (normaliseToZeroMeanUnitVariance wel testVal))

