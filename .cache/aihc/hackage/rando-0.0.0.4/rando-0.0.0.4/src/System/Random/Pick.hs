module System.Random.Pick (
     pickOne
   , flipCoin

   -- * \"Internal\" functions
   , pickOne'
   , flipCoin'
   ) where

import System.Random.TF.Gen (TFGen)
import System.Random.TF.Instances (randomR)

import System.Random.Rando.Internal (inIO)

pickOne :: [x] -> IO x
pickOne l = inIO $ pickOne' l

pickOne' :: [x] -> TFGen -> (x, TFGen)
pickOne' l g0 =
   let (ix, g1) =
          randomR (0, (length::[a]->Int) l - 1) g0
   in (l !! ix, g1)

flipCoin :: IO Bool
flipCoin = inIO $ flipCoin'

flipCoin' :: TFGen -> (Bool, TFGen)
flipCoin' = pickOne' [True, False]
