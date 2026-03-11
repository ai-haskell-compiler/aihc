module System.Random.Shuffle.FisherYates (
     shuffle
   , shuffle'
   ) where

import Control.Applicative (pure) -- For older GHCs
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.Random.TF
import System.Random.TF.Instances

import System.Random.Rando.Internal (inIO)

shuffle :: [x] -> IO [x]
shuffle l = inIO $ shuffle' l

-- The Fisher-Yates shuffle, asymptotically optimal for both space and time:
shuffle' :: [x] -> TFGen -> ([x], TFGen)
shuffle' list gen0 =

   runST $ do
      genVar <- newSTRef gen0

      v <- V.thaw $ V.fromList list
      let vLen = VM.length v

      forM_ [vLen - 1, vLen - 2 .. 1] $ \n -> do
         indexToSwap <- randSTFromZero genVar n
         -- The "unsafe" means it doesn't have bounds checks, which is fine here:
         VM.unsafeSwap v indexToSwap n

      -- This "unsafe" is also safe here:
      final <- V.unsafeFreeze v
      finalGen <- readSTRef genVar
      pure (V.toList final, finalGen)


-- | Picks from 0 to maxval, inclusive
randSTFromZero :: STRef s TFGen -> Int -> ST s Int
randSTFromZero genVar maxVal = do
   g0 <- readSTRef genVar
   let (x, g1) = randomR (0, maxVal) g0
   writeSTRef genVar g1
   pure x

