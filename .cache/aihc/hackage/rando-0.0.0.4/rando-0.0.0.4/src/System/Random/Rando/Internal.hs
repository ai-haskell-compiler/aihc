module System.Random.Rando.Internal (
     inIO
   ) where

import Control.Applicative (pure) -- For older GHCs
-- import System.Random (RandomGen)
import System.Random.TF

inIO :: (TFGen -> (x, TFGen)) -> IO x
inIO f = do
   g <- newTFGen
   let (x, _) = f g
   pure x
