module Midair.Examples.MaxToMin (
     hotswapMaxToMin
   ) where

import Midair

import Control.Arrow
import Control.Concurrent

wait10Secs :: IO ()
wait10Secs = threadDelay (10^(7::Int))

hotswapMaxToMin :: IO ()
hotswapMaxToMin = do
   showNR <- mkNodeRef $ sMap ("\nMax: "++)
   compareNR <- mkNodeRef sMax

   putStrLn "Getting max:"
   (tid0, tid1, _wholeGraph) <- runGetChar $
      sPutStrLn <<< nRef showNR <<< sMap show <<< nRef compareNR

   wait10Secs
   putStrLn "Changeup! Now getting min:"

   -- Atomically, so we don't ever see the wrong label!:
   atomically $ do
      hotSwapSTM showNR $ \_ -> sMap ("\nMin: "++)
      hotSwapSTM compareNR sMinHS

   wait10Secs
   mapM_ killThread [tid0, tid1]
