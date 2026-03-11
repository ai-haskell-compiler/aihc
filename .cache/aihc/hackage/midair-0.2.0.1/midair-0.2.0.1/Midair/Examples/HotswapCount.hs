{-# LANGUAGE LambdaCase #-}

module Midair.Examples.HotswapCount (
     hotswapCount
   ) where

import Midair

import Control.Arrow
import Control.Concurrent
import Data.Maybe

-- This is the same as "sCountFiresBy 3":
fToStartWith :: SFlow a Int
fToStartWith = sFold 0 $ \_ -> (+3)

-- This is the same as "sCountFiresByHS 2":
fToSwapIn :: Maybe Int -> SFlow a Int
fToSwapIn x = sFold (fromMaybe 0 x) $ \_ -> (+2)

showVal :: SFlow Int String
showVal = sMap $ \n ->
   if n `rem` 3 /= 0 && n `rem` 2 /= 0
      then "\nCool! Value only possible with hotswap: " ++ show n
      else "\nValue: " ++ show n

hotswapCount :: IO ()
hotswapCount = do
   nr <- mkNodeRef fToStartWith

   putStrLn "Ok, old style -- start typing"
   (tid, tid2, _) <- runGetChar $
      sPutStrLn <<< showVal <<< nRef nr
   threadDelay $ 10^(7::Int)

   putStrLn "Ok new style now! -- type more!"
   hotSwap nr fToSwapIn
   threadDelay $ 10^(7::Int)

   killThread tid
   killThread tid2
