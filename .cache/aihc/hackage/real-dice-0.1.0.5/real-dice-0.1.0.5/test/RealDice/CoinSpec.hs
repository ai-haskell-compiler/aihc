{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module RealDice.CoinSpec (doesnt_give_same_result_10_times) where

import Control.Monad.State
import Data.List.Extra (allSame)
import RealDice.Coin (CoinGen, flipCoin, mkCoinGen)
import Test.QuickCheck

data CoinState where
  CoinState :: {generator :: CoinGen} -> CoinState

generateFlips :: Int -> [Bool]
generateFlips n =
  evalState
    (generateFlipsSinglePass 10 [])
    (CoinState (mkCoinGen n))

generateFlipsSinglePass :: Int -> [Bool] -> State CoinState [Bool]
generateFlipsSinglePass 0 flips = return flips
generateFlipsSinglePass i flips = do
  coinSt <- get
  let (randomInteger, gen') = flipCoin (generator coinSt)
  put CoinState {generator = gen'}
  generateFlipsSinglePass (i - 1) (randomInteger : flips)

doesnt_give_same_result_10_times :: Int -> Property
doesnt_give_same_result_10_times n = n > 0 ==> not (allSame (generateFlips n))
