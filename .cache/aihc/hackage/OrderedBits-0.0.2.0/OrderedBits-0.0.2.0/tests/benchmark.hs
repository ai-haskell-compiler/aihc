
module Main where

import           Criterion.Main
import qualified Data.Vector.Unboxed as VU
import           Data.Word (Word(..))

import           Data.Bits.Ordered



main = defaultMain
  [ bgroup "Int"  [ bench "08" $ whnf (VU.sum . popCntSorted :: Int  -> Int )  8
                  , bench "12" $ whnf (VU.sum . popCntSorted :: Int  -> Int ) 12
                  , bench "16" $ whnf (VU.sum . popCntSorted :: Int  -> Int ) 16
--                  , bench "20" $ whnf (VU.sum . popCntSorted :: Int  -> Int ) 20
--                  , bench "24" $ whnf (VU.sum . popCntSorted :: Int  -> Int ) 24
                  ]
  , bgroup "Word" [ bench "08" $ whnf (VU.sum . popCntSorted :: Int  -> Word)  8
                  , bench "12" $ whnf (VU.sum . popCntSorted :: Int  -> Word) 12
                  , bench "16" $ whnf (VU.sum . popCntSorted :: Int  -> Word) 16
--                  , bench "20" $ whnf (VU.sum . popCntSorted :: Int  -> Word) 20
--                  , bench "24" $ whnf (VU.sum . popCntSorted :: Int  -> Word) 24
                  ]
  , bgroup "MemoInt"  [ bench "08" $ whnf (VU.sum . popCntMemoInt  :: Int  -> Int )  8
                      , bench "12" $ whnf (VU.sum . popCntMemoInt  :: Int  -> Int ) 12
                      , bench "16" $ whnf (VU.sum . popCntMemoInt  :: Int  -> Int ) 16
--                      , bench "20" $ whnf (VU.sum . popCntMemoInt  :: Int  -> Int ) 20
--                      , bench "24" $ whnf (VU.sum . popCntMemoInt  :: Int  -> Int ) 24
                      ]
  , bgroup "MemoWord" [ bench "08" $ whnf (VU.sum . popCntMemoWord :: Int  -> Word)  8
                      , bench "12" $ whnf (VU.sum . popCntMemoWord :: Int  -> Word) 12
                      , bench "16" $ whnf (VU.sum . popCntMemoWord :: Int  -> Word) 16
--                      , bench "20" $ whnf (VU.sum . popCntMemoWord :: Int  -> Word) 20
--                      , bench "24" $ whnf (VU.sum . popCntMemoWord :: Int  -> Word) 24
                      ]
--  , bgroup "small ops" [ bench "
--                       ]
  ]

