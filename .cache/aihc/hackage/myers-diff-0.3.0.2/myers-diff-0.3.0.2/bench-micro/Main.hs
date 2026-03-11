{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Monad
import Criterion
import Criterion.Main
import Data.Maybe
import Data.String.Interpolate
import Data.Text as T
import Data.Text.Internal.Fusion
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as M
import Weigh


-- * Weight testing

testFunc :: Int -> Text -> Weigh ()
testFunc inputSize text = wgroup [i|#{inputSize} characters|] $ do
  func' "VU.fromList" fromListMethod text
  func' "VU.fromListN" fromListNMethod text
  func' "generate" generateMethod text
  func' "unfoldr" unfoldrMethod text
  func' "unfoldrN" unfoldrNMethod text
  func' "unfoldrExactN" unfoldrExactNMethod text
  func' "stream" streamCreateMethod text

-- * Criterion testing

testGroup :: Int -> Text -> Benchmark
testGroup inputSize text =
  bgroup [i|#{inputSize} characters|] [
    bench "unfoldr" $ nf unfoldrMethod text
    , bench "stream" $ nf streamCreateMethod text
    -- , bench "VU.fromList" $ nf fromListMethod text
    -- , bench "VU.fromListN" $ nf fromListNMethod text
    -- , bench "generate" $ nf generateMethod text
    -- , bench "unfoldrN" $ nf unfoldrNMethod text
    -- , bench "unfoldrExactN" $ nf unfoldrExactNMethod text
    ]


-- * Methods

fromListMethod :: Text -> VU.Vector Char
fromListMethod = VU.fromList . T.unpack

fromListNMethod :: Text -> VU.Vector Char
fromListNMethod t = VU.fromListN (T.length t) (T.unpack t)

-- | This one has great allocation but it's O(n^2) time!
generateMethod :: Text -> VU.Vector Char
generateMethod t = VU.generate (T.length t) (T.index t)

unfoldrMethod :: Text -> VU.Vector Char
unfoldrMethod = VU.unfoldr T.uncons

unfoldrNMethod :: Text -> VU.Vector Char
unfoldrNMethod t = VU.unfoldrN (T.length t) T.uncons t

unfoldrExactNMethod :: Text -> VU.Vector Char
unfoldrExactNMethod t = VU.unfoldrExactN (T.length t) (fromJust . T.uncons) t

streamCreateMethod :: Text -> VU.Vector Char
streamCreateMethod t =
  case stream t of
    Stream step s0 _ -> VU.create $ do
      m <- M.new (T.length t)
      let
        go s i =
          case step s of
            Done -> pure ()
            Skip s' -> go s' i
            Yield x s' -> do
              M.write m i x
              go s' (i + 1)
      go s0 0
      pure m

-- * Main

main :: IO ()
main = do
  defaultMain [
    testGroup n (T.replicate n "0") | n <- [10, 100, 1000, 10000, 100000]
    -- testGroup n | n <- [10, 100, 1000, 10000, 100000]
    ]

  mainWith $
    forM_ [10, 100, 1000, 10000, 100000] $ \n -> do
      let !text = T.replicate n "0"
      testFunc n text
