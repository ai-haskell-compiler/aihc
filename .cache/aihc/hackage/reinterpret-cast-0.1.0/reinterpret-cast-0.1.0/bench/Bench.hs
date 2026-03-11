module Main (main) where

import           Criterion.Main

import qualified Data.ReinterpretCast as Current
import qualified Data.ReinterpretCast.Internal.ImplArray as FC
import qualified Data.ReinterpretCast.Internal.ImplFFI as FFI
-- Keep comparing against the data-binary-ieee754 package in case that changes.
import qualified Data.Binary.IEEE754 as IEEE


main :: IO ()
main = do

  defaultMain
    [ bgroup "w2f" [ bench "current" $ whnf Current.wordToFloat 1
                   , bench "array"   $ whnf FC.wordToFloat      1
                   , bench "FFI"     $ whnf FFI.wordToFloat     1
                   , bench "ieee"    $ whnf IEEE.wordToFloat    1
                   ]
    , bgroup "f2w" [ bench "current" $ whnf Current.floatToWord 1.0
                   , bench "array"   $ whnf FC.floatToWord      1.0
                   , bench "FFI"     $ whnf FFI.floatToWord     1.0
                   , bench "ieee"    $ whnf IEEE.floatToWord    1.0
                   ]
    , bgroup "w2d" [ bench "current" $ whnf Current.wordToDouble 1
                   , bench "array"   $ whnf FC.wordToDouble      1
                   , bench "FFI"     $ whnf FFI.wordToDouble     1
                   , bench "ieee"    $ whnf IEEE.wordToDouble    1
                   ]
    , bgroup "d2w" [ bench "current" $ whnf Current.doubleToWord 1.0
                   , bench "array"   $ whnf FC.doubleToWord      1.0
                   , bench "FFI"     $ whnf FFI.doubleToWord     1.0
                   , bench "ieee"    $ whnf IEEE.doubleToWord    1.0
                   ]
    ]
