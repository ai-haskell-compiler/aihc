
{-# LANGUAGE TemplateHaskell #-}

import Criterion.Main
import Development.IncludeFile
import qualified Data.ByteString as B

$(includeFileInSource "test/random" "random")

main :: IO ()
main = defaultMain
  [ bench "include-file" $ whnf (const random) (0 :: Int)
  , bench "read-file"    $ whnfIO $ B.readFile "test/random"
    ]
