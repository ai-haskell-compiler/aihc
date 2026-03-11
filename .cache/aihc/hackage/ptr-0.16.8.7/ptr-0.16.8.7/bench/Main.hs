module Main where

import Criterion.Main
import qualified Data.Serialize as Cereal
import qualified Ptr.Read as Read
import Prelude

main :: IO ()
main =
  defaultMain
    [ bench "int32InBe"
        $ nf
          (Read.runOnByteStringFinishing (Read.int32InBe))
        $! Cereal.runPut (Cereal.putInt32be 1),
      bench "int32InBe*3"
        $ nf
          (Read.runOnByteStringFinishing ((,,) <$> Read.int32InBe <*> Read.int32InBe <*> Read.int32InBe))
        $! Cereal.runPut (Cereal.putInt32be 1 <> Cereal.putInt32be 2 <> Cereal.putInt32be 3)
    ]
