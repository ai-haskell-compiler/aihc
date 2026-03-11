



module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.MMap as V

import           System.IO.Temp


prop_read_write :: [Double] -> Property
prop_read_write l = monadicIO $ do
    let v = V.fromList l

    v' <- run $ withSystemTempFile "vector-mmap-" $ \fn _ -> do
      _ <- V.writeMMapVector fn v
      V.unsafeMMapVector fn Nothing

    assert (v == v')


main = quickCheck prop_read_write
