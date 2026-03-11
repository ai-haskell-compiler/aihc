module Main where

import System.IO (BufferMode(LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import Test.Hspec
import qualified Test.Massiv.Serialise.BoxedSpec as Boxed (spec)
import qualified Test.Massiv.Serialise.CoreSpec as Core (spec)
import qualified Test.Massiv.Serialise.PrimitiveSpec as Primitive (spec)
import qualified Test.Massiv.Serialise.StorableSpec as Storable (spec)
import qualified Test.Massiv.Serialise.UnboxedSpec as Unboxed (spec)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspec $ do
    describe "Core" $ do
      Core.spec
    describe "Boxed" $ do
      Boxed.spec
    describe "Unboxed" $ do
      Primitive.spec
      Storable.spec
      Unboxed.spec
