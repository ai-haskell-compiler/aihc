{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Version.OrphansSpec (main, spec) where

import Test.Hspec

import Data.Data
import Data.Orphans ()
import Data.Version

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data Version instance" $
    it "allows obtaining a Version constructor" $
      dataTypeName (dataTypeOf (Version [1,2,3] [])) `shouldBe`
#if __GLASGOW_HASKELL__ >= 801 && __GLASGOW_HASKELL__ < 903
        -- Some old versions of GHC incorrectly return "Version" due to
        -- https://gitlab.haskell.org/ghc/ghc/-/issues/20371.
        "Version"
#elif __GLASGOW_HASKELL__ >= 910
        -- In GHC 9.10 and later, Version is defined in
        -- GHC.Internal.Data.Version (in the ghc-internal library).
        "GHC.Internal.Data.Version.Version"
#else
        -- In older versions of GHC, Version is defined in Data.Version (in the
        -- base library).
        "Data.Version.Version"
#endif

  describe "IsList Version instance" $
    it "creates a Version from an Int list" $
      [1,2,3] `shouldBe` Version [1,2,3] []
