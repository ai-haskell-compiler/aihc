{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Sandwich

#ifdef UNI_MYERS
import Spec.UniMyersSpec as UniMyersSpec
#endif

#ifdef DIFF
import Spec.DiffMyersSpec as DiffMyersSpec
#endif

import Spec.VectorMyersSpec as VectorMyersSpec


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ do
#ifdef UNI_MYERS
  UniMyersSpec.spec
#endif

#ifdef DIFF
  DiffMyersSpec.spec
#endif

  VectorMyersSpec.spec
