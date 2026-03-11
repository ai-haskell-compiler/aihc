{-# LANGUAGE TemplateHaskell #-}

module Data.BitVector.Sized.Panic
  ( panic
  ) where

import Panic hiding (panic)
import qualified Panic

data BVSized = BVSized

panic :: String -> [String] -> a
panic = Panic.panic BVSized

instance PanicComponent BVSized where
  panicComponentName _ = "bv-sized"
  panicComponentIssues _ = "https://github.com/GaloisInc/bv-sized/issues"

  {-# Noinline panicComponentRevision #-}
  panicComponentRevision = $useGitRevision
