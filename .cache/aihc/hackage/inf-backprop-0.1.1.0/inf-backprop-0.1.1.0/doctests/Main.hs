import Test.DocTest (doctest)
import Prelude (IO)

-- This test suite exists only to add dependencies
main :: IO ()
main =
  doctest
    [ "-XHaskell2010",
      "-XNoImplicitPrelude",
      "-XGADTs",
      "-XTypeFamilies",
      "-XMultiParamTypeClasses",
      "-XFlexibleInstances",
      "-XScopedTypeVariables",
      "-XConstraintKinds",
      "-XRankNTypes",
      "-XInstanceSigs",
      "-XTupleSections",
      "-XFlexibleContexts",
      "-XDeriveFunctor",
      "src"
    ]
