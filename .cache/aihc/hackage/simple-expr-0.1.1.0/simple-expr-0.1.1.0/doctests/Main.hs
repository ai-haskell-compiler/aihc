import Test.DocTest (doctest)
import Prelude (IO)

-- This test suite exists only to add dependencies
main :: IO ()
main =
  doctest
    [ "-XHaskell2010",
      "-XNoImplicitPrelude",
      "-XInstanceSigs",
      "-XDeriveFunctor",
      "-XMultiParamTypeClasses",
      "-XFlexibleInstances",
      "-XRankNTypes",
      "-XFlexibleContexts",
      "-XScopedTypeVariables",
      "-XConstraintKinds",
      "src"
    ]
