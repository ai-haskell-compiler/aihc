module Main (
  main,
) where

import Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "-XDerivingStrategies"
    , "-XGeneralizedNewtypeDeriving"
    , "-XOverloadedLists"
    , "-XOverloadedStrings"
    , "-XTypeApplications"
    , "-XScopedTypeVariables"
    , "-isrc"
    , "src/Trimdent.hs"
    ]
