module Main (main) where

import Test.Compat.Expr (exprCompatTests)
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "aihc-parser-compat"
      [exprCompatTests]
