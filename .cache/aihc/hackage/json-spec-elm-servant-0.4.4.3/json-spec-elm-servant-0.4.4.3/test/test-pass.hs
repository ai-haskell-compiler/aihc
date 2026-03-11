
{-|
  A "test" that always passes, so that systems that don't have Elm
  installed can still run the tests (as, for instance, in some kind of
  CI workflow that always runs  the tests for all dependencies).
-}
module Main (main) where

main :: IO ()
main =
  putStrLn $
    unlines
      [ ""
      , ""
      , "    NB! We are _NOT_ running the tests really because the `compile-elm`"
      , "    flag is not set. If you are sure that the proper Elm tools are"
      , "    installed on the system, then you can set the `compile-elm` flag"
      , "    and real tests will be run"
      , ""
      , ""
      ]
