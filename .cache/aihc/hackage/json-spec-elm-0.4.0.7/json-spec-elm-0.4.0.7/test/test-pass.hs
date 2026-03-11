module Main (main) where

main :: IO ()
main =
  putStrLn $
    unlines
      [ ""
      , ""
      , "    NB! We are _NOT_ really running the tests because the"
      , "    `compile-elm` flag is not set. If you are sure that the"
      , "    proper Elm tools are installed on the system, then you can"
      , "    set the `compile-elm` flag and real tests will be run."
      , ""
      , ""
      ]
