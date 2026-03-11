module Main where

import qualified Spec.Blogger as Blogger
import qualified Spec.IO as IO
import qualified Spec.OutputFormat as OutputFormat
import qualified Spec.Wordpress as Wordpress
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Spec tests"
      [ Blogger.tests,
        Wordpress.tests,
        OutputFormat.tests,
        IO.tests
      ]
