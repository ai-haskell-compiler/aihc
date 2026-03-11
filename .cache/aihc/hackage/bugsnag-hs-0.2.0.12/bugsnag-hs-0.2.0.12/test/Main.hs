module Main
  ( main,
  )
where

import qualified Spec.Network.Bugsnag

main :: IO ()
main = do
  Spec.Network.Bugsnag.tests
