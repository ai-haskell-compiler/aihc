module Main where
 import Prelude
 import System.Environment (getArgs)

 import Test.DocTest.Gen (ddgen)

 main :: IO ()
 main = getArgs >>= ddgen
