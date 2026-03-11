module Main where

import qualified Test.Numeric.FFTW.Shape as TestShape
import qualified Test.Main as TestMain
import Test.Numeric.FFTW.Common (prefix)

import qualified Test.DocTest.Driver as DocTest


main :: IO ()
main = DocTest.run $ (>> TestMain.main) $
   mapM_ (\(name,prop) ->
            DocTest.printPrefix (name ++ ": ") >> DocTest.property prop) $
   prefix "Shape" TestShape.tests ++
   []
