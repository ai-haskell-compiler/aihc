{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified DocTest.Main as DocTestMain
import qualified Test.Shape as TestShape
import Test.Utility (prefix)

import qualified Test.DocTest.Driver as DocTest


main :: IO ()
main = DocTest.run $ (>> DocTestMain.main) $
   mapM_ (\(name,prop) ->
            DocTest.printPrefix (name ++ ": ") >> DocTest.property prop) $
   prefix "Shape" TestShape.tests ++
   []
