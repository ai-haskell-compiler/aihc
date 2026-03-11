module PrettyProcess where

import Math.ExpPairs.ProcessMatrix
import Math.ExpPairs.PrettyProcess

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive)

import Instances ()

testUglifyPrettify :: [Process] -> Bool
testUglifyPrettify xs = uglify (prettify xs) == xs

testSuite :: TestTree
testSuite = testGroup "PrettyProcess"
  [ adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `min` 12)) $
      SC.testProperty "uglify . prettify == id" testUglifyPrettify
  , adjustOption (\(QC.QuickCheckTests n) -> QC.QuickCheckTests (n `div` 2)) $
      QC.testProperty "uglify . prettify == id" testUglifyPrettify
  ]
