module Process where

import Math.ExpPairs.Process

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive)

import Instances ()

testReadShow :: Path -> Bool
testReadShow p@(Path _ xs) = read (concatMap show xs) == p

testOrd :: Path -> Path -> Bool
testOrd p1 p2 = compare p1 p2 == compare (x1 / z1) (x2 / z2)
  && compare (y2 / z2) (y1 / z1) == compare (x1 / z1) (x2 / z2)
  where
    (x1, y1, z1) = evalPath p1 (1, 4, 6)
    (x2, y2, z2) = evalPath p2 (1, 4, 6)

testSuite :: TestTree
testSuite = testGroup "Process"
  [ adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `min` 13)) $
      SC.testProperty "read . show == id" testReadShow
  , QC.testProperty "read . show == id" testReadShow
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `min` 8)) $
      SC.testProperty "Ord of Processes" testOrd
  , QC.testProperty "Ord of Processes" testOrd
  ]
