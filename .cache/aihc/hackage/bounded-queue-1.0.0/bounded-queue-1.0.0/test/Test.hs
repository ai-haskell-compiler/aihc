module Main where

import           Data.Foldable (foldl')
import qualified Data.Queue.Bounded as BQ
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "cons"
    [ testCase "Cycles Properly" cycles
    ]
  , testGroup "Misc."
    [ testCase "take few" takeFew
    , testCase "take many" takeMany
    , testCase "drop few" dropFew
    , testCase "drop many" dropMany
    ]
  ]

cycles :: Assertion
cycles = cycled @?= expected
  where
    cycled = foldl' (flip BQ.cons) orig [11..20]
    orig = BQ.fromList 10 ([1..10] :: [Int])
    expected = BQ.fromList 10 [20, 19 ..11]

takeFew :: Assertion
takeFew = length (BQ.take 5 $ BQ.fromList 10 ['a'..]) @?= 5

takeMany :: Assertion
takeMany = length (BQ.take 100 $ BQ.fromList 10 ['a'..]) @?= 10

dropFew :: Assertion
dropFew = length (BQ.drop 6 $ BQ.fromList 10 ['a'..]) @?= 4

dropMany :: Assertion
dropMany = length (BQ.drop 100 $ BQ.fromList 10 ['a'..]) @?= 0
