import Test.Tasty

import IntMap
import List
import Map

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [listProps, mapProps, intMapProps]
