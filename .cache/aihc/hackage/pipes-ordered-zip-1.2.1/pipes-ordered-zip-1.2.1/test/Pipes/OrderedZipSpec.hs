module Pipes.OrderedZipSpec (spec) where

import Control.Foldl (list, purely)
import Test.Hspec
import Pipes (each, (>->))
import Pipes.Prelude (fold')
import Pipes.Safe (runSafeT)

import Pipes.OrderedZip (orderedZip, orderedZipAll, orderCheckPipe, WrongInputOrderException(..))

spec :: Spec
spec = do
    testOrderedZip
    testOrderedZipAll
    testOrderCheckPipe

testOrderedZip :: Spec
testOrderedZip = describe "orderedZip" $ do
    let a = each ([1,3,4,6,8] :: [Int])
        b = each ([2,3,4,5,8] :: [Int])
        mergedProd = orderedZip compare a b
    it "should give correctly merged output" $
        (fst <$> purely fold' list mergedProd) `shouldReturn` result
  where
    result = [(Just 1, Nothing),
              (Nothing, Just 2),
              (Just 3, Just 3),
              (Just 4, Just 4),
              (Nothing, Just 5),
              (Just 6, Nothing),
              (Just 8, Just 8)]

testOrderedZipAll :: Spec
testOrderedZipAll = describe "orderedZipAll" $ do
    let a = each ([1,  3,4,  6,  8] :: [Int])
        b = each ([  2,3,4,5,    8] :: [Int])
        c = each ([  2,3,  5,  7,8] :: [Int])
        mergedProd = orderedZipAll compare [a, b, c]
    it "should give correctly merged output" $
        (fst <$> purely fold' list mergedProd) `shouldReturn` result
  where
    result = [[Just 1, Nothing, Nothing],
              [Nothing, Just 2, Just 2],
              [Just 3, Just 3, Just 3],
              [Just 4, Just 4, Nothing],
              [Nothing, Just 5, Just 5],
              [Just 6, Nothing, Nothing],
              [Nothing, Nothing, Just 7],
              [Just 8, Just 8, Just 8]]

testOrderCheckPipe :: Spec
testOrderCheckPipe = describe "orderCheckPipe" $ do
    let a = each ([1,4,3,6,8] :: [Int])
        p = a >-> orderCheckPipe compare
    it "should throw if invalid input order is given" $
        runSafeT (purely fold' list p) `shouldThrow` (==WrongInputOrderException "ordering violated: 4 should come after 3")
    it "should do nothing if input is ordered" $ do
        let a' = each ([1,3,4,6,8] :: [Int])
            p' = a' >-> orderCheckPipe compare
        runSafeT (fst <$> purely fold' list p') `shouldReturn` [1, 3, 4, 6, 8]

