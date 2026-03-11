{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Matrix.Static
import Data.Monoid (Sum(Sum), Product(Product), (<>))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [ docExamples, instanceTests ]

instanceTests :: TestTree
instanceTests =
  let u = fromListUnsafe @2 @2 @(Int -> Int) (map (*) [1,2,3,4])
      v = fromListUnsafe @2 @2 @(Int -> Int) (map (+) [3,-4,0,2])
      w = fromListUnsafe @2 @2 @Int [4,-1,2,2]
      a = fromListUnsafe @2 @2 @(Sum Int) (map Sum [-1,-4,0,2])
      x = 2
      f = (+5)
      p = fromListUnsafe @2 @2 @Int [2,2,2,2]
   in testGroup "Instance Tests"
  [ testGroup "Applicative laws"
    [ testCase "identitiy" $ pure id <*> w @?= w
    , testCase "composition" $ pure (.) <*> u <*> v <*> w @?= u <*> (v <*> w)
    , testCase "homomorphism" $ pure f <*> pure x @?= (pure (f x) :: Matrix 2 2 Int)
    , testCase "interchange" $ u <*> pure x @?= (pure ($ x) <*> u :: Matrix 2 2 Int)
    ]
  , testGroup "Monoidal laws"
    [ testCase "right identity" $ a <> mempty @?= a
    , testCase "left identity" $ mempty <> a @?= a
    , testCase "associativity" $ 
        let b = fromListUnsafe @2 @2 @(Sum Int) (map Sum [-1,-4,0,2])
            c = fromListUnsafe @2 @2 @(Sum Int) (map Sum [-1,-4,0,2])
        in (a <> b) <> c @?= a <> (b <> c)
    ]
  ]

docExamples :: TestTree
docExamples =
  let mat33 = fromListUnsafe @3 @3 @Int [1..9]
   in testGroup "Doc examples"
  [ testCase "mapRowUnsafe" $
      mapRowUnsafe (\_ x -> x + 1) 2 mat33 @?=
          fromListUnsafe [1,2,3,5,6,7,7,8,9]
  , testCase "mapRow" $
      mapRow @2 (\_ x -> x + 1) mat33 @?=
          fromListUnsafe [1,2,3,5,6,7,7,8,9]
  , testCase "mapColUnsafe" $
      mapColUnsafe (\_ x -> x + 1) 2 mat33 @?=
          fromListUnsafe [1,3,3,4,6,6,7,9,9]
  , testCase "mapCol" $
      mapCol @2 (\_ x -> x + 1) mat33 @?=
          fromListUnsafe [1,3,3,4,6,6,7,9,9]
#if MIN_VERSION_matrix(0,3,6)
  , testCase "mapPos" $
      mapPos (\(r,c) _ -> r - c) mat33 @?=
          fromListUnsafe [0,(-1),(-2),1,0,(-1),2,1,0]
#endif
  , testCase "zero" $
      zero @2 @2 @Int @?= fromListUnsafe [0,0,0,0]
  , testCase "matrix" $
      (matrix (\(i,j) -> 2*i-j) :: Matrix 2 4 Int) @?=
          fromListUnsafe [1,0,-1,-2,3,2,1,0]
  , testCase "identity" $
      identity @3 @Int @?=
          fromListUnsafe [1,0,0,0,1,0,0,0,1]
  , testCase "fromList" $
      (fromList [1..9] :: Maybe (Matrix 3 3 Int)) @?=
          (Just $ fromListUnsafe [1,2,3,4,5,6,7,8,9])
  , testCase "fromLists" $
      (fromLists [[1,2,3],[4,5,6],[7,8,9]] :: Maybe (Matrix 3 3 Int)) @?=
          (Just $ fromListUnsafe [1,2,3,4,5,6,7,8,9])
  , testCase "fromListsUnsafe" $
      (fromListsUnsafe [[1,2,3],[4,5,6],[7,8,9]] :: Matrix 3 3 Int) @?=
          fromListUnsafe [1,2,3,4,5,6,7,8,9]
  , testCase "toList" $
      toList mat33 @?= [1..9]
  , testCase "toLists" $
      toLists mat33 @?= [[1,2,3],[4,5,6],[7,8,9]]
  , testCase "permMatrix" $
      permMatrix @3 @2 @3 @Int @?= fromListUnsafe [1,0,0,0,0,1,0,1,0]
  , testCase "permMatrixUnsafe" $
      permMatrixUnsafe @3 @Int 2 3 @?= fromListUnsafe [1,0,0,0,0,1,0,1,0]
  , testCase "getElem" $
      getElem @2 @1 (fromListUnsafe @2 @2 [1..4]) @?= (3 :: Int)
  , testCase "unsafeGet" $
      unsafeGet 2 1 (fromListUnsafe @2 @2 [1..4]) @?= (3 :: Int)
  , testCase "setElem" $
      setElem @1 @2 0 (fromListUnsafe @1 @3 @Int [1,2,3]) @?=
          fromListUnsafe [1,0,3]
  , testCase "transpose" $
      transpose mat33 @?= fromListUnsafe [1,4,7,2,5,8,3,6,9]
  , testCase "extendTo" $
      extendTo @4 @5 0 mat33 @?= 
          fromListUnsafe [1,2,3,0,0,4,5,6,0,0,7,8,9,0,0,0,0,0,0,0]
  , testCase "submatrixUnsafe" $
      submatrixUnsafe @2 @2 1 2 mat33 @?= fromListUnsafe [2,3,5,6]
  , testCase "minorMatrixUnsafe" $
      minorMatrixUnsafe 2 2 mat33 @?= fromListUnsafe [1,3,7,9]
  , testCase "minorMatrix" $
      minorMatrix @2 @2 mat33 @?= fromListUnsafe [1,3,7,9]
  , testCase "splitAndJoin" $
      joinBlocks (splitBlocks @2 @2 mat33) @?= mat33
  , testCase "<|>" $
      fromListUnsafe @2 @2 [1,2,3,4] <|> fromListUnsafe @2 @2 [6,7,8,9] @?=
          fromListUnsafe @2 @4 @Int [1,2,6,7,3,4,8,9]
  , testCase "<->" $
      fromListUnsafe @2 @2 [1,2,3,4] <-> fromListUnsafe @2 @2 [6,7,8,9] @?=
          fromListUnsafe @4 @2 @Int [1,2,3,4,6,7,8,9]
  , testCase "scaleMatrix" $
      scaleMatrix 2 mat33 @?= fromListUnsafe [2,4,6,8,10,12,14,16,18]
  , testCase "scaleRowUnsafe" $
      scaleRowUnsafe 3 2 mat33 @?= fromListUnsafe [1,2,3,12,15,18,7,8,9]
  , testCase "scaleRow" $
      scaleRow @2 3 mat33 @?= fromListUnsafe [1,2,3,12,15,18,7,8,9]
  , testCase "ombineRowsUnsafe" $
      combineRowsUnsafe 2 2 1 mat33 @?= fromListUnsafe [1,2,3,6,9,12,7,8,9]
  , testCase "combineRows" $
      combineRows @2 @1 2 mat33 @?= fromListUnsafe [1,2,3,6,9,12,7,8,9]
  , testCase "switchRowsUnsafe" $
      switchRowsUnsafe 1 2 mat33 @?= fromListUnsafe [4,5,6,1,2,3,7,8,9]
  , testCase "switchRows" $
      switchRows @1 @2 mat33 @?= fromListUnsafe [4,5,6,1,2,3,7,8,9]
  , testCase "switchColsUnsafe" $
      switchColsUnsafe 1 2 mat33 @?= fromListUnsafe [2,1,3,5,4,6,8,7,9]
  , testCase "switchCols" $
      switchCols @1 @2 mat33 @?= fromListUnsafe [2,1,3,5,4,6,8,7,9]
  , testCase "luDecomp" $
      (luDecomp $ fromListUnsafe @3 @3 @Double [1,2,0,0,2,1,2,0,2]) @?=
          Just ( fromListUnsafe [2,0,2,0,2,(-1),0,0,2]
               , fromListUnsafe [1,0,0,(1/2),1,0,0,1,1]
               , fromListUnsafe [0,0,1,1,0,0,0,1,0]
               , 1 )
  , testCase "luDecomp'" $
      (luDecomp' $ fromListUnsafe @3 @2 @Double [1,0,0,2,2,1]) @?=
          Just ( fromListUnsafe [2,1,0,2,0,0]
               , fromListUnsafe [1,0,0,0,1,0,(1/2),(-1/4),1]
               , fromListUnsafe [0,0,1,0,1,0,1,0,0]
               , fromListUnsafe [1,0,0,1]
               , -1
               , 1 )
  -- TODO: Test case for cholDecomp
  , testCase "trace" $
      trace mat33 @?= 15
  , testCase "diagProd" $
      diagProd mat33 @?= 45
  ]


