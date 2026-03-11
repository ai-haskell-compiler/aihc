-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Data.DoublyLinkedList.STRef (tests) where

import Control.Monad (void)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.DoublyLinkedList.STRef
  ( cons,
    delete,
    empty,
    fromList,
    head,
    last,
    next,
    null,
    toList,
    value,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Prelude hiding (head, last, null)

tests :: TestTree
tests =
  testGroup
    "Data.DoublyLinkedList.STRef"
    [ constructionTests,
      traversalTests,
      queryTests,
      insertionTests,
      deletionTests,
      listConversionTests,
      integrationTests
    ]

unimplemented :: Assertion
unimplemented = assertFailure "unimplemented"

-- Construction
constructionTests :: TestTree
constructionTests =
  testGroup
    "Construction"
    [ emptyTests,
      fromListTests
    ]

emptyTests :: TestTree
emptyTests = testCase "empty" (pure ())

fromListTests :: TestTree
fromListTests = testCase "fromList" (pure ())

-- Traversal
traversalTests :: TestTree
traversalTests =
  testGroup "Traversal" [headTests, lastTests, nextTests, prevTests]

headTests :: TestTree
headTests =
  testGroup
    "head"
    [ testCase "head of empty list" $ do
        -- Construct empty list using empty
        let firstNodeValue1 :: Maybe Int
            firstNodeValue1 = runST $ do
              list <- empty
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue1 @?= Nothing

        -- Construct empty list using fromList
        let firstNodeValue2 :: Maybe Int
            firstNodeValue2 = runST $ do
              list <- fromList []
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue2 @?= Nothing,
      testCase "head of non-empty lists" $ do
        let firstNodeValue1 :: Maybe Int
            firstNodeValue1 = runST $ do
              list <- fromList [1]
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue1 @?= Just 1
        let firstNodeValue2 :: Maybe Int
            firstNodeValue2 = runST $ do
              list <- fromList [2, 1]
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue2 @?= Just 2
        let firstNodeValue3 :: Maybe Int
            firstNodeValue3 = runST $ do
              list <- fromList [3, 2, 1]
              firstNode <- head list
              pure (value <$> firstNode)
        firstNodeValue3 @?= Just 3
    ]

lastTests :: TestTree
lastTests =
  testGroup
    "last"
    [ testCase "last of empty list" $ do
        -- Construct empty list using empty
        let lastNodeValue1 :: Maybe Int
            lastNodeValue1 = runST $ do
              list <- empty
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue1 @?= Nothing

        -- Construct empty list using fromList
        let lastNodeValue2 :: Maybe Int
            lastNodeValue2 = runST $ do
              list <- fromList []
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue2 @?= Nothing,
      testCase "last of non-empty lists" $ do
        let lastNodeValue1 :: Maybe Int
            lastNodeValue1 = runST $ do
              list <- fromList [1]
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue1 @?= Just 1
        let lastNodeValue2 :: Maybe Int
            lastNodeValue2 = runST $ do
              list <- fromList [1, 2]
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue2 @?= Just 2
        let lastNodeValue3 :: Maybe Int
            lastNodeValue3 = runST $ do
              list <- fromList [1, 2, 3]
              lastNode <- last list
              pure (value <$> lastNode)
        lastNodeValue3 @?= Just 3
    ]

nextTests :: TestTree
nextTests =
  ignoreTest $
    testGroup
      "next"
      [ testCase "next on empty list" unimplemented,
        testCase "next on [1]" unimplemented,
        testCase "next on [3, 2, 1]" unimplemented,
        testCase "next on [1, 1, 1, 1, 1]" unimplemented
      ]

prevTests :: TestTree
prevTests =
  ignoreTest $
    testGroup
      "prev"
      [ testCase "prev on empty list" unimplemented,
        testCase "prev on [1]" unimplemented,
        testCase "prev on [3, 2, 1]" unimplemented,
        testCase "prev on [1, 1, 1, 1, 1]" unimplemented
      ]

-- Query
queryTests :: TestTree
queryTests = testGroup "Query" [nullTests, valueTests]

nullTests :: TestTree
nullTests =
  testGroup
    "null"
    [ testCase "null of empty list" $ do
        -- Construct empty list using empty
        let isNull1 :: Bool
            isNull1 = runST (empty >>= null)
        assertBool "empty list is null" isNull1

        -- Construct empty list using fromList
        let isNull2 :: Bool
            isNull2 = runST (fromList [] >>= null)
        assertBool "empty list is null" isNull2,
      testCase "null of non-empty list" $ do
        let isNull1 :: Bool
            isNull1 = runST (fromList [1 :: Int] >>= null)
        assertBool "non-empty list is not null" (not isNull1)
        let isNull2 :: Bool
            isNull2 = runST (fromList [1 :: Int, 2] >>= null)
        assertBool "non-empty list is not null" (not isNull2)
        let isNull3 :: Bool
            isNull3 = runST (fromList [1 :: Int, 2, 3] >>= null)
        assertBool "non-empty list is not null" (not isNull3)
    ]

valueTests :: TestTree
valueTests = testCase "value" (pure ())

-- Insertion
insertionTests :: TestTree
insertionTests =
  testGroup
    "Insertion"
    [ consTests,
      snocTests,
      insertBeforeTests,
      insertAfterTests,
      insertBeforeAfterTests
    ]

consTests :: TestTree
consTests =
  testGroup
    "cons"
    [ testCase "Create [1] with cons" $
        let xs :: [Int]
            xs = runST $ do
              ys <- empty
              void (cons ys 1)
              toList ys
         in xs @?= [1],
      testCase "Create [1, 2] with cons" $
        let xs :: [Int]
            xs = runST $ do
              ys <- empty
              void (cons ys 2)
              void (cons ys 1)
              toList ys
         in xs @?= [1, 2],
      testCase "Create [1, 2, 3] with cons" $
        let xs :: [Int]
            xs = runST $ do
              ys <- empty
              void (cons ys 3)
              void (cons ys 2)
              void (cons ys 1)
              toList ys
         in xs @?= [1, 2, 3]
    ]

snocTests :: TestTree
snocTests =
  testGroup
    "snoc"
    [ ignoreTest $ testCase "Create [1] with snoc" unimplemented,
      ignoreTest $ testCase "Create [1, 2] with snoc" unimplemented,
      ignoreTest $ testCase "Create [1, 2, 3] with snoc" unimplemented
    ]

insertBeforeTests :: TestTree
insertBeforeTests =
  testGroup
    "insertBefore"
    [ ignoreTest $ testCase "Create [1, 0] with insertBefore" unimplemented,
      ignoreTest $ testCase "Create [1, 2, 0] with insertBefore" unimplemented,
      ignoreTest $
        testCase "Create [1, 2, 3, 0] with insertBefore" unimplemented
    ]

insertAfterTests :: TestTree
insertAfterTests =
  testGroup
    "insertAfter"
    [ ignoreTest $ testCase "Create [0, 1] with insertAfter" unimplemented,
      ignoreTest $ testCase "Create [0, 1, 2] with insertAfter" unimplemented,
      ignoreTest $ testCase "Create [0, 1, 2, 3] with insertAfter" unimplemented
    ]

insertBeforeAfterTests :: TestTree
insertBeforeAfterTests =
  testGroup
    "insertBefore and insertAfter"
    [ ignoreTest $
        testCase
          "Create [-1, 0, 1] with insertBefore and insertAfter"
          unimplemented,
      ignoreTest $
        testCase
          "Create [-2, -1, 0, 1, 2] with insertBefore and insertAfter"
          unimplemented,
      ignoreTest $
        testCase
          "Create [-3, -2, -1, 0, 1, 2, 3] with insertBefore and insertAfter"
          unimplemented
    ]

-- Deletion
deletionTests :: TestTree
deletionTests = testGroup "Deletion" [deleteTests]

deleteTests :: TestTree
deleteTests =
  testGroup
    "delete"
    [ testCase "Turn [-2, -1, 0, 1, 2] -> [-2, -1, 1, 2] using delete" $ do
        let xs :: Maybe [Int]
            xs = runST $ runMaybeT $ do
              dllist <- lift $ fromList [-2 .. 2]
              node1 <- MaybeT $ head dllist
              node2 <- MaybeT $ next node1
              node3 <- MaybeT $ next node2
              lift (delete node3)
              lift (toList dllist)
         in xs @?= Just [-2, -1, 1, 2],
      testCase "Turn [1, 2, 3, 4, 5, 6] -> [2, 4, 6] using delete" $
        let xs :: Maybe [Int]
            xs = runST $ runMaybeT $ do
              dlist <- lift $ fromList [1 .. 6]
              node1 <- MaybeT (head dlist)
              lift (delete node1)
              node2 <- MaybeT (next node1)
              node3 <- MaybeT (next node2)
              node4 <- MaybeT (next node3)
              lift (delete node3)
              node5 <- MaybeT (next node4)
              lift (delete node5)
              lift (toList dlist)
         in xs @?= Just [2, 4, 6]
    ]

-- List conversion
listConversionTests :: TestTree
listConversionTests = testGroup "List conversion" [toListTests]

toListTests :: TestTree
toListTests = testCase "toList" (pure ())

-- Integration
integrationTests :: TestTree
integrationTests = ignoreTest $ testCase "Integration" unimplemented
