module Main where

import Data.Primitive
import qualified Data.Serialize as Serialize
import qualified Data.Vector.Primitive as PrimitiveVector
import qualified Focus
import qualified Main.Gens as Gen
import qualified Main.Transaction as Transaction
import PrimitiveExtras.By6Bits (By6Bits)
import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified Test.QuickCheck as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude hiding (choose)

main :: IO ()
main =
  defaultMain
    $ testGroup "All"
    $ [ testGroup "SmallArray"
          $ [ testCase "set"
                $ let array = SmallArray.list [1, 2, 3]
                   in assertEqual
                        ""
                        [1, 4, 3]
                        (SmallArray.toList (SmallArray.set 1 4 array)),
              testCase "insert"
                $ let array = SmallArray.list [1, 2, 3]
                   in assertEqual
                        ""
                        [1, 4, 2, 3]
                        (SmallArray.toList (SmallArray.insert 1 4 array)),
              testCase "unset"
                $ let array = SmallArray.list [1, 2, 3]
                   in assertEqual
                        ""
                        [1, 3]
                        (SmallArray.toList (SmallArray.unset 1 array)),
              testCase "focus insert, when exists"
                $ let array = SmallArray.list [1, 2, 3]
                   in assertEqual
                        ""
                        [1, 4, 3]
                        (SmallArray.toList (snd (runIdentity (SmallArray.focusOnFoundElement (Focus.insert 4) (== 2) (const False) array)))),
              testCase "focus insert, when doesn't exist"
                $ let array = SmallArray.list [1, 2, 3]
                   in assertEqual
                        ""
                        [4, 1, 2, 3]
                        (SmallArray.toList (snd (runIdentity (SmallArray.focusOnFoundElement (Focus.insert 4) (== 4) (const False) array)))),
              testCase "focus delete"
                $ let array = SmallArray.list [1, 2, 3]
                   in assertEqual
                        ""
                        [1, 3]
                        (SmallArray.toList (snd (runIdentity (SmallArray.focusOnFoundElement Focus.delete (== 2) (const False) array))))
            ],
        testGroup "By6Bits"
          $ [ testCase "empty" $ do
                assertEqual
                  ""
                  (replicate (finiteBitSize (undefined :: Int)) Nothing)
                  (By6Bits.toMaybeList (By6Bits.empty :: By6Bits Int32)),
              testProperty "toMaybeList, maybeList" $ forAll Gen.maybeList $ \maybeList ->
                maybeList === By6Bits.toMaybeList (By6Bits.maybeList maybeList),
              testCase "unset"
                $ assertEqual
                  ""
                  ([Just 1, Nothing, Nothing, Just 3] <> replicate (finiteBitSize (undefined :: Int) - 4) Nothing)
                  (By6Bits.toMaybeList (By6Bits.unset 1 (By6Bits.maybeList [Just 1, Just 2, Nothing, Just 3]))),
              testTransactionProperty "set" Gen.setTransaction,
              testTransactionProperty "unset" Gen.unsetTransaction,
              testTransactionProperty "focusInsert" Gen.focusInsertTransaction,
              testTransactionProperty "focusDelete" Gen.focusDeleteTransaction,
              testTransactionProperty "unfoldl" Gen.unfoldlTransaction,
              testTransactionProperty "lookup" Gen.lookupTransaction,
              testTransactionProperty "composite" Gen.transaction
            ],
        testGroup "PrimArray"
          $ [ testProperty "Construction from primitive vector"
                $ let gen = do
                        offset <- choose (0, 2)
                        length <- choose (0, 100)
                        listSize <-
                          let minSize = offset + length
                           in choose (minSize, minSize + 100)
                        list :: [Int32] <- replicateM listSize arbitrary
                        return (list, offset, length)
                   in forAll gen $ \(inputList, offset, length) ->
                        let !inputVec = PrimitiveVector.fromList inputList
                            !sliceVec = PrimitiveVector.slice offset length inputVec
                            !sliceList = PrimitiveVector.toList sliceVec
                         in sliceList === primArrayToList (PrimArray.primitiveVector sliceVec),
              testProperty "Serializes well with as in memory" $ forAll (Gen.primArray Gen.element) $ \primArray ->
                Right primArray
                  === Serialize.runGet
                    (PrimArray.cerealGetAsInMemory Serialize.get)
                    (Serialize.runPut ((PrimArray.cerealPutAsInMemory Serialize.put) primArray))
            ]
      ]

testTransactionProperty :: String -> Gen (Transaction.Transaction Int) -> TestTree
testTransactionProperty name transactionGen =
  testProperty (showString "Transaction: " name)
    $ forAll ((,) <$> Gen.maybeList <*> transactionGen)
    $ \(maybeList, transaction) ->
      case transaction of
        Transaction.Transaction name applyToMaybeList applyToBy6Bits ->
          let ssa = By6Bits.maybeList maybeList
              (result1, newMaybeList) = runState applyToMaybeList maybeList
              (result2, newSsa) = runState applyToBy6Bits ssa
              newSsaMaybeList = By6Bits.toMaybeList newSsa
           in QuickCheck.counterexample
                ("transaction: " <> show name <> "\nnewMaybeList1: " <> show newMaybeList <> "\nnewMaybeList2: " <> show newSsaMaybeList <> "\nresult1: " <> show result1 <> "\nresult2: " <> show result2)
                (newMaybeList == By6Bits.toMaybeList newSsa && result1 == result2)
