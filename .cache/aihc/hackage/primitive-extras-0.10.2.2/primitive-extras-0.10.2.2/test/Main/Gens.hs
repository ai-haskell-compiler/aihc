module Main.Gens where

import Data.Primitive
import Main.Transaction (Transaction)
import qualified Main.Transaction as Transaction
import qualified PrimitiveExtras.By6Bits as By6Bits
import Test.QuickCheck.Gen
import Prelude hiding (choose, index)

element :: Gen Int
element = choose (0, 999)

index :: Gen Int
index = choose (0, 9)

lookupTransaction :: (Show element, Eq element) => Gen (Transaction element)
lookupTransaction = Transaction.lookup <$> index

setTransaction :: Gen (Transaction Int)
setTransaction = Transaction.set <$> index <*> element

focusInsertTransaction :: Gen (Transaction Int)
focusInsertTransaction = Transaction.focusInsert <$> index <*> element

focusDeleteTransaction :: Gen (Transaction Int)
focusDeleteTransaction = Transaction.focusDelete <$> index

unsetTransaction :: Gen (Transaction element)
unsetTransaction = Transaction.unset <$> index

unfoldlTransaction :: (Show element, Eq element) => Gen (Transaction element)
unfoldlTransaction = pure Transaction.elementsUnfoldl

transaction :: Gen (Transaction Int)
transaction =
  frequency
    [ (9, lookupTransaction),
      (9, setTransaction),
      (9, unsetTransaction),
      (9, focusInsertTransaction),
      (9, focusDeleteTransaction)
    ]

maybeList :: Gen [Maybe Int]
maybeList =
  replicateM (finiteBitSize (undefined :: Int))
    $ frequency
    $ [ (4, fmap Just element),
        (1, pure Nothing)
      ]

sparseSmallArray :: Gen (By6Bits.By6Bits Int)
sparseSmallArray =
  By6Bits.maybeList <$> maybeList

primArray :: (Prim a) => Gen a -> Gen (PrimArray a)
primArray aGen = do
  list <- listOf aGen
  return (primArrayFromList list)
