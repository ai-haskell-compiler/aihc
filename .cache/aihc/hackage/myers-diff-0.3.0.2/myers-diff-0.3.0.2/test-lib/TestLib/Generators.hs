{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module TestLib.Generators where

import Data.Function
import Data.String.Interpolate
import Data.Text as T
import Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text ()


newtype InsertOrDelete = InsertOrDelete (Text, Text)
  deriving (Show, Eq)
instance Arbitrary InsertOrDelete where
  arbitrary = InsertOrDelete <$> oneof [arbitraryInsert, arbitraryDelete]

newtype DocInsertOrDelete = DocInsertOrDelete (Text, Text)
  deriving (Show, Eq)
instance Arbitrary DocInsertOrDelete where
  arbitrary = DocInsertOrDelete <$> oneof [arbitraryDocInsert, arbitraryDocDelete]

newtype MultiInsertOrDelete = MultiInsertOrDelete (Text, Text)
  deriving (Show, Eq)
instance Arbitrary MultiInsertOrDelete where
  arbitrary = arbitrary >>= (MultiInsertOrDelete <$>) . arbitraryChangesSized

newtype DocMultiInsertOrDelete = DocMultiInsertOrDelete (Text, Text)
  deriving (Show, Eq)
instance Arbitrary DocMultiInsertOrDelete where
  arbitrary = arbitrary >>= (DocMultiInsertOrDelete <$>) . arbitraryChangesSized

-- * Apply a series of changes

arbitraryChangesSized :: Text -> Gen (Text, Text)
arbitraryChangesSized initial = sized $ \n -> flip fix (n, initial) $ \loop -> \case
  (0, x) -> return (initial, x)
  (j, cur) -> do
    -- Prefer inserts to deletes at a 3:1 ratio
    next <- oneof [arbitraryInsertOn cur, arbitraryInsertOn cur, arbitraryInsertOn cur, arbitraryDeleteOn cur]
    loop (j - 1, next)

-- * Inserts and deletes

-- | Arbitrarily insert up to size parameter characters
arbitraryDocInsert :: Gen (Text, Text)
arbitraryDocInsert = arbitraryDoc >>= (\initial -> (initial, ) <$> arbitraryInsertOn initial)

arbitraryInsert :: Gen (Text, Text)
arbitraryInsert = arbitrary >>= (\initial -> (initial, ) <$> arbitraryInsertOn initial)

arbitraryInsertOn :: Text -> Gen Text
arbitraryInsertOn initial = do
  toInsert <- arbitrary

  pos <- chooseInt (0, T.length initial)
  let (x, y) = T.splitAt pos initial

  return (x <> toInsert <> y)


arbitraryDelete :: Gen (Text, Text)
arbitraryDelete = arbitrary >>= (\initial -> (initial, ) <$> arbitraryDeleteOn initial)

arbitraryDocDelete :: Gen (Text, Text)
arbitraryDocDelete = arbitraryDoc >>= (\initial -> (initial, ) <$> arbitraryDeleteOn initial)

-- | Arbitrarily delete up to size parameter characters
arbitraryDeleteOn :: Text -> Gen Text
arbitraryDeleteOn initial = do
  size <- getSize
  amountToDelete :: Int <- chooseInt (1, size)

  pos1 <- chooseInt (0, max 0 (T.length initial - 1))
  pos2 <- chooseInt (pos1, min (pos1 + amountToDelete) (T.length initial))

  let (x, y) = T.splitAt pos1 initial
  let (_, z) = T.splitAt (pos2 - pos1) y

  return (x <> z)

-- * Docs

arbitraryLine :: Gen Text
arbitraryLine = oneof [pure "", arbitrary]

arbitraryDoc :: Gen Text
arbitraryDoc = T.intercalate "\n" <$> listOf arbitraryLine

arbitraryAlphanumericString :: Gen Text
arbitraryAlphanumericString = T.pack <$> listOf alphaNumericChar

alphaNumericChar :: Gen Char
alphaNumericChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '])

file1 :: Text
file1 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       abc

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]

file2 :: Text
file2 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       a

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]
