{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Generators where

import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Text (Text)
import qualified Data.Text as X

import Squeather (SQLData(SQLNull, SQLText, SQLBlob, SQLInteger, SQLFloat))

sqlData :: MonadGen m => m SQLData
sqlData = Gen.choice [pure SQLNull, text, blob, integer, float]
  where
    text = fmap SQLText $ Gen.text (Range.exponential 0 1_000)
      (Gen.frequency [(4, Gen.ascii), (1, Gen.unicode)])
    blob = fmap SQLBlob $ Gen.bytes (Range.exponential 0 1_000)
    integer = fmap SQLInteger $ Gen.int64 Range.exponentialBounded
    float = fmap SQLFloat $ Gen.double (Range.exponentialFloatFrom 0 (-1_000) 1_000)

-- | Creates an SQL statement which produces a table with the given
-- number of columns.  Each column is named @cNUM@, where C is the
-- index, starting at 0, such as @c0@, @c1@, etc.  The table is
-- named @t@.
createTableStatement
  :: Int
  -- ^ Number of columns.  Must be at least 1; otherwise 'error' is
  -- applied.
  -> Text
createTableStatement nCols
  | nCols < 1 = error $ "createTableStatement: applied to less than one column: " ++ show nCols
  | otherwise = "CREATE TABLE t (c0" <> cols <> ");"
  where
    nOver1 = nCols - 1
    cols = X.concat . map mkCol $ [1..nOver1]
    mkCol n = ",c" `X.append` (X.pack . show $ n)

insertStatement
  :: Int
  -- ^ Number of columns
  -> Text
insertStatement nCols
  | nCols < 1 = error $ "insertStatement: applied to less than one column: " ++ show nCols
  | otherwise = "INSERT INTO t (c0" <> cols <> ") VALUES (:c0" <> colParams <> ");"
  where
    nOver1 = nCols - 1
    cols = X.concat . map mkCol $ [1..nOver1]
    mkCol n = ",c" `X.append` (X.pack . show $ n)
    colParams = X.concat . map mkParamCol $ [1..nOver1]
    mkParamCol n = ",:c" `X.append` (X.pack . show $ n)

-- | Pair 'SQLData' with column labels.
addColumnLabels :: [a] -> [(Text, a)]
addColumnLabels = zipWith f ([0..] :: [Int])
  where
    f num item = (X.pack $ ":c" <> show num, item)
