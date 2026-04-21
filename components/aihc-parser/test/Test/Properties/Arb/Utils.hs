module Test.Properties.Arb.Utils
  ( optional,
    smallList0,
    smallList1,
    smallList2,
  )
where

import Control.Monad (replicateM)
import Test.QuickCheck

-- | Optionally generate a value, or Nothing.
optional :: Gen a -> Gen (Maybe a)
optional gen = do
  n <- getSize
  if n <= 0 then pure Nothing else oneof [pure Nothing, Just <$> gen]

-- | Generate a list of 0 to 3 elements.
smallList0 :: Gen a -> Gen [a]
smallList0 gen = do
  limit <- getSize
  n <- chooseInt (0, min 3 (max 0 limit))
  replicateM n (scale (`div` n) gen)

-- | Generate a list of 1 to 3 elements.
smallList1 :: Gen a -> Gen [a]
smallList1 gen = do
  limit <- getSize
  n <- chooseInt (1, min 3 (max 1 limit))
  replicateM n (scale (`div` n) gen)

-- | Generate a list of 2 to 3 elements.
smallList2 :: Gen a -> Gen [a]
smallList2 gen = do
  limit <- getSize
  n <- chooseInt (2, min 3 (max 2 limit))
  replicateM n (scale (`div` n) gen)
