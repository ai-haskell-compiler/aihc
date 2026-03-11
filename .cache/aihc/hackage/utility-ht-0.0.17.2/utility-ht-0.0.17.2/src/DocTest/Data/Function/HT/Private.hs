-- Do not edit! Automatically created with doctest-extract from src/Data/Function/HT/Private.hs
{-# LINE 7 "src/Data/Function/HT/Private.hs" #-}

module DocTest.Data.Function.HT.Private where

import Data.Function.HT.Private
import qualified Test.DocTest.Driver as DocTest

{-# LINE 8 "src/Data/Function/HT/Private.hs" #-}
import     Test.QuickCheck (NonNegative(NonNegative))

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Function.HT.Private:22: "
{-# LINE 22 "src/Data/Function/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 22 "src/Data/Function/HT/Private.hs" #-}
      \(NonNegative n) x -> nest n succ x == nest1 n succ (x::Integer)
  )
 DocTest.printPrefix "Data.Function.HT.Private:23: "
{-# LINE 23 "src/Data/Function/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 23 "src/Data/Function/HT/Private.hs" #-}
      \(NonNegative n) x -> nest n succ x == nest2 n succ (x::Integer)
  )
 DocTest.printPrefix "Data.Function.HT.Private:48: "
{-# LINE 48 "src/Data/Function/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 48 "src/Data/Function/HT/Private.hs" #-}
      \a0 a (NonNegative n) -> powerAssociative (+) a0 a n == (powerAssociativeList (+) a0 a n :: Integer)
  )
 DocTest.printPrefix "Data.Function.HT.Private:49: "
{-# LINE 49 "src/Data/Function/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 49 "src/Data/Function/HT/Private.hs" #-}
      \a0 a (NonNegative n) -> powerAssociative (+) a0 a n == (powerAssociativeNaive (+) a0 a n :: Integer)
  )
