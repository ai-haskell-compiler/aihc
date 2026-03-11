-- Do not edit! Automatically created with doctest-extract from src/Data/List/Reverse/Private.hs
{-# LINE 9 "src/Data/List/Reverse/Private.hs" #-}

module DocTest.Data.List.Reverse.Private where

import Data.List.Reverse.Private
import qualified Test.DocTest.Driver as DocTest

{-# LINE 10 "src/Data/List/Reverse/Private.hs" #-}
import     Test.Utility (forAllPredicates)
import     qualified Data.List.Reverse.StrictElement as Rev
import     Prelude hiding (dropWhile, takeWhile)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.List.Reverse.Private:16: "
{-# LINE 16 "src/Data/List/Reverse/Private.hs" #-}
 DocTest.property(
{-# LINE 16 "src/Data/List/Reverse/Private.hs" #-}
      forAllPredicates $ \p xs -> dropWhile p xs == Rev.dropWhile p xs
  )
 DocTest.printPrefix "Data.List.Reverse.Private:23: "
{-# LINE 23 "src/Data/List/Reverse/Private.hs" #-}
 DocTest.property(
{-# LINE 23 "src/Data/List/Reverse/Private.hs" #-}
      forAllPredicates $ \p xs -> takeWhile0 p xs == Rev.takeWhile p xs
  )
 DocTest.printPrefix "Data.List.Reverse.Private:32: "
{-# LINE 32 "src/Data/List/Reverse/Private.hs" #-}
 DocTest.property(
{-# LINE 32 "src/Data/List/Reverse/Private.hs" #-}
      forAllPredicates $ \p xs -> takeWhile1 p xs == Rev.takeWhile p xs
  )
 DocTest.printPrefix "Data.List.Reverse.Private:46: "
{-# LINE 46 "src/Data/List/Reverse/Private.hs" #-}
 DocTest.property(
{-# LINE 46 "src/Data/List/Reverse/Private.hs" #-}
      forAllPredicates $ \p xs -> takeWhile2 p xs == Rev.takeWhile p xs
  )
