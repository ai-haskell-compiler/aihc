-- Do not edit! Automatically created with doctest-extract from src/Data/Ix/Enum.hs
module DocTest.Data.Ix.Enum where

import Data.Ix.Enum
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest


test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Ix.Enum:31: "
{-# LINE 31 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 31 "src/Data/Ix/Enum.hs" #-}
    range ('x','z')
  )
  [ExpectedLine [LineChunk "\"xyz\""]]
 DocTest.printPrefix "Data.Ix.Enum:33: "
{-# LINE 33 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 33 "src/Data/Ix/Enum.hs" #-}
    range (LT,GT)
  )
  [ExpectedLine [LineChunk "[LT,EQ,GT]"]]
 DocTest.printPrefix "Data.Ix.Enum:39: "
{-# LINE 39 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 39 "src/Data/Ix/Enum.hs" #-}
    index ('a','z') 'e'
  )
  [ExpectedLine [LineChunk "4"]]
 DocTest.printPrefix "Data.Ix.Enum:45: "
{-# LINE 45 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 45 "src/Data/Ix/Enum.hs" #-}
    unsafeIndex ('a','z') 'e'
  )
  [ExpectedLine [LineChunk "4"]]
 DocTest.printPrefix "Data.Ix.Enum:51: "
{-# LINE 51 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 51 "src/Data/Ix/Enum.hs" #-}
    inRange ('a','z') 'e'
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.Ix.Enum:53: "
{-# LINE 53 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 53 "src/Data/Ix/Enum.hs" #-}
    inRange ('x','z') 'a'
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.Ix.Enum:59: "
{-# LINE 59 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 59 "src/Data/Ix/Enum.hs" #-}
    rangeSize ('x','z')
  )
  [ExpectedLine [LineChunk "3"]]
 DocTest.printPrefix "Data.Ix.Enum:65: "
{-# LINE 65 "src/Data/Ix/Enum.hs" #-}
 DocTest.example(
{-# LINE 65 "src/Data/Ix/Enum.hs" #-}
    unsafeRangeSize ('x','z')
  )
  [ExpectedLine [LineChunk "3"]]
