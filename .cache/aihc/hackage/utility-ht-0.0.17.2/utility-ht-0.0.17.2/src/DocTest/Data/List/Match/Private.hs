-- Do not edit! Automatically created with doctest-extract from src/Data/List/Match/Private.hs
{-# LINE 15 "src/Data/List/Match/Private.hs" #-}

module DocTest.Data.List.Match.Private where

import Data.List.Match.Private
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 16 "src/Data/List/Match/Private.hs" #-}
import     qualified Data.List.Match.Private as Match
import     qualified Data.List as List

import     qualified Test.QuickCheck as QC

newtype     List = List [Integer] deriving (Show)
instance     QC.Arbitrary List where
       arbitrary = fmap List QC.arbitrary
       shrink (List xs) = map List $ QC.shrink xs

newtype     Shape = Shape [Ordering] deriving (Show)
instance     QC.Arbitrary Shape where
       arbitrary = fmap Shape QC.arbitrary
       shrink (Shape xs) = map Shape $ QC.shrink xs

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.List.Match.Private:34: "
{-# LINE 34 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 34 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> Match.take xs ys == List.take (length xs) ys
  )
 DocTest.printPrefix "Data.List.Match.Private:46: "
{-# LINE 46 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 46 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> Match.drop xs ys == List.drop (length xs) ys
  )
 DocTest.printPrefix "Data.List.Match.Private:47: "
{-# LINE 47 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 47 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> Match.take xs ys ++ Match.drop xs ys == ys
  )
 DocTest.printPrefix "Data.List.Match.Private:54: "
{-# LINE 54 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 54 "src/Data/List/Match/Private.hs" #-}
           \(Shape xs) (List ys) -> Match.drop xs ys == dropRec xs ys
  )
 DocTest.printPrefix "Data.List.Match.Private:63: "
{-# LINE 63 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 63 "src/Data/List/Match/Private.hs" #-}
           \(Shape xs) (List ys) -> Match.drop xs ys == drop0 xs ys
  )
 DocTest.printPrefix "Data.List.Match.Private:70: "
{-# LINE 70 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 70 "src/Data/List/Match/Private.hs" #-}
           \(Shape xs) (List ys) -> Match.drop xs ys == drop1 xs ys
  )
 DocTest.printPrefix "Data.List.Match.Private:75: "
{-# LINE 75 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 75 "src/Data/List/Match/Private.hs" #-}
           \(Shape xs) (List ys) -> Match.drop xs ys == drop2 xs ys
  )
 DocTest.printPrefix "Data.List.Match.Private:84: "
{-# LINE 84 "src/Data/List/Match/Private.hs" #-}
 DocTest.example(
{-# LINE 84 "src/Data/List/Match/Private.hs" #-}
    laxTail ""
  )
  [ExpectedLine [LineChunk "\"\""]]
 DocTest.printPrefix "Data.List.Match.Private:86: "
{-# LINE 86 "src/Data/List/Match/Private.hs" #-}
 DocTest.example(
{-# LINE 86 "src/Data/List/Match/Private.hs" #-}
    laxTail "a"
  )
  [ExpectedLine [LineChunk "\"\""]]
 DocTest.printPrefix "Data.List.Match.Private:88: "
{-# LINE 88 "src/Data/List/Match/Private.hs" #-}
 DocTest.example(
{-# LINE 88 "src/Data/List/Match/Private.hs" #-}
    laxTail "ab"
  )
  [ExpectedLine [LineChunk "\"b\""]]
 DocTest.printPrefix "Data.List.Match.Private:94: "
{-# LINE 94 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 94 "src/Data/List/Match/Private.hs" #-}
           \(List xs) -> Match.laxTail xs == Match.laxTail0 xs
  )
 DocTest.printPrefix "Data.List.Match.Private:99: "
{-# LINE 99 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 99 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> Match.splitAt xs ys == (Match.take xs ys, Match.drop xs ys)
  )
 DocTest.printPrefix "Data.List.Match.Private:100: "
{-# LINE 100 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 100 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> Match.splitAt xs ys == List.splitAt (length xs) ys
  )
 DocTest.printPrefix "Data.List.Match.Private:110: "
{-# LINE 110 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 110 "src/Data/List/Match/Private.hs" #-}
           \(Shape xs) (List ys) -> Match.takeRev xs ys == reverse (Match.take xs (reverse ys))
  )
 DocTest.printPrefix "Data.List.Match.Private:114: "
{-# LINE 114 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 114 "src/Data/List/Match/Private.hs" #-}
           \(Shape xs) (List ys) -> Match.dropRev xs ys == reverse (Match.drop xs (reverse ys))
  )
 DocTest.printPrefix "Data.List.Match.Private:122: "
{-# LINE 122 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 122 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> equalLength xs ys == (length xs == length ys)
  )
 DocTest.printPrefix "Data.List.Match.Private:134: "
{-# LINE 134 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 134 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> compareLength xs ys == compare (length xs) (length ys)
  )
 DocTest.printPrefix "Data.List.Match.Private:144: "
{-# LINE 144 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 144 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> Match.compareLength xs ys == Match.compareLength0 xs ys
  )
 DocTest.printPrefix "Data.List.Match.Private:156: "
{-# LINE 156 "src/Data/List/Match/Private.hs" #-}
 DocTest.property(
{-# LINE 156 "src/Data/List/Match/Private.hs" #-}
      \(Shape xs) (List ys) -> Match.compareLength xs ys == Match.compareLength1 xs ys
  )
 DocTest.printPrefix "Data.List.Match.Private:166: "
{-# LINE 166 "src/Data/List/Match/Private.hs" #-}
 DocTest.example(
{-# LINE 166 "src/Data/List/Match/Private.hs" #-}
    lessOrEqualLength "" undefined
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.Match.Private:181: "
{-# LINE 181 "src/Data/List/Match/Private.hs" #-}
 DocTest.example(
{-# LINE 181 "src/Data/List/Match/Private.hs" #-}
    shorterList (shorterList (repeat 'a') (repeat 'b')) "abc"
  )
  [ExpectedLine [LineChunk "\"abc\""]]
 DocTest.printPrefix "Data.List.Match.Private:200: "
{-# LINE 200 "src/Data/List/Match/Private.hs" #-}
 DocTest.example(
{-# LINE 200 "src/Data/List/Match/Private.hs" #-}
    List.take 3 $ shorterListEq ("abc" ++ repeat 'a') ("abcdef" ++ repeat 'b')
  )
  [ExpectedLine [LineChunk "\"abc\""]]
