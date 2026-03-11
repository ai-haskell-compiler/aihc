-- Do not edit! Automatically created with doctest-extract from src/Data/List/HT/Private.hs
{-# LINE 21 "src/Data/List/HT/Private.hs" #-}

module DocTest.Data.List.HT.Private where

import Data.List.HT.Private
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 22 "src/Data/List/HT/Private.hs" #-}
import     qualified Test.QuickCheck as QC
import     Test.Utility (forAllPredicates)
import     Test.QuickCheck (NonNegative(NonNegative), Positive(Positive), NonEmptyList(NonEmpty))
import     qualified Data.List as List
import     Data.List (transpose)
import     Data.Maybe.HT (toMaybe)
import     Data.Maybe (mapMaybe, isNothing)
import     Data.Char (isLetter, isUpper, toUpper)
import     Data.Eq.HT (equating)
import     Control.Monad (liftM2)

divMaybe     :: Int -> Int -> Maybe Int
divMaybe     m n = case divMod n m of (q,0) -> Just q; _ -> Nothing

forAllMaybeFn     :: (QC.Testable test) => ((Int -> Maybe Int) -> test) -> QC.Property
forAllMaybeFn     prop = QC.forAll (QC.choose (1,4)) $ prop . divMaybe

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.List.HT.Private:101: "
{-# LINE 101 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 101 "src/Data/List/HT/Private.hs" #-}
    groupBy (<) "abcdebcdef"
  )
  [ExpectedLine [LineChunk "[\"abcde\",\"bcdef\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:108: "
{-# LINE 108 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 108 "src/Data/List/HT/Private.hs" #-}
    List.groupBy (<) "abcdebcdef"
  )
  [ExpectedLine [LineChunk "[\"abcdebcdef\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:179: "
{-# LINE 179 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 179 "src/Data/List/HT/Private.hs" #-}
    words "a  a"
  )
  [ExpectedLine [LineChunk "[\"a\",\"a\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:181: "
{-# LINE 181 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 181 "src/Data/List/HT/Private.hs" #-}
    chop (' '==) "a  a"
  )
  [ExpectedLine [LineChunk "[\"a\",\"\",\"a\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:184: "
{-# LINE 184 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 184 "src/Data/List/HT/Private.hs" #-}
    lines "a\n\na"
  )
  [ExpectedLine [LineChunk "[\"a\",\"\",\"a\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:186: "
{-# LINE 186 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 186 "src/Data/List/HT/Private.hs" #-}
    chop ('\n'==) "a\n\na"
  )
  [ExpectedLine [LineChunk "[\"a\",\"\",\"a\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:189: "
{-# LINE 189 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 189 "src/Data/List/HT/Private.hs" #-}
    lines "a\n"
  )
  [ExpectedLine [LineChunk "[\"a\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:191: "
{-# LINE 191 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 191 "src/Data/List/HT/Private.hs" #-}
    chop ('\n'==) "a\n"
  )
  [ExpectedLine [LineChunk "[\"a\",\"\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:220: "
{-# LINE 220 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 220 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p xs -> uncurry (++) (breakAfter p xs) == xs
  )
 DocTest.printPrefix "Data.List.HT.Private:239: "
{-# LINE 239 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 239 "src/Data/List/HT/Private.hs" #-}
           forAllPredicates $ \p xs -> breakAfterRec p xs == breakAfterFoldr p xs
  )
 DocTest.printPrefix "Data.List.HT.Private:247: "
{-# LINE 247 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 247 "src/Data/List/HT/Private.hs" #-}
           forAllPredicates $ \p xs -> breakAfterRec p xs == breakAfterBreak p xs
  )
 DocTest.printPrefix "Data.List.HT.Private:254: "
{-# LINE 254 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 254 "src/Data/List/HT/Private.hs" #-}
           forAllPredicates $ \p xs -> breakAfterRec p xs == breakAfterTakeUntil p xs
  )
 DocTest.printPrefix "Data.List.HT.Private:267: "
{-# LINE 267 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 267 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p xs -> takeUntil p xs == fst (breakAfter p xs)
  )
 DocTest.printPrefix "Data.List.HT.Private:280: "
{-# LINE 280 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 280 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p xs -> concat (segmentAfter p xs) == xs
  )
 DocTest.printPrefix "Data.List.HT.Private:281: "
{-# LINE 281 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 281 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p xs -> length (filter p xs) == length (tail (segmentAfter p xs))
  )
 DocTest.printPrefix "Data.List.HT.Private:282: "
{-# LINE 282 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 282 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p -> all (p . last) . init . segmentAfter p
  )
 DocTest.printPrefix "Data.List.HT.Private:283: "
{-# LINE 283 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 283 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p -> all (all (not . p) . init) . init . segmentAfter p
  )
 DocTest.printPrefix "Data.List.HT.Private:287: "
{-# LINE 287 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 287 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p x -> flip seq True . (!!100) . concat . segmentAfter p . cycle . (x:)
  )
 DocTest.printPrefix "Data.List.HT.Private:309: "
{-# LINE 309 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 309 "src/Data/List/HT/Private.hs" #-}
    segmentBefore isUpper "AbcdXyz"
  )
  [ExpectedLine [LineChunk "[\"\",\"Abcd\",\"Xyz\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:311: "
{-# LINE 311 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 311 "src/Data/List/HT/Private.hs" #-}
    segmentBefore isUpper "kAbcdXYZ"
  )
  [ExpectedLine [LineChunk "[\"k\",\"Abcd\",\"X\",\"Y\",\"Z\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:314: "
{-# LINE 314 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 314 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p xs -> concat (segmentBefore p xs) == xs
  )
 DocTest.printPrefix "Data.List.HT.Private:315: "
{-# LINE 315 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 315 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p xs -> length (filter p xs) == length (tail (segmentBefore p xs))
  )
 DocTest.printPrefix "Data.List.HT.Private:316: "
{-# LINE 316 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 316 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p -> all (p . head) . tail . segmentBefore p
  )
 DocTest.printPrefix "Data.List.HT.Private:317: "
{-# LINE 317 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 317 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p -> all (all (not . p) . tail) . tail . segmentBefore p
  )
 DocTest.printPrefix "Data.List.HT.Private:318: "
{-# LINE 318 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 318 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p x -> flip seq True . (!!100) . concat . segmentBefore p . cycle . (x:)
  )
 DocTest.printPrefix "Data.List.HT.Private:330: "
{-# LINE 330 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 330 "src/Data/List/HT/Private.hs" #-}
           forAllPredicates $ \p xs -> segmentBefore p xs == segmentBefore' p xs
  )
 DocTest.printPrefix "Data.List.HT.Private:341: "
{-# LINE 341 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 341 "src/Data/List/HT/Private.hs" #-}
           forAllPredicates $ \p xs -> segmentBefore p xs == segmentBefore'' p xs
  )
 DocTest.printPrefix "Data.List.HT.Private:353: "
{-# LINE 353 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 353 "src/Data/List/HT/Private.hs" #-}
    segmentBeforeJust (\c -> toMaybe (isLetter c) (toUpper c)) "123a5345b---"
  )
  [ExpectedLine [LineChunk "(\"123\",[('A',\"5345\"),('B',\"---\")])"]]
 DocTest.printPrefix "Data.List.HT.Private:369: "
{-# LINE 369 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 369 "src/Data/List/HT/Private.hs" #-}
    segmentAfterJust (\c -> toMaybe (isLetter c) (toUpper c)) "123a5345b---"
  )
  [ExpectedLine [LineChunk "([(\"123\",'A'),(\"5345\",'B')],\"---\")"]]
 DocTest.printPrefix "Data.List.HT.Private:382: "
{-# LINE 382 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 382 "src/Data/List/HT/Private.hs" #-}
    segmentBeforeRight [Left 'a', Right LT, Right GT, Left 'b']
  )
  [ExpectedLine [LineChunk "(\"a\",[(LT,\"\"),(GT,\"b\")])"]]
 DocTest.printPrefix "Data.List.HT.Private:385: "
{-# LINE 385 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 385 "src/Data/List/HT/Private.hs" #-}
      forAllMaybeFn $ \f xs -> segmentBeforeJust f xs == segmentBeforeRight (map (\x -> maybe (Left x) Right (f x)) xs)
  )
 DocTest.printPrefix "Data.List.HT.Private:399: "
{-# LINE 399 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 399 "src/Data/List/HT/Private.hs" #-}
    segmentAfterRight [Left 'a', Right LT, Right GT, Left 'b']
  )
  [ExpectedLine [LineChunk "([(\"a\",LT),(\"\",GT)],\"b\")"]]
 DocTest.printPrefix "Data.List.HT.Private:402: "
{-# LINE 402 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 402 "src/Data/List/HT/Private.hs" #-}
      forAllMaybeFn $ \f xs -> segmentAfterJust f xs == segmentAfterRight (map (\x -> maybe (Left x) Right (f x)) xs)
  )
 DocTest.printPrefix "Data.List.HT.Private:425: "
{-# LINE 425 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 425 "src/Data/List/HT/Private.hs" #-}
    removeEach "abc"
  )
  [ExpectedLine [LineChunk "[('a',\"bc\"),('b',\"ac\"),('c',\"ab\")]"]]
 DocTest.printPrefix "Data.List.HT.Private:427: "
{-# LINE 427 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 427 "src/Data/List/HT/Private.hs" #-}
    removeEach "a"
  )
  [ExpectedLine [LineChunk "[('a',\"\")]"]]
 DocTest.printPrefix "Data.List.HT.Private:429: "
{-# LINE 429 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 429 "src/Data/List/HT/Private.hs" #-}
    removeEach ""
  )
  [ExpectedLine [LineChunk "[]"]]
 DocTest.printPrefix "Data.List.HT.Private:437: "
{-# LINE 437 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 437 "src/Data/List/HT/Private.hs" #-}
    splitEverywhere "abc"
  )
  [ExpectedLine [LineChunk "[(\"\",'a',\"bc\"),(\"a\",'b',\"c\"),(\"ab\",'c',\"\")]"]]
 DocTest.printPrefix "Data.List.HT.Private:439: "
{-# LINE 439 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 439 "src/Data/List/HT/Private.hs" #-}
    splitEverywhere "a"
  )
  [ExpectedLine [LineChunk "[(\"\",'a',\"\")]"]]
 DocTest.printPrefix "Data.List.HT.Private:441: "
{-# LINE 441 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 441 "src/Data/List/HT/Private.hs" #-}
    splitEverywhere ""
  )
  [ExpectedLine [LineChunk "[]"]]
 DocTest.printPrefix "Data.List.HT.Private:464: "
{-# LINE 464 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 464 "src/Data/List/HT/Private.hs" #-}
      \(NonEmpty xs) -> splitLast (xs::String)  ==  (init xs, last xs)
  )
 DocTest.printPrefix "Data.List.HT.Private:484: "
{-# LINE 484 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 484 "src/Data/List/HT/Private.hs" #-}
      \xs -> maybe True ((init xs, last xs) == ) (viewR (xs::String))
  )
 DocTest.printPrefix "Data.List.HT.Private:505: "
{-# LINE 505 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 505 "src/Data/List/HT/Private.hs" #-}
      \xs -> switchR True (\ixs lxs -> ixs == init xs && lxs == last xs) (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:519: "
{-# LINE 519 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 519 "src/Data/List/HT/Private.hs" #-}
      \n xs -> takeRev n (xs::String) == reverse (take n (reverse xs))
  )
 DocTest.printPrefix "Data.List.HT.Private:528: "
{-# LINE 528 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 528 "src/Data/List/HT/Private.hs" #-}
      \n xs -> dropRev n (xs::String) == reverse (drop n (reverse xs))
  )
 DocTest.printPrefix "Data.List.HT.Private:536: "
{-# LINE 536 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 536 "src/Data/List/HT/Private.hs" #-}
      \n xs -> splitAtRev n (xs::String) == (dropRev n xs, takeRev n xs)
  )
 DocTest.printPrefix "Data.List.HT.Private:537: "
{-# LINE 537 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 537 "src/Data/List/HT/Private.hs" #-}
      \n xs -> (xs::String) == uncurry (++) (splitAtRev n xs)
  )
 DocTest.printPrefix "Data.List.HT.Private:551: "
{-# LINE 551 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 551 "src/Data/List/HT/Private.hs" #-}
    maybePrefixOf "abc" "abcdef"
  )
  [ExpectedLine [LineChunk "Just \"def\""]]
 DocTest.printPrefix "Data.List.HT.Private:553: "
{-# LINE 553 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 553 "src/Data/List/HT/Private.hs" #-}
    maybePrefixOf "def" "abcdef"
  )
  [ExpectedLine [LineChunk "Nothing"]]
 DocTest.printPrefix "Data.List.HT.Private:562: "
{-# LINE 562 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 562 "src/Data/List/HT/Private.hs" #-}
    maybeSuffixOf "abc" "abcdef"
  )
  [ExpectedLine [LineChunk "Nothing"]]
 DocTest.printPrefix "Data.List.HT.Private:564: "
{-# LINE 564 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 564 "src/Data/List/HT/Private.hs" #-}
    maybeSuffixOf "def" "abcdef"
  )
  [ExpectedLine [LineChunk "Just \"abc\""]]
 DocTest.printPrefix "Data.List.HT.Private:575: "
{-# LINE 575 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 575 "src/Data/List/HT/Private.hs" #-}
      forAllMaybeFn $ \f xs -> partitionMaybe f xs == (mapMaybe f xs, filter (isNothing . f) xs)
  )
 DocTest.printPrefix "Data.List.HT.Private:576: "
{-# LINE 576 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 576 "src/Data/List/HT/Private.hs" #-}
      forAllPredicates $ \p xs -> partition p xs == partitionMaybe (\x -> toMaybe (p x) x) xs
  )
 DocTest.printPrefix "Data.List.HT.Private:589: "
{-# LINE 589 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 589 "src/Data/List/HT/Private.hs" #-}
    takeWhileJust [Just 'a', Just 'b', Nothing, Just 'c']
  )
  [ExpectedLine [LineChunk "\"ab\""]]
 DocTest.printPrefix "Data.List.HT.Private:594: "
{-# LINE 594 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 594 "src/Data/List/HT/Private.hs" #-}
    takeWhileJust $ map (fmap fst . viewL) ["abc","def","","xyz"]
  )
  [ExpectedLine [LineChunk "\"ad\""]]
 DocTest.printPrefix "Data.List.HT.Private:615: "
{-# LINE 615 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 615 "src/Data/List/HT/Private.hs" #-}
           forAllMaybeFn $ \f xs -> dropWhileNothing f xs == dropWhileNothingRec f xs
  )
 DocTest.printPrefix "Data.List.HT.Private:622: "
{-# LINE 622 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 622 "src/Data/List/HT/Private.hs" #-}
           forAllMaybeFn $ \f xs -> snd (breakJust f xs) == dropWhileNothing f xs
  )
 DocTest.printPrefix "Data.List.HT.Private:633: "
{-# LINE 633 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 633 "src/Data/List/HT/Private.hs" #-}
           forAllMaybeFn $ \f xs -> breakJust f xs == breakJustRemoveEach f xs
  )
 DocTest.printPrefix "Data.List.HT.Private:641: "
{-# LINE 641 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 641 "src/Data/List/HT/Private.hs" #-}
           forAllMaybeFn $ \f xs -> breakJust f xs == breakJustPartial f xs
  )
 DocTest.printPrefix "Data.List.HT.Private:669: "
{-# LINE 669 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 669 "src/Data/List/HT/Private.hs" #-}
    sieve 6 ['a'..'z']
  )
  [ExpectedLine [LineChunk "\"agmsy\""]]
 DocTest.printPrefix "Data.List.HT.Private:676: "
{-# LINE 676 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 676 "src/Data/List/HT/Private.hs" #-}
           \(Positive n) xs -> sieve n xs == sieve' n (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:679: "
{-# LINE 679 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 679 "src/Data/List/HT/Private.hs" #-}
           \(Positive n) xs -> sieve n xs == sieve'' n (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:682: "
{-# LINE 682 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 682 "src/Data/List/HT/Private.hs" #-}
           \(Positive n) xs -> sieve n xs == sieve''' n (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:691: "
{-# LINE 691 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 691 "src/Data/List/HT/Private.hs" #-}
    sliceHorizontal 6 ['a'..'z']
  )
  [ExpectedLine [LineChunk "[\"agmsy\",\"bhntz\",\"ciou\",\"djpv\",\"ekqw\",\"flrx\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:694: "
{-# LINE 694 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 694 "src/Data/List/HT/Private.hs" #-}
      \(NonEmpty xs) -> QC.forAll (QC.choose (1, length xs)) $ \n -> sliceHorizontal n xs == transpose (sliceVertical n (xs::String))
  )
 DocTest.printPrefix "Data.List.HT.Private:695: "
{-# LINE 695 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 695 "src/Data/List/HT/Private.hs" #-}
      \(NonEmpty xs) -> QC.forAll (QC.choose (1, length xs)) $ \n -> sliceVertical  n xs == transpose (sliceHorizontal n (xs::String))
  )
 DocTest.printPrefix "Data.List.HT.Private:699: "
{-# LINE 699 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 699 "src/Data/List/HT/Private.hs" #-}
    sliceHorizontal 4 ([]::[Int])
  )
  [ExpectedLine [LineChunk "[[],[],[],[]]"]]
 DocTest.printPrefix "Data.List.HT.Private:707: "
{-# LINE 707 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 707 "src/Data/List/HT/Private.hs" #-}
           \(NonNegative n) xs -> sliceHorizontal n xs == sliceHorizontal' n (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:711: "
{-# LINE 711 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 711 "src/Data/List/HT/Private.hs" #-}
           \(Positive n) xs -> sliceHorizontal n xs == sliceHorizontal'' n (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:720: "
{-# LINE 720 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 720 "src/Data/List/HT/Private.hs" #-}
    sliceVertical 6 ['a'..'z']
  )
  [ExpectedLine [LineChunk "[\"abcdef\",\"ghijkl\",\"mnopqr\",\"stuvwx\",\"yz\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:729: "
{-# LINE 729 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 729 "src/Data/List/HT/Private.hs" #-}
           \(NonNegative n) xs -> equating (take 100000) (sliceVertical n xs) (sliceVertical' n (xs::String))
  )
 DocTest.printPrefix "Data.List.HT.Private:742: "
{-# LINE 742 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 742 "src/Data/List/HT/Private.hs" #-}
      \(NonEmpty xs) ys -> replace xs xs ys == (ys::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:743: "
{-# LINE 743 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 743 "src/Data/List/HT/Private.hs" #-}
      \(NonEmpty xs) (NonEmpty ys) -> equating (take 1000) (replace xs ys (cycle xs)) (cycle (ys::String))
  )
 DocTest.printPrefix "Data.List.HT.Private:881: "
{-# LINE 881 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 881 "src/Data/List/HT/Private.hs" #-}
      \xs -> shearTranspose xs  ==  map reverse (shear (xs::[String]))
  )
 DocTest.printPrefix "Data.List.HT.Private:916: "
{-# LINE 916 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 916 "src/Data/List/HT/Private.hs" #-}
      \xs ys -> let f x y = (x::Char,y::Int) in concat (outerProduct f xs ys)  ==  liftM2 f xs ys
  )
 DocTest.printPrefix "Data.List.HT.Private:939: "
{-# LINE 939 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 939 "src/Data/List/HT/Private.hs" #-}
      \ys xs -> let ps = map (<=) ys in takeWhileMulti ps xs == takeWhileMulti' ps (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:993: "
{-# LINE 993 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 993 "src/Data/List/HT/Private.hs" #-}
    lengthAtLeast 0 ""
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:995: "
{-# LINE 995 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 995 "src/Data/List/HT/Private.hs" #-}
    lengthAtLeast 3 "ab"
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:997: "
{-# LINE 997 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 997 "src/Data/List/HT/Private.hs" #-}
    lengthAtLeast 3 "abc"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:999: "
{-# LINE 999 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 999 "src/Data/List/HT/Private.hs" #-}
    lengthAtLeast 3 $ repeat 'a'
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1001: "
{-# LINE 1001 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1001 "src/Data/List/HT/Private.hs" #-}
    lengthAtLeast 3 $ "abc" ++ undefined
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1004: "
{-# LINE 1004 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1004 "src/Data/List/HT/Private.hs" #-}
      \n xs -> lengthAtLeast n (xs::String)  ==  (length xs >= n)
  )
 DocTest.printPrefix "Data.List.HT.Private:1013: "
{-# LINE 1013 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1013 "src/Data/List/HT/Private.hs" #-}
    lengthAtMost 0 ""
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1015: "
{-# LINE 1015 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1015 "src/Data/List/HT/Private.hs" #-}
    lengthAtMost 3 "ab"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1017: "
{-# LINE 1017 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1017 "src/Data/List/HT/Private.hs" #-}
    lengthAtMost 3 "abc"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1019: "
{-# LINE 1019 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1019 "src/Data/List/HT/Private.hs" #-}
    lengthAtMost 3 "abcd"
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1021: "
{-# LINE 1021 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1021 "src/Data/List/HT/Private.hs" #-}
    lengthAtMost 3 $ repeat 'a'
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1023: "
{-# LINE 1023 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1023 "src/Data/List/HT/Private.hs" #-}
    lengthAtMost 3 $ "abcd" ++ undefined
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1026: "
{-# LINE 1026 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1026 "src/Data/List/HT/Private.hs" #-}
      \n xs -> lengthAtMost n (xs::String)  ==  (length xs <= n)
  )
 DocTest.printPrefix "Data.List.HT.Private:1035: "
{-# LINE 1035 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1035 "src/Data/List/HT/Private.hs" #-}
      \n xs -> lengthAtMost0 n (xs::String)  ==  (length xs <= n)
  )
 DocTest.printPrefix "Data.List.HT.Private:1082: "
{-# LINE 1082 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1082 "src/Data/List/HT/Private.hs" #-}
      \n (NonEmpty xs) -> rotate n xs == rotate' n (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:1089: "
{-# LINE 1089 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1089 "src/Data/List/HT/Private.hs" #-}
      \(NonNegative n) xs -> rotate n xs == rotate'' n (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:1099: "
{-# LINE 1099 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1099 "src/Data/List/HT/Private.hs" #-}
    mergeBy (<=) "agh" "begz"
  )
  [ExpectedLine [LineChunk "\"abegghz\""]]
 DocTest.printPrefix "Data.List.HT.Private:1107: "
{-# LINE 1107 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1107 "src/Data/List/HT/Private.hs" #-}
    allEqual "aab"
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1109: "
{-# LINE 1109 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1109 "src/Data/List/HT/Private.hs" #-}
    allEqual "aaa"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1111: "
{-# LINE 1111 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1111 "src/Data/List/HT/Private.hs" #-}
    allEqual "aa"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1113: "
{-# LINE 1113 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1113 "src/Data/List/HT/Private.hs" #-}
    allEqual "a"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1115: "
{-# LINE 1115 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1115 "src/Data/List/HT/Private.hs" #-}
    allEqual ""
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1122: "
{-# LINE 1122 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1122 "src/Data/List/HT/Private.hs" #-}
    isAscending "abc"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1124: "
{-# LINE 1124 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1124 "src/Data/List/HT/Private.hs" #-}
    isAscending "abb"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1126: "
{-# LINE 1126 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1126 "src/Data/List/HT/Private.hs" #-}
    isAscending "aba"
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1128: "
{-# LINE 1128 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1128 "src/Data/List/HT/Private.hs" #-}
    isAscending "cba"
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1130: "
{-# LINE 1130 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1130 "src/Data/List/HT/Private.hs" #-}
    isAscending "a"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1132: "
{-# LINE 1132 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1132 "src/Data/List/HT/Private.hs" #-}
    isAscending ""
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1145: "
{-# LINE 1145 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1145 "src/Data/List/HT/Private.hs" #-}
    mapAdjacent (<=) ""
  )
  [ExpectedLine [LineChunk "[]"]]
 DocTest.printPrefix "Data.List.HT.Private:1147: "
{-# LINE 1147 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1147 "src/Data/List/HT/Private.hs" #-}
    mapAdjacent (<=) "a"
  )
  [ExpectedLine [LineChunk "[]"]]
 DocTest.printPrefix "Data.List.HT.Private:1149: "
{-# LINE 1149 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1149 "src/Data/List/HT/Private.hs" #-}
    mapAdjacent (<=) "aba"
  )
  [ExpectedLine [LineChunk "[True,False]"]]
 DocTest.printPrefix "Data.List.HT.Private:1151: "
{-# LINE 1151 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1151 "src/Data/List/HT/Private.hs" #-}
    mapAdjacent (,) "abc"
  )
  [ExpectedLine [LineChunk "[('a','b'),('b','c')]"]]
 DocTest.printPrefix "Data.List.HT.Private:1154: "
{-# LINE 1154 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1154 "src/Data/List/HT/Private.hs" #-}
      \x xs -> mapAdjacent subtract (scanl (+) x xs) == (xs::[Integer])
  )
 DocTest.printPrefix "Data.List.HT.Private:1162: "
{-# LINE 1162 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1162 "src/Data/List/HT/Private.hs" #-}
      \xs -> mapAdjacent (,) xs == mapAdjacentPointfree (,) (xs::String)
  )
 DocTest.printPrefix "Data.List.HT.Private:1169: "
{-# LINE 1169 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1169 "src/Data/List/HT/Private.hs" #-}
    let f x y z = [x,y]++show(z::Int) in mapAdjacent1 f 'a' [('b',1), ('c',2), ('d',3)]
  )
  [ExpectedLine [LineChunk "[\"ab1\",\"bc2\",\"cd3\"]"]]
 DocTest.printPrefix "Data.List.HT.Private:1178: "
{-# LINE 1178 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1178 "src/Data/List/HT/Private.hs" #-}
    equalWith (<=) "ab" "bb"
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.List.HT.Private:1180: "
{-# LINE 1180 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1180 "src/Data/List/HT/Private.hs" #-}
    equalWith (<=) "aa" "bbb"
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1182: "
{-# LINE 1182 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1182 "src/Data/List/HT/Private.hs" #-}
    equalWith (==) "aa" "aaa"
  )
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Data.List.HT.Private:1185: "
{-# LINE 1185 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1185 "src/Data/List/HT/Private.hs" #-}
      \as bs -> let f a b = abs (a-b) <= (10::Int) in equalWith f as bs ==  equalWithRec f as bs
  )
 DocTest.printPrefix "Data.List.HT.Private:1186: "
{-# LINE 1186 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1186 "src/Data/List/HT/Private.hs" #-}
      \as bs -> let f a b = abs (a-b) <= (10::Int) in equalWith f as bs ==  equalWithLiftM f as bs
  )
 DocTest.printPrefix "Data.List.HT.Private:1222: "
{-# LINE 1222 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1222 "src/Data/List/HT/Private.hs" #-}
    range 0 :: [Integer]
  )
  [ExpectedLine [LineChunk "[]"]]
 DocTest.printPrefix "Data.List.HT.Private:1224: "
{-# LINE 1224 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1224 "src/Data/List/HT/Private.hs" #-}
    range 1 :: [Integer]
  )
  [ExpectedLine [LineChunk "[0]"]]
 DocTest.printPrefix "Data.List.HT.Private:1226: "
{-# LINE 1226 "src/Data/List/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 1226 "src/Data/List/HT/Private.hs" #-}
    range 8 :: [Integer]
  )
  [ExpectedLine [LineChunk "[0,1,2,3,4,5,6,7]"]]
 DocTest.printPrefix "Data.List.HT.Private:1229: "
{-# LINE 1229 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1229 "src/Data/List/HT/Private.hs" #-}
      \(NonNegative n) -> length (range n :: [Integer]) == n
  )
 DocTest.printPrefix "Data.List.HT.Private:1256: "
{-# LINE 1256 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1256 "src/Data/List/HT/Private.hs" #-}
      \x -> equating (take 1000) (List.iterate (x+) x) (iterateAssociative (+) (x::Integer))
  )
 DocTest.printPrefix "Data.List.HT.Private:1275: "
{-# LINE 1275 "src/Data/List/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 1275 "src/Data/List/HT/Private.hs" #-}
      \x -> equating (take 1000) (List.iterate (x+) x) (iterateLeaky (+) (x::Integer))
  )
