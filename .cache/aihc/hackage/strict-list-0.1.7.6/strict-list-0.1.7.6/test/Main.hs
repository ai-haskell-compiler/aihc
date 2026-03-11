{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.List as Lazy
import qualified Data.Maybe as Maybe
import GHC.Exts as Exports (IsList (..))
import StrictList
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude hiding (List, break, choose, drop, dropWhile, filter, head, init, last, reverse, span, tail, take, takeWhile, toList)

main :: IO ()
main =
  defaultMain
    $ testGroup ""
    $ [ testProperty "toList"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            toList strict === lazy,
        testProperty "fromList"
          $ forAll lazyListGen
          $ \lazy ->
            toList (fromList @(List Word8) lazy) === lazy,
        testProperty "reverse"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            toList (reverse strict) === Lazy.reverse lazy,
        testProperty "take"
          $ forAll ((,) <$> arbitrary <*> strictAndLazyListGen)
          $ \(amount, (strict, lazy)) ->
            toList (take amount strict) === Lazy.take amount lazy,
        testProperty "drop"
          $ forAll ((,) <$> arbitrary <*> strictAndLazyListGen)
          $ \(amount, (strict, lazy)) ->
            toList (drop amount strict) === Lazy.drop amount lazy,
        testProperty "filter"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            toList (filter predicate strict) === Lazy.filter predicate lazy,
        testProperty "filterReversed"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            toList (filterReversed predicate strict) === Lazy.reverse (Lazy.filter predicate lazy),
        testProperty "takeWhile"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            toList (takeWhile predicate strict) === Lazy.takeWhile predicate lazy,
        testProperty "takeWhileReversed"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            toList (takeWhileReversed predicate strict) === Lazy.reverse (Lazy.takeWhile predicate lazy),
        testProperty "dropWhile"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            toList (dropWhile predicate strict) === Lazy.dropWhile predicate lazy,
        testProperty "span"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            bimap toList toList (span predicate strict) === Lazy.span predicate lazy,
        testProperty "break"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            bimap toList toList (break predicate strict) === Lazy.break predicate lazy,
        testProperty "takeWhileFromEnding"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            toList (takeWhileFromEnding predicate strict) === Lazy.takeWhile predicate (Lazy.reverse lazy),
        testProperty "dropWhileFromEnding"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            toList (dropWhileFromEnding predicate strict) === Lazy.dropWhile predicate (Lazy.reverse lazy),
        testProperty "spanFromEnding"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(predicate, (strict, lazy)) ->
            bimap toList toList (spanFromEnding predicate strict) === Lazy.span predicate (Lazy.reverse lazy),
        testProperty "head"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            head strict === listToMaybe lazy,
        testProperty "last"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            last strict === listToMaybe (Lazy.reverse lazy),
        testProperty "tail"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            toList (tail strict) === Lazy.drop 1 lazy,
        testProperty "init"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            toList (init strict) === Lazy.take (Lazy.length lazy - 1) lazy,
        testProperty "initReversed"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            toList (initReversed strict) === Lazy.reverse (Lazy.take (Lazy.length lazy - 1) lazy),
        testProperty "fromListReversed"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            toList (fromListReversed lazy) === Lazy.reverse lazy,
        testProperty "prependReversed"
          $ forAll ((,) <$> strictAndLazyListGen <*> strictAndLazyListGen)
          $ \((strict1, lazy1), (strict2, lazy2)) ->
            toList (prependReversed strict1 strict2) === Lazy.reverse lazy1 <> lazy2,
        testProperty "mapReversed"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(mapper, (strict, lazy)) ->
            toList (mapReversed mapper strict) === Lazy.reverse (fmap mapper lazy),
        testProperty "apReversed"
          $ forAll ((,) <$> strictAndLazyListGen <*> strictAndLazyListGen)
          $ \((strict1, lazy1), (strict2, lazy2)) ->
            toList (apReversed (fmap (,) strict1) strict2) === Lazy.reverse ((,) <$> lazy1 <*> lazy2),
        testProperty "apZippingReversed"
          $ forAll ((,) <$> strictAndLazyListGen <*> strictAndLazyListGen)
          $ \((strict1, lazy1), (strict2, lazy2)) ->
            toList (apZippingReversed (fmap (,) strict1) strict2) === Lazy.reverse (Lazy.zip lazy1 lazy2),
        testProperty "explodeReversed"
          $ forAll ((,) <$> strictAndLazyKleisliGen <*> strictAndLazyListGen)
          $ \((strictK, lazyK), (strict, lazy)) ->
            toList (explodeReversed strictK strict) === Lazy.reverse (lazy >>= lazyK),
        testProperty "fmap"
          $ forAll ((,) <$> predicateGen <*> strictAndLazyListGen)
          $ \(mapper, (strict, lazy)) ->
            toList (fmap mapper strict) === fmap mapper lazy,
        testProperty "<*>"
          $ forAll ((,) <$> strictAndLazyListGen <*> strictAndLazyListGen)
          $ \((strict1, lazy1), (strict2, lazy2)) ->
            toList ((,) <$> strict1 <*> strict2) === ((,) <$> lazy1 <*> lazy2),
        testProperty "<>"
          $ forAll ((,) <$> strictAndLazyListGen <*> strictAndLazyListGen)
          $ \((strict1, lazy1), (strict2, lazy2)) ->
            toList (strict1 <> strict2) === (lazy1 <> lazy2),
        testProperty "<|>"
          $ forAll ((,) <$> strictAndLazyListGen <*> strictAndLazyListGen)
          $ \((strict1, lazy1), (strict2, lazy2)) ->
            toList (strict1 <|> strict2) === (lazy1 <|> lazy2),
        testProperty ">>="
          $ forAll ((,) <$> strictAndLazyKleisliGen <*> strictAndLazyListGen)
          $ \((strictK, lazyK), (strict, lazy)) ->
            toList (strict >>= strictK) === (lazy >>= lazyK),
        testProperty "foldl'"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            foldl' (flip (:)) [] strict === foldl' (flip (:)) [] lazy,
        testProperty "foldr"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            foldr (:) [] strict === foldr (:) [] lazy,
        testProperty "traverse"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            let fn x = if mod x 2 == 0 then Right x else Left x
             in fmap toList (traverse fn strict) === traverse fn lazy,
        testProperty "toListReversed"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            lazy === toListReversed (reverse strict),
        testProperty "mapMaybeReversed"
          $ forAll strictAndLazyListGen
          $ \(strict, lazy) ->
            let mapper x = if mod x 2 == 0 then Just x else Nothing
             in Maybe.mapMaybe mapper lazy
                  === toListReversed (mapMaybeReversed mapper strict),
        testProperty "catMaybesReversed" $ \(lazy :: [Maybe Word8]) ->
          Maybe.catMaybes lazy
            === toListReversed (catMaybesReversed (fromList lazy))
      ]
  where
    lazyListGen = arbitrary @[Word8]
    strictAndLazyListGen = do
      lazy <- lazyListGen
      return (foldr Cons Nil lazy, lazy)
    predicateGen = do
      op <- elements [(>), (>=), (==), (<=), (<)]
      x <- arbitrary @Word8
      return (op x)
    strictAndLazyKleisliGen = do
      lazy <- sizedListGen 10
      let lazyK x = fmap (+ x) lazy
          strictK = foldr Cons Nil . lazyK
       in return (strictK, lazyK)
    sizedListGen maxSize = do
      length <- choose (0, maxSize)
      replicateM length (arbitrary @Word8)

-- Workarounds to satisfy QuickCheck's requirements,
-- when we need to generate a predicate.

instance Show (Word8 -> Bool) where
  show _ = "(Word8 -> Bool) function"

instance Show (Word8 -> List Word8) where
  show _ = "(Word8 -> List Word8) function"

instance Show (Word8 -> [Word8]) where
  show _ = "(Word8 -> [Word8]) function"
