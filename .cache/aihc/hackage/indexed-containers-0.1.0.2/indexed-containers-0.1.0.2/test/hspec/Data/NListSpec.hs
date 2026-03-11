-- Generated code, do not modify by hand. Generate by running TestGen.hs.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}
module Data.NListSpec where

import Test.Hspec
import Prelude hiding (concat, drop, head, init, last, length, replicate, reverse, splitAt, tail, take, unzip, zip, zipWith)
import Data.NList

infix 4 ===
(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe

spec :: Spec
spec = do
  describe "Testing Data.NList" $ do
    it "" $ do
      length empty === 0
      length (singleton 'a' :: NList 1 Char) === 1
      'a' <:> singleton 'b' === mk2 'a' 'b'
      mk2 'a' 'b' <++> mk3 'c' 'd' 'e' === mk5 'a' 'b' 'c' 'd' 'e'
      length (mk3 'a' 'b' 'c') === 3
      head (mk3 'a' 'b' 'c') === 'a'
      headMay (empty :: NList 0 Int) === Nothing
      headMay (mk3 'a' 'b' 'c') === Just 'a'
      tail (singleton 'a') === empty
      tail (mk3 'a' 'b' 'c') === mk2 'b' 'c'
      tail' (empty :: NList 0 ()) === empty
      tail' (singleton 'a') === empty
      tail' (mk3 'a' 'b' 'c') === mk2 'b' 'c'
      tailMay (empty :: NList 0 ()) === Nothing
      tailMay (singleton 'a') === Just empty
      tailMay (mk3 'a' 'b' 'c') === Just (mk2 'b' 'c')
      last (mk3 'a' 'b' 'c') === 'c'
      lastMay (empty :: NList 0 Int) === Nothing
      lastMay (mk3 'a' 'b' 'c') === Just 'c'
      init (singleton 'a') === empty
      init (mk3 'a' 'b' 'c') === mk2 'a' 'b'
      init' (empty :: NList 0 ()) === empty
      init' (singleton 'a') === empty
      init' (mk3 'a' 'b' 'c') === mk2 'a' 'b'
      initMay (empty :: NList 0 ()) === Nothing
      initMay (singleton 'a') === Just empty
      initMay (mk3 'a' 'b' 'c') === Just (mk2 'a' 'b')
      uncons (singleton 'a') === ('a', empty)
      uncons (mk3 'a' 'b' 'c') === ('a', mk2 'b' 'c')
      take @0 (mk3 'a' 'b' 'c') === empty
      take @2 (mk3 'a' 'b' 'c') === mk2 'a' 'b'
      take @3 (mk3 'a' 'b' 'c') === mk3 'a' 'b' 'c'
      drop @0 (mk3 'a' 'b' 'c') === mk3 'a' 'b' 'c'
      drop @2 (mk3 'a' 'b' 'c') === singleton 'c'
      drop @3 (mk3 'a' 'b' 'c') === empty
      splitAt @0 (mk3 'a' 'b' 'c') === (empty, mk3 'a' 'b' 'c')
      splitAt @2 (mk3 'a' 'b' 'c') === (mk2 'a' 'b', singleton 'c')
      splitAt @3 (mk3 'a' 'b' 'c') === (mk3 'a' 'b' 'c', empty)
      reverse (mk3 'a' 'b' 'c') === mk3 'c' 'b' 'a'
      intersperse (',') empty === empty
      intersperse (',') (singleton 'a') === singleton 'a'
      intersperse (',') (mk3 'a' 'b' 'c') === mk5 'a' ',' 'b' ',' 'c'
      transpose (mk2 (mk3 1 2 3) (mk3 4 5 6)) === mk3 (mk2 1 4) (mk2 2 5) (mk2 3 6)
      kth @0 (mk4 'a' 'b' 'c' 'd') === 'a'
      kth @3 (mk4 'a' 'b' 'c' 'd') === 'd'
      sort (mk6 1 4 2 8 5 7) === mk6 1 2 4 5 7 8
      sortOn negate (mk6 1 4 2 8 5 7) === mk6 8 7 5 4 2 1
      sortBy (\x y -> compare (-x) (-y)) (mk6 1 4 2 8 5 7) === mk6 8 7 5 4 2 1
      toList (mk3 'a' 'b' 'c') === "abc"
      zip (mk2 1 2) (mk2 'a' 'b') === mk2 (1, 'a') (2, 'b')
      zipWith (+) (mk2 1 2) (mk2 3 4) === mk2 4 6
      unzip (mk2 (1, 'a') (2, 'b')) === ((mk2 1 2), (mk2 'a' 'b'))
      concat (mk2 (mk3 1 2 3) (mk3 4 5 6)) === mk6 1 2 3 4 5 6
      replicate @3 'a' === mk3 'a' 'a' 'a'
      toList (mk1 'a') === "a"
      toList (mk2 'a' 'b') === "ab"
      toList (mk3 'a' 'b' 'c') === "abc"
      toList (mk4 'a' 'b' 'c' 'd') === "abcd"
      toList (mk5 'a' 'b' 'c' 'd' 'e') === "abcde"
      toList (mk6 'a' 'b' 'c' 'd' 'e' 'f') === "abcdef"
      toList (mk7 'a' 'b' 'c' 'd' 'e' 'f' 'g') === "abcdefg"
      toList (mk8 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h') === "abcdefgh"
      toList (mk9 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i') === "abcdefghi"
      toList (mk10 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j') === "abcdefghij"
      fromTuple ('a', 'b') === mk2 'a' 'b'
      fromTuple ('a', 'b', 'c') === mk3 'a' 'b' 'c'
      fromTuple ('a', 'b', 'c', 'd') === mk4 'a' 'b' 'c' 'd'
      fromTuple ('a', 'b', 'c', 'd', 'e') === mk5 'a' 'b' 'c' 'd' 'e'
      fromTuple ('a', 'b', 'c', 'd', 'e', 'f') === mk6 'a' 'b' 'c' 'd' 'e' 'f'
      fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g') === mk7 'a' 'b' 'c' 'd' 'e' 'f' 'g'
      fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h') === mk8 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h'
      fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i') === mk9 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i'
      fromTuple ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j') === mk10 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j'
      toTuple (mk2 'a' 'b') === ('a', 'b')
      toTuple (mk3 'a' 'b' 'c') === ('a', 'b', 'c')
      toTuple (mk4 'a' 'b' 'c' 'd') === ('a', 'b', 'c', 'd')
      toTuple (mk5 'a' 'b' 'c' 'd' 'e') === ('a', 'b', 'c', 'd', 'e')
      toTuple (mk6 'a' 'b' 'c' 'd' 'e' 'f') === ('a', 'b', 'c', 'd', 'e', 'f')
      toTuple (mk7 'a' 'b' 'c' 'd' 'e' 'f' 'g') === ('a', 'b', 'c', 'd', 'e', 'f', 'g')
      toTuple (mk8 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h') === ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
      toTuple (mk9 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i') === ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
      toTuple (mk10 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j') === ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
