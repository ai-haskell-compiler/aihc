module Iri.CodePointPredicates.Core where

import qualified Data.Vector.Unboxed as A
import Iri.Prelude hiding (Predicate, inRange, (&&&), (|||))

type Predicate =
  Int -> Bool

{-# NOINLINE cached #-}
cached :: Predicate -> Predicate
cached predicate =
  case A.generate 256 predicate of
    vector -> A.unsafeIndex vector

oneOfChars :: [Char] -> Predicate
oneOfChars string i =
  elem i (fmap ord string)

infixr 2 |||

(|||) :: Predicate -> Predicate -> Predicate
(|||) left right i =
  left i || right i

infixr 3 &&&

(&&&) :: Predicate -> Predicate -> Predicate
(&&&) left right i =
  left i && right i

inRange :: Int -> Int -> Predicate
inRange min max i =
  i >= min && i <= max

inCharRange :: Char -> Char -> Predicate
inCharRange min max =
  inRange (ord min) (ord max)

-- | 7-bit
septimal :: Predicate
septimal i =
  i < 0x80

nonSeptimal :: Predicate
nonSeptimal i =
  i >= 0x80

control :: Predicate
control i =
  i
    <= 0x1F
    || i
    == 0x7F

asciiAlphanumeric :: Predicate
asciiAlphanumeric =
  inCharRange 'a' 'z'
    ||| inCharRange 'A' 'Z'
    ||| inCharRange '0' '9'
