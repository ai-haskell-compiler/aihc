{-# HLINT ignore "Use camelCase" #-}

module Text.ParserCombinators.ReadP
  ( ReadP,
    ReadS,
    readP_to_S,
    readS_to_P,
  )
where

import Prelude
  ( Applicative (..),
    Functor (..),
    Monad (..),
    ReadS,
    String,
    (++),
    (.),
  )

newtype ReadP a = ReadP (ReadS a)

instance Functor ReadP where
  fmap function (ReadP parser) =
    ReadP (readPMapResults function . parser)

instance Applicative ReadP where
  pure value = ReadP (\input -> [(value, input)])

  ReadP functionParser <*> ReadP valueParser =
    ReadP (readPApplyResults valueParser . functionParser)

instance Monad ReadP where
  ReadP parser >>= next =
    ReadP (readPBindResults next . parser)

  ReadP first >> ReadP second =
    ReadP (readPThenResults second . first)

  return = pure

readPMapResults :: (a -> b) -> [(a, String)] -> [(b, String)]
readPMapResults _ [] = []
readPMapResults function ((value, rest) : results) =
  (function value, rest) : readPMapResults function results

readPApplyResults :: ReadS a -> [(a -> b, String)] -> [(b, String)]
readPApplyResults _ [] = []
readPApplyResults parser ((function, rest) : results) =
  readPMapResults function (parser rest) ++ readPApplyResults parser results

readPBindResults :: (a -> ReadP b) -> [(a, String)] -> [(b, String)]
readPBindResults _ [] = []
readPBindResults next ((value, rest) : results) =
  readP_to_S (next value) rest ++ readPBindResults next results

readPThenResults :: ReadS b -> [(a, String)] -> [(b, String)]
readPThenResults _ [] = []
readPThenResults parser ((_, rest) : results) =
  parser rest ++ readPThenResults parser results

readP_to_S :: ReadP a -> ReadS a
readP_to_S (ReadP parser) = parser

readS_to_P :: ReadS a -> ReadP a
readS_to_P = ReadP
