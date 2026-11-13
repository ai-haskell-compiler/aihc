{-# LANGUAGE ScopedTypeVariables #-}

-- | The helpers of the 'Read' class and the instances of the types that the
-- primitive package and this module define.
--
-- The class itself lives in "GHC.Prim.Read", and 'Prelude' re-exports it.
-- The class cannot live in 'Prelude', because the instances need 'Prelude'
-- names such as 'Ratio'; a module that imported only 'Prelude' would then
-- see the class without its instances.
module GHC.Internal.Read
  ( Read (..),
    readListDefault,
    readListPrecDefault,
    choose,
    readNumber,
    readFieldHash,
    numberToRational,
    numberToRangedRational,
  )
where

import Data.Either (Either (..))
import Data.Proxy (Proxy (..))
import GHC.Err (undefined)
import GHC.Float (Double, Float, RealFloat (..))
import GHC.Int (Int, Int16, Int32, Int64, Int8)
import GHC.Internal.Base (Applicative (..), Functor (..), List (..), Maybe (..), Monad (..), String, otherwise)
import GHC.Internal.Classes (Ord (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import GHC.Internal.Integer (Integer)
import GHC.Num (Num (..))
import GHC.Prim.Read
  ( Digits,
    Lexeme (..),
    Number (..),
    Read (..),
    ReadPrec,
    ReadS,
    expectP,
    lexP,
    list,
    minPrec,
    numberDigitsValue,
    paren,
    parens,
    parseSignedInteger,
    pfail,
    prec,
    readPrec_to_S,
    readS_to_Prec,
    reset,
    step,
    stringEqual,
    (+++),
  )
import GHC.Real (Fractional (..), Integral (..), Ratio, Rational, (%), (^))
import GHC.Types (Bool (..), Char, Ordering (..))
import GHC.Word (Word, Word16, Word32, Word64, Word8)

readListDefault :: (Read a) => ReadS [a]
readListDefault = readPrec_to_S readListPrec minPrec

readListPrecDefault :: (Read a) => ReadPrec [a]
readListPrecDefault = list readPrec

choose :: [(String, ReadPrec a)] -> ReadPrec a
choose [] = pfail
choose ((name, parser) : alternatives) = chooseOne +++ choose alternatives
  where
    chooseOne = do
      token <- lexP
      case token of
        Ident actual -> ifStringsEqual actual
        Symbol actual -> ifStringsEqual actual
        _ -> pfail
    ifStringsEqual actual =
      case stringEqual actual name of
        True -> parser
        False -> pfail

readNumber :: (Num a) => (Lexeme -> ReadPrec a) -> ReadPrec a
readNumber convert =
  parens
    ( do
        token <- lexP
        case token of
          Symbol sign ->
            case stringEqual sign "-" of
              True -> do
                magnitude <- lexP
                value <- convert magnitude
                return (negate value)
              False -> convert token
          _ -> convert token
    )

readFieldHash :: String -> ReadPrec a -> ReadPrec a
readFieldHash name parser = do
  expectP (Ident name)
  expectP (Symbol "#")
  expectP (Punc "=")
  parser

readIntegralPrec :: (Num a) => ReadPrec a
readIntegralPrec =
  readS_to_Prec (\_ input -> convertIntegralResults (parseSignedInteger input))

convertIntegralResults :: (Num a) => [(Integer, String)] -> [(a, String)]
convertIntegralResults [] = []
convertIntegralResults ((value, rest) : results) =
  (fromInteger value, rest) : convertIntegralResults results

-- | The exact value of a literal, as in GHC. A decimal literal keeps its
-- digits until here, so that it only loses precision once, when it is
-- converted to the type that is read.
numberToRational :: Number -> Rational
numberToRational (MkNumber base iPart) = numberDigitsValue (toInteger base) iPart % 1
numberToRational (MkDecimal iPart mFPart mExp) =
  let i = numberDigitsValue 10 iPart
   in case (mFPart, mExp) of
        (Nothing, Nothing) -> i % 1
        (Nothing, Just exponentValue)
          | exponentValue >= 0 -> (i * (10 ^ exponentValue)) % 1
          | otherwise -> i % (10 ^ negate exponentValue)
        (Just fPart, Nothing) -> fracExp 0 i fPart
        (Just fPart, Just exponentValue) -> fracExp exponentValue i fPart

-- | Add the fraction digits to the mantissa and the exponent, as in GHC.
fracExp :: Integer -> Integer -> Digits -> Rational
fracExp exponentValue mantissa []
  | exponentValue < 0 = mantissa % (10 ^ negate exponentValue)
  | otherwise = fromInteger (mantissa * 10 ^ exponentValue)
fracExp exponentValue mantissa (digit : digits) =
  fracExp (exponentValue - 1) (mantissa * 10 + toInteger digit) digits

readFractionalPrec :: (RealFloat a) => ReadPrec a
readFractionalPrec = readNumber convertFractional

-- | The value of a literal, as in GHC. A literal with an exponent beyond the
-- range of the type gives infinity at once, and does not compute the power.
convertFractional :: forall a. (RealFloat a) => Lexeme -> ReadPrec a
convertFractional (Number token) =
  case numberToRangedRational (floatRange (undefined :: a)) token of
    Nothing -> return (1 / 0)
    Just rational -> return (fromRational rational)
convertFractional (Ident name) =
  case stringEqual name "NaN" of
    True -> return (0 / 0)
    False ->
      case stringEqual name "Infinity" of
        True -> return (1 / 0)
        False -> pfail
convertFractional _ = pfail

-- | The value of a literal, or 'Nothing' if the literal is too large for the
-- range of exponents, as in GHC. A literal that is too small gives zero.
numberToRangedRational :: (Int, Int) -> Number -> Maybe Rational
numberToRangedRational (neg, pos) n@(MkDecimal iPart mFPart (Just exponentValue)) =
  let mFirstDigit = case dropZeroDigits iPart of
        iPart'@(_ : _) -> Just (digitCount iPart')
        [] -> case mFPart of
          Nothing -> Nothing
          Just fPart ->
            case dropZeroDigits fPart of
              [] -> Nothing
              rest -> Just (negate (digitCount fPart - digitCount rest))
   in case mFirstDigit of
        Nothing -> Just 0
        Just firstDigit ->
          let firstDigit' = toInteger firstDigit + exponentValue
           in case firstDigit' > toInteger (pos + 3) of
                True -> Nothing
                False ->
                  case firstDigit' < toInteger (neg - 3) of
                    True -> Just 0
                    False -> Just (numberToRational n)
numberToRangedRational _ n = Just (numberToRational n)

-- | The digits after the leading zeros.
dropZeroDigits :: Digits -> Digits
dropZeroDigits (0 : rest) = dropZeroDigits rest
dropZeroDigits digits = digits

digitCount :: Digits -> Int
digitCount [] = 0
digitCount (_ : rest) = 1 + digitCount rest

instance Read Double where
  readPrec = readFractionalPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Float where
  readPrec = readFractionalPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Bool where
  readPrec = parens (choose [("False", return False), ("True", return True)])
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Ordering where
  readPrec = parens (choose [("LT", return LT), ("EQ", return EQ), ("GT", return GT)])
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Int where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Integer where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

-- The sized integral types read like 'Int' and 'Integer': the digits become
-- an 'Integer' that 'fromInteger' narrows to the type, which is what GHC's
-- instances do.
instance Read Int8 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Int16 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Int32 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Int64 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Word where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Word8 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Word16 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Word32 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Word64 where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Integral a, Read a) => Read (Ratio a) where
  readPrec =
    parens
      ( prec
          7
          ( do
              numeratorValue <- step readPrec
              expectP (Symbol "%")
              denominatorValue <- step readPrec
              return (numeratorValue % denominatorValue)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read () where
  readPrec = parens (paren (return ()))
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Char where
  readPrec =
    parens
      ( do
          token <- lexP
          case token of
            Char value -> return value
            _ -> pfail
      )
  readListPrec =
    parens
      ( ( do
            token <- lexP
            case token of
              String value -> return value
              _ -> pfail
        )
          +++ readListPrecDefault
      )
  readList = readListDefault

instance (Read a) => Read [a] where
  readPrec = readListPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a) => Read (Maybe a) where
  readPrec =
    parens
      ( choose [("Nothing", return Nothing)]
          +++ prec 10 (do expectP (Ident "Just"); value <- step readPrec; return (Just value))
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a) => Read (NonEmpty a) where
  readPrec =
    parens
      ( prec
          5
          ( do
              value <- step readPrec
              expectP (Symbol ":|")
              values <- step readPrec
              return (value :| values)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read (Proxy t) where
  readPrec = parens (do expectP (Ident "Proxy"); return Proxy)
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b) => Read (Either a b) where
  readPrec =
    parens
      ( prec 10 (do expectP (Ident "Left"); value <- step readPrec; return (Left value))
          +++ prec 10 (do expectP (Ident "Right"); value <- step readPrec; return (Right value))
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b) => Read (a, b) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              return (first, second)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b, Read c) => Read (a, b, c) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              expectP (Punc ",")
              third <- readPrec
              return (first, second, third)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              expectP (Punc ",")
              third <- readPrec
              expectP (Punc ",")
              fourth <- readPrec
              return (first, second, third, fourth)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              expectP (Punc ",")
              third <- readPrec
              expectP (Punc ",")
              fourth <- readPrec
              expectP (Punc ",")
              fifth <- readPrec
              return (first, second, third, fourth, fifth)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f) => Read (a, b, c, d, e, f) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              expectP (Punc ",")
              third <- readPrec
              expectP (Punc ",")
              fourth <- readPrec
              expectP (Punc ",")
              fifth <- readPrec
              expectP (Punc ",")
              sixth <- readPrec
              return (first, second, third, fourth, fifth, sixth)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g) => Read (a, b, c, d, e, f, g) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              expectP (Punc ",")
              third <- readPrec
              expectP (Punc ",")
              fourth <- readPrec
              expectP (Punc ",")
              fifth <- readPrec
              expectP (Punc ",")
              sixth <- readPrec
              expectP (Punc ",")
              seventh <- readPrec
              return (first, second, third, fourth, fifth, sixth, seventh)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault
