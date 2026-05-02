{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex.Numbers
  ( lexFloat,
    lexHexFloat,
    lexInt,
    lexIntBase,
    withOptionalMagicHashSuffix,
  )
where

import Aihc.Parser.Lex.Types
import Aihc.Parser.Syntax (Extension (ExtendedLiterals, MagicHash, NumericUnderscores), FloatType (..), NumericType (..))
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text, pattern (:<))
import Data.Text qualified as T
import Numeric (readHex, readInt, readOct)
import Text.Read (readMaybe)

lexIntBase :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexIntBase env st =
  case lexerInput st of
    '0' :< (base :< rest)
      | base `elem` ("xXoObB" :: String) ->
          let allowUnderscores = hasExt NumericUnderscores env
              isDigitChar
                | base `elem` ("xX" :: String) = isHexDigit
                | base `elem` ("oO" :: String) = isOctDigit
                | otherwise = (`elem` ("01" :: String))
              (digitsRaw, _) = takeDigitsWithUnderscores allowUnderscores isDigitChar rest
           in if T.null digitsRaw
                then Nothing
                else
                  let raw = "0" <> T.singleton base <> digitsRaw
                      n
                        | base `elem` ("xX" :: String) = readHexLiteral raw
                        | base `elem` ("oO" :: String) = readOctLiteral raw
                        | otherwise = readBinLiteral raw
                      (tokTxt, tokKind, st') =
                        lexIntSuffix env st raw n
                   in Just (mkToken st st' tokTxt tokKind, st')
    _ -> Nothing

lexHexFloat :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexHexFloat env st =
  case lexerInput st of
    '0' :< (x :< rest1)
      | x `elem` ("xX" :: String),
        let (intDigits, rest2) = T.span isHexDigit rest1,
        not (T.null intDigits) -> do
          let (mFracDigits, rest3) =
                case rest2 of
                  '.' :< more ->
                    let (frac, rest') = T.span isHexDigit more
                     in if T.null frac then (Nothing, rest2) else (Just frac, rest')
                  _ -> (Nothing, rest2)
          expo <- takeHexExponent rest3
          if T.length expo <= 1
            then Nothing
            else
              let dotAndFrac =
                    case mFracDigits of
                      Just ds -> "." <> ds
                      Nothing -> ""
                  fracDigits = fromMaybe "" mFracDigits
                  raw = "0" <> T.singleton x <> intDigits <> dotAndFrac <> expo
                  value = parseHexFloatLiteral (T.unpack intDigits) (T.unpack fracDigits) (T.unpack expo)
                  (tokTxt, tokKind, st') =
                    lexFloatSuffix env st raw value
               in Just (mkToken st st' tokTxt tokKind, st')
    _ -> Nothing

lexFloat :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexFloat env st =
  let allowUnderscores = hasExt NumericUnderscores env
      input = lexerInput st
      startsWithUnderscore =
        case input of
          '_' :< _ -> True
          _ -> False
      (lhsRaw, rest) = takeDigitsWithUnderscores allowUnderscores isDigit input
   in if T.null lhsRaw || startsWithUnderscore
        then Nothing
        else case rest of
          '.' :< dotRest@(d :< _)
            | isDigit d ->
                let (rhsRaw, rest') = takeDigitsWithUnderscores allowUnderscores isDigit dotRest
                    (expo, _) = takeExponent allowUnderscores rest'
                    raw = lhsRaw <> "." <> rhsRaw <> expo
                    normalized = T.filter (/= '_') raw
                    value = parseDecimalFloatLiteral normalized
                    (tokTxt, tokKind, st') =
                      lexFloatSuffix env st raw value
                 in Just (mkToken st st' tokTxt tokKind, st')
          _ ->
            case takeExponent allowUnderscores rest of
              (expo, _)
                | T.null expo -> Nothing
                | otherwise ->
                    let raw = lhsRaw <> expo
                        normalized = T.filter (/= '_') raw
                        value = parseDecimalFloatLiteral normalized
                        (tokTxt, tokKind, st') =
                          lexFloatSuffix env st raw value
                     in Just (mkToken st st' tokTxt tokKind, st')

lexInt :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexInt env st =
  let allowUnderscores = hasExt NumericUnderscores env
      input = lexerInput st
      startsWithUnderscore =
        case input of
          '_' :< _ -> True
          _ -> False
      (digitsRaw, _) = takeDigitsWithUnderscores allowUnderscores isDigit input
   in if T.null digitsRaw || startsWithUnderscore
        then Nothing
        else
          let digits = T.filter (/= '_') digitsRaw
              n = read (T.unpack digits) :: Integer
              (tokTxt, tokKind, st') =
                lexIntSuffix env st digitsRaw n
           in Just (mkToken st st' tokTxt tokKind, st')

-- | Parse optional MagicHash suffix (# or ##) and return the hash text and new state.
magicHashSuffix :: LexerEnv -> LexerState -> Maybe (Text, LexerState)
magicHashSuffix env st =
  case lexerInput st of
    '#' :< rest
      | hasExt MagicHash env ->
          case rest of
            '#' :< _ -> Just ("##", advanceN 2 st)
            _ -> Just ("#", advanceN 1 st)
    _ -> Nothing

-- | Parse optional MagicHash and ExtendedLiterals suffix for integers.
-- Handles: (nothing), #, ##, #Int8, #Int16, #Int32, #Int64, #Int, #Word8, #Word16, #Word32, #Word64, #Word
lexIntSuffix :: LexerEnv -> LexerState -> Text -> Integer -> (Text, LexTokenKind, LexerState)
lexIntSuffix env st raw n =
  let st' = advanceChars raw st
      input = lexerInput st'
      withMagicHash =
        case magicHashSuffix env st' of
          Just ("##", st'') -> (raw <> "##", TkInteger n TWordHash, st'')
          Just ("#", st'') -> (raw <> "#", TkInteger n TIntHash, st'')
          _ -> (raw, TkInteger n TInteger, st')
   in case input of
        '#' :< rest
          | hasExt ExtendedLiterals env ->
              case parseExtendedIntSuffix rest of
                Just (typeName, suffixLen) ->
                  let fullRaw = raw <> T.take (1 + suffixLen) input
                      st'' = advanceN (1 + suffixLen) st'
                   in (fullRaw, TkInteger n typeName, st'')
                Nothing -> withMagicHash
          | otherwise -> withMagicHash
        _ -> (raw, TkInteger n TInteger, st')

-- | Parse optional MagicHash and ExtendedLiterals suffix for floats.
-- Handles: (nothing), #, ##
lexFloatSuffix :: LexerEnv -> LexerState -> Text -> Rational -> (Text, LexTokenKind, LexerState)
lexFloatSuffix env st raw value =
  let st' = advanceChars raw st
   in case magicHashSuffix env st' of
        Just ("##", st'') -> (raw <> "##", TkFloat value TDoubleHash, st'')
        Just ("#", st'') -> (raw <> "#", TkFloat value TFloatHash, st'')
        _ -> (raw, TkFloat value TFractional, st')

-- | Parse an ExtendedLiterals type suffix after the initial '#'.
-- Returns (NumericType, length of type name without the '#').
-- e.g. "Word8" -> (TWord8Hash, 5), "Int" -> (TIntHash, 3)
parseExtendedIntSuffix :: Text -> Maybe (NumericType, Int)
parseExtendedIntSuffix input =
  case matchType input of
    Just ("Int8", len) -> Just (TInt8Hash, len)
    Just ("Int16", len) -> Just (TInt16Hash, len)
    Just ("Int32", len) -> Just (TInt32Hash, len)
    Just ("Int64", len) -> Just (TInt64Hash, len)
    Just ("Int", len) -> Just (TIntHash, len)
    Just ("Word8", len) -> Just (TWord8Hash, len)
    Just ("Word16", len) -> Just (TWord16Hash, len)
    Just ("Word32", len) -> Just (TWord32Hash, len)
    Just ("Word64", len) -> Just (TWord64Hash, len)
    Just ("Word", len) -> Just (TWordHash, len)
    _ -> Nothing

-- | Try to match one of the known type names at the start of the text.
-- Returns (matched name, length) if the match is followed by a non-identifier char
-- or end of input.
matchType :: Text -> Maybe (Text, Int)
matchType input =
  let candidates = ["Int8", "Int16", "Int32", "Int64", "Int", "Word8", "Word16", "Word32", "Word64", "Word"]
   in firstMatch candidates input

firstMatch :: [Text] -> Text -> Maybe (Text, Int)
firstMatch [] _ = Nothing
firstMatch (name : rest) input =
  let len = T.length name
   in case T.stripPrefix name input of
        Just remaining
          | T.null remaining || not (isIdentChar (T.head remaining)) ->
              Just (name, len)
        _ -> firstMatch rest input

isIdentChar :: Char -> Bool
isIdentChar c = isDigit c || isAsciiUpper c || c == '_' || isAsciiLower c

-- Scan ([_]*[digit])* and return a zero-copy split.
takeDigitsWithUnderscores :: Bool -> (Char -> Bool) -> Text -> (Text, Text)
takeDigitsWithUnderscores False isDigitChar = T.span isDigitChar
takeDigitsWithUnderscores True isDigitChar = \chars ->
  let (consumed, rest) = T.span (\c -> isDigitChar c || c == '_') chars
   in if T.null consumed || T.last consumed /= '_'
        then (consumed, rest)
        else
          let trimmed = T.dropWhileEnd (== '_') consumed
           in (trimmed, T.drop (T.length trimmed) chars)

takeExponent :: Bool -> Text -> (Text, Text)
takeExponent allowUnderscores chars =
  case chars of
    '_' :< _
      | allowUnderscores ->
          let (_allUnderscores, rest') = T.span (== '_') chars
           in case rest' of
                marker :< rest2
                  | marker `elem` ("eE" :: String) ->
                      let (_signPart, rest3) =
                            case rest2 of
                              sign :< more | sign `elem` ("+-" :: String) -> (T.singleton sign, more)
                              _ -> ("", rest2)
                          digitsStartWithUnderscore =
                            case rest3 of
                              '_' :< _ -> True
                              _ -> False
                          (digits, rest4) = takeDigitsWithUnderscores allowUnderscores isDigit rest3
                       in if T.null digits || digitsStartWithUnderscore
                            then ("", chars)
                            else
                              let consumed = T.take (T.length chars - T.length rest4) chars
                               in (consumed, rest4)
                _ -> ("", chars)
    marker :< rest
      | marker `elem` ("eE" :: String) ->
          let (_signPart, rest1) =
                case rest of
                  sign :< more | sign `elem` ("+-" :: String) -> (T.singleton sign, more)
                  _ -> ("", rest)
              digitsStartWithUnderscore =
                case rest1 of
                  '_' :< _ -> True
                  _ -> False
              (digits, rest2) = takeDigitsWithUnderscores allowUnderscores isDigit rest1
           in if T.null digits || digitsStartWithUnderscore then ("", chars) else let consumed = T.take (T.length chars - T.length rest2) chars in (consumed, rest2)
    _ -> ("", chars)

takeHexExponent :: Text -> Maybe Text
takeHexExponent chars =
  case chars of
    marker :< rest
      | marker `elem` ("pP" :: String) ->
          let (_signPart, rest1) =
                case rest of
                  sign :< more | sign `elem` ("+-" :: String) -> (T.singleton sign, more)
                  _ -> ("", rest)
              (digits, _) = T.span isDigit rest1
           in if T.null digits then Nothing else Just (T.take (T.length chars - T.length rest1 + T.length digits) chars)
    _ -> Nothing

readBaseLiteral :: String -> (String -> [(Integer, String)]) -> Text -> Integer
readBaseLiteral label parser txt =
  case parser (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> error ("invalid " ++ label ++ " literal: " ++ T.unpack txt)

readHexLiteral :: Text -> Integer
readHexLiteral = readBaseLiteral "hex" readHex

readOctLiteral :: Text -> Integer
readOctLiteral = readBaseLiteral "octal" readOct

readBinLiteral :: Text -> Integer
readBinLiteral = readBaseLiteral "binary" (readInt 2 (`elem` ("01" :: String)) digitToInt)

parseDecimalFloatLiteral :: Text -> Rational
parseDecimalFloatLiteral txt =
  case parseSignedDecimalRational txt of
    Just value -> value
    Nothing -> error ("invalid decimal float literal: " ++ T.unpack txt)

parseSignedDecimalRational :: Text -> Maybe Rational
parseSignedDecimalRational txt = do
  let (sign, unsigned) =
        case T.uncons txt of
          Just ('-', rest) -> (-1, rest)
          Just ('+', rest) -> (1, rest)
          _ -> (1, txt)
      (mantissaTxt, exponentTxt0) = T.break (`elem` ['e', 'E']) unsigned
      exponentTxt = T.drop 1 exponentTxt0
  mantissa <- parseDecimalMantissa mantissaTxt
  exponentN <-
    if T.null exponentTxt0
      then Just 0
      else parseSignedInteger exponentTxt
  pure (applyDecimalExponent (fromInteger sign * mantissa) exponentN)

parseDecimalMantissa :: Text -> Maybe Rational
parseDecimalMantissa txt = do
  let (whole, frac0) = T.break (== '.') txt
      frac = T.drop 1 frac0
  if T.null whole && T.null frac
    then Nothing
    else do
      wholeDigits <- parseDecimalDigits whole
      fracDigits <- parseDecimalDigits frac
      let fracScale = 10 ^ T.length frac
      pure ((wholeDigits * fracScale + fracDigits) % fracScale)

parseDecimalDigits :: Text -> Maybe Integer
parseDecimalDigits digits
  | T.null digits = Just 0
  | T.all isDigit digits = readMaybe (T.unpack digits)
  | otherwise = Nothing

parseSignedInteger :: Text -> Maybe Integer
parseSignedInteger digits =
  case T.uncons digits of
    Just ('+', rest) -> parseDecimalDigits rest
    Just ('-', rest) -> negate <$> parseDecimalDigits rest
    _ -> parseDecimalDigits digits

applyDecimalExponent :: Rational -> Integer -> Rational
applyDecimalExponent value exponentN
  | exponentN >= 0 = value * fromInteger (10 ^ exponentN)
  | otherwise = value / fromInteger (10 ^ negate exponentN)

parseHexFloatLiteral :: String -> String -> String -> Rational
parseHexFloatLiteral intDigits fracDigits expo =
  (parseHexDigits intDigits + parseHexFraction fracDigits) * (2 ^^ exponentValue expo)

parseHexDigits :: String -> Rational
parseHexDigits = foldl (\acc d -> acc * 16 + fromIntegral (digitToInt d)) 0

parseHexFraction :: String -> Rational
parseHexFraction ds =
  sum [fromIntegral (digitToInt d) % (16 ^ i) | (d, i) <- zip ds [1 :: Integer ..]]

exponentValue :: String -> Int
exponentValue expo =
  case expo of
    _ : '-' : ds | not (null ds) -> negate (fromMaybe 0 (readMaybe ds))
    _ : '+' : ds | not (null ds) -> fromMaybe 0 (readMaybe ds)
    _ : ds | not (null ds) -> fromMaybe 0 (readMaybe ds)
    _ -> 0

withOptionalMagicHashSuffix ::
  Int ->
  LexerEnv ->
  LexerState ->
  Text ->
  LexTokenKind ->
  (Text -> LexTokenKind) ->
  (Text, LexTokenKind, LexerState)
withOptionalMagicHashSuffix maxHashes env st raw plainKind hashKind =
  let st' = advanceChars raw st
      hashCount =
        if hasExt MagicHash env
          then min maxHashes (T.length (T.takeWhile (== '#') (lexerInput st')))
          else 0
   in case hashCount of
        0 -> (raw, plainKind, st')
        _ ->
          let hashes = T.replicate hashCount "#"
              rawHash = raw <> hashes
           in (rawHash, hashKind rawHash, advanceChars hashes st')
