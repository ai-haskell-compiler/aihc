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
import Aihc.Parser.Syntax (Extension (MagicHash, NumericUnderscores))
import Data.Char (digitToInt, isDigit, isHexDigit, isOctDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pattern (:<))
import Data.Text qualified as T
import Numeric (readHex, readInt, readOct)
import Text.Read (readMaybe)

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
                        withOptionalMagicHashSuffix 2 env st raw (TkIntegerBase n raw) (TkIntegerBaseHash n)
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
                     in (Just frac, rest')
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
                    withOptionalMagicHashSuffix 2 env st raw (TkFloat value raw) (TkFloatHash value)
               in Just (mkToken st st' tokTxt tokKind, st')
    _ -> Nothing

lexFloat :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexFloat env st =
  let allowUnderscores = hasExt NumericUnderscores env
      (lhsRaw, rest) = takeDigitsWithUnderscores allowUnderscores isDigit (lexerInput st)
   in if T.null lhsRaw
        then Nothing
        else case rest of
          '.' :< dotRest@(d :< _)
            | isDigit d ->
                let (rhsRaw, rest') = takeDigitsWithUnderscores allowUnderscores isDigit dotRest
                    (expo, _) = takeExponent allowUnderscores rest'
                    raw = lhsRaw <> "." <> rhsRaw <> expo
                    normalized = T.filter (/= '_') raw
                    value = read (T.unpack normalized) :: Double
                    (tokTxt, tokKind, st') =
                      withOptionalMagicHashSuffix 2 env st raw (TkFloat value raw) (TkFloatHash value)
                 in Just (mkToken st st' tokTxt tokKind, st')
          _ ->
            case takeExponent allowUnderscores rest of
              (expo, _)
                | T.null expo -> Nothing
                | otherwise ->
                    let raw = lhsRaw <> expo
                        normalized = T.filter (/= '_') raw
                        value = read (T.unpack normalized) :: Double
                        (tokTxt, tokKind, st') =
                          withOptionalMagicHashSuffix 2 env st raw (TkFloat value raw) (TkFloatHash value)
                     in Just (mkToken st st' tokTxt tokKind, st')

lexInt :: LexerEnv -> LexerState -> Maybe (LexToken, LexerState)
lexInt env st =
  let allowUnderscores = hasExt NumericUnderscores env
      (digitsRaw, _) = takeDigitsWithUnderscores allowUnderscores isDigit (lexerInput st)
   in if T.null digitsRaw
        then Nothing
        else
          let digits = T.filter (/= '_') digitsRaw
              n = read (T.unpack digits) :: Integer
              (tokTxt, tokKind, st') =
                withOptionalMagicHashSuffix 2 env st digitsRaw (TkInteger n) (TkIntegerHash n)
           in Just (mkToken st st' tokTxt tokKind, st')

-- Scan ([_]*[digit])* and return a zero-copy split.
-- Uses T.span (which tracks the byte offset while scanning, no second pass)
-- rather than computing a character count and calling T.splitAt.
takeDigitsWithUnderscores :: Bool -> (Char -> Bool) -> Text -> (Text, Text)
takeDigitsWithUnderscores False isDigitChar = T.span isDigitChar
takeDigitsWithUnderscores True isDigitChar = \chars ->
  let (consumed, rest) = T.span (\c -> isDigitChar c || c == '_') chars
   in if T.null consumed || T.last consumed /= '_'
        then (consumed, rest)
        else -- Rare: trailing underscores (invalid syntax). Trim them off.
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
                          (digits, rest4) = takeDigitsWithUnderscores allowUnderscores isDigit rest3
                       in if T.null digits
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
              (digits, rest2) = takeDigitsWithUnderscores allowUnderscores isDigit rest1
           in if T.null digits then ("", chars) else let consumed = T.take (T.length chars - T.length rest2) chars in (consumed, rest2)
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

readHexLiteral :: Text -> Integer
readHexLiteral txt =
  case readHex (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> error ("invalid hex literal: " ++ T.unpack txt)

readOctLiteral :: Text -> Integer
readOctLiteral txt =
  case readOct (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> error ("invalid octal literal: " ++ T.unpack txt)

readBinLiteral :: Text -> Integer
readBinLiteral txt =
  case readInt 2 (`elem` ("01" :: String)) digitToInt (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> error ("invalid binary literal: " ++ T.unpack txt)

parseHexFloatLiteral :: String -> String -> String -> Double
parseHexFloatLiteral intDigits fracDigits expo =
  (parseHexDigits intDigits + parseHexFraction fracDigits) * (2 ^^ exponentValue expo)

parseHexDigits :: String -> Double
parseHexDigits = foldl (\acc d -> acc * 16 + fromIntegral (digitToInt d)) 0

parseHexFraction :: String -> Double
parseHexFraction ds =
  sum [fromIntegral (digitToInt d) / (16 ^^ i) | (d, i) <- zip ds [1 :: Int ..]]

exponentValue :: String -> Int
exponentValue expo =
  case expo of
    _ : '-' : ds | not (null ds) -> negate (fromMaybe 0 (readMaybe ds))
    _ : '+' : ds | not (null ds) -> fromMaybe 0 (readMaybe ds)
    _ : ds | not (null ds) -> fromMaybe 0 (readMaybe ds)
    _ -> 0
