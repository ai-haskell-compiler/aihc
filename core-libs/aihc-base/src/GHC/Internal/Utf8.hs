-- | The UTF-8 codec that string marshalling and the program arguments
-- share. Bytes are plain 'Int's so that both users can pass what their
-- byte access returns; a malformed sequence decodes to U+FFFD.
module GHC.Internal.Utf8
  ( encodeUtf8,
    decodeUtf8,
  )
where

import Data.Foldable (foldr)
import GHC.Base (ord)
import GHC.Char (chr)
import Prelude

encodeUtf8 :: String -> [Int]
encodeUtf8 = foldr (\character rest -> encodeCodePoint (ord character) ++ rest) []

encodeCodePoint :: Int -> [Int]
encodeCodePoint codePoint =
  case codePoint <= 127 of
    True -> [codePoint]
    False ->
      case codePoint <= 2047 of
        True -> [192 + quot codePoint 64, 128 + rem codePoint 64]
        False ->
          case codePoint <= 65535 of
            True ->
              [ 224 + quot codePoint 4096,
                128 + rem (quot codePoint 64) 64,
                128 + rem codePoint 64
              ]
            False ->
              [ 240 + quot codePoint 262144,
                128 + rem (quot codePoint 4096) 64,
                128 + rem (quot codePoint 64) 64,
                128 + rem codePoint 64
              ]

decodeUtf8 :: [Int] -> String
decodeUtf8 [] = []
decodeUtf8 (first : rest) =
  case first < 128 of
    True -> chr first : decodeUtf8 rest
    False -> decodeMultibyte first rest

decodeMultibyte :: Int -> [Int] -> String
decodeMultibyte first rest =
  case first >= 194 && first <= 223 of
    True ->
      case rest of
        second : remaining ->
          case continuation second of
            True -> chr ((first - 192) * 64 + second - 128) : decodeUtf8 remaining
            False -> replacement : decodeUtf8 rest
        [] -> [replacement]
    False ->
      case first >= 224 && first <= 239 of
        True -> decodeThree first rest
        False ->
          case first >= 240 && first <= 244 of
            True -> decodeFour first rest
            False -> replacement : decodeUtf8 rest

decodeThree :: Int -> [Int] -> String
decodeThree first rest =
  case rest of
    second : (third : remaining) ->
      let codePoint = (first - 224) * 4096 + (second - 128) * 64 + third - 128
       in case continuation second && continuation third && codePoint >= 2048 && notSurrogate codePoint of
            True -> chr codePoint : decodeUtf8 remaining
            False -> replacement : decodeUtf8 rest
    _ -> replacement : decodeUtf8 rest

decodeFour :: Int -> [Int] -> String
decodeFour first rest =
  case rest of
    second : (third : (fourth : remaining)) ->
      let codePoint = (first - 240) * 262144 + (second - 128) * 4096 + (third - 128) * 64 + fourth - 128
       in case continuation second && continuation third && continuation fourth && codePoint >= 65536 && codePoint <= 1114111 of
            True -> chr codePoint : decodeUtf8 remaining
            False -> replacement : decodeUtf8 rest
    _ -> replacement : decodeUtf8 rest

continuation :: Int -> Bool
continuation byte = byte >= 128 && byte <= 191

notSurrogate :: Int -> Bool
notSurrogate codePoint = codePoint < 55296 || codePoint > 57343

replacement :: Char
replacement = chr 65533
