-- |
-- Module: Options.Util
-- License: MIT
module Options.Util where

import Data.Char (isAlphaNum, isLetter, isUpper)
import qualified Data.Set as Set

stringToGhc704 :: String -> String
stringToGhc704 = id

validFieldName :: String -> Bool
validFieldName = valid
  where
    valid s = case s of
      [] -> False
      c : cs -> validFirst c && all validGeneral cs
    validFirst c = c == '_' || (isLetter c && not (isUpper c))
    validGeneral c = isAlphaNum c || c == '_' || c == '\''

validShortFlag :: Char -> Bool
validShortFlag = isAlphaNum

validLongFlag :: String -> Bool
validLongFlag = valid
  where
    valid s = case s of
      [] -> False
      c : cs -> validFirst c && all validGeneral cs
    validFirst = isAlphaNum
    validGeneral c = isAlphaNum c || c == '-' || c == '_'

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = Set.size (Set.fromList xs) /= length xs

mapEither :: (a -> Either err b) -> [a] -> Either err [b]
mapEither fn = loop []
  where
    loop acc [] = Right (reverse acc)
    loop acc (a : as) = case fn a of
      Left err -> Left err
      Right b -> loop (b : acc) as
