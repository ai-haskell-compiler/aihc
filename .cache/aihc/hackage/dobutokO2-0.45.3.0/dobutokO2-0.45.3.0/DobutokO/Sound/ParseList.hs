-- |
-- Module      :  DobutokO.Sound.ParseList
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.ParseList where 

import Data.Char (isSpace)
import qualified Data.Vector as V
import Text.Read (lex,readMaybe)
import Data.Maybe (isNothing,fromMaybe)

parseTup :: String -> [String]
parseTup xs = map (dropWhile isSpace . fst) (takeWhile (/= ("","")) . iterate (head . lex . snd) $ head (lex xs))

parseTupV :: String -> V.Vector String
parseTupV = V.fromList . parseTup

containsExt :: [String] -> Bool
containsExt = elem ".." 

containsExtV :: V.Vector String -> Bool
containsExtV = V.elem ".."

-- | Predicate to check whether a 'V.Vector' does not contain round parentheses or dash (a minus sign) as its elements. Is used internally in the
-- 'parseStoLInts' function to avoid lists with negative elements.
canBePreParseV :: V.Vector String -> Bool
canBePreParseV v = not (V.elem "(" v || V.elem "-" v || V.elem ")" v)

-- | Notification. Uses an 'Int' limitation to avoid infinite lists. All arguments must be not negative.
parseV :: Int -> V.Vector String -> Maybe [Int]
parseV n v
 | compare n 0 /= LT && V.findIndices (== "..") v == V.singleton 2 && V.length v == 4 = 
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 3 == "]"
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int in
        case ins1 of
          Just ins -> if compare n ins /= GT then Just [ins] else Just [ins..n]
          Nothing  -> Nothing
      else Nothing
 | compare n 0 /= LT && V.findIndices (== "..") v == V.singleton 2 && V.length v == 5 =
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 4 == "]"
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int 
               ins2 = readMaybe (V.unsafeIndex v 3)::Maybe Int in
        case (ins1,ins2) of
          (Just ins01,Just ins02) -> if ins02 >= ins01 then Just [ins01..ins02] else Nothing
          _                       -> Nothing
      else Nothing
 | compare n 0 /= LT && V.findIndices (== "..") v == V.singleton 4 && V.length v == 6 =
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 2 == "," && V.unsafeIndex v 5 == "]" 
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int 
               ins2 = readMaybe (V.unsafeIndex v 3)::Maybe Int in
        case (ins1,ins2) of
          (Just ins01,Just ins02) ->
            case compare ins02 ins01 of
             GT -> if compare n ins02 /= LT then Just [ins01,ins02..n] else Just [ins01,ins02]
             EQ -> Just [ins01]
             _  -> Just [ins01,ins02..0]
          _                       -> Nothing
      else Nothing
 | compare n 0 /= LT && V.findIndices (== "..") v == V.singleton 4 && V.length v == 7 =
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 2 == "," && V.unsafeIndex v 6 == "]" 
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int 
               ins2 = readMaybe (V.unsafeIndex v 3)::Maybe Int
               ins3 = readMaybe (V.unsafeIndex v 5)::Maybe Int in
        case (ins1,ins2,ins3) of
          (Just ins01,Just ins02,Just ins03) -> if null [ins01,ins02..ins03] then Nothing else Just [ins01,ins02..ins03]
          _                       -> Nothing
      else Nothing
 | compare n 0 /= LT && V.unsafeIndex v 0 == "[" && V.unsafeIndex v (V.length v - 1) == "]" && V.length v `rem` 2 == 1 &&
    (V.toList . V.findIndices (== ",") $ v) == [2,4..(V.length v - 2)] =
      let insV1 = V.imap (\i _ -> readMaybe (V.unsafeIndex v (2 * i + 1))::Maybe Int) (V.unsafeSlice 0 (V.length v `quot` 2) v) in
       if V.any isNothing insV1
         then Nothing
         else Just (V.toList . V.mapMaybe id $ insV1)
 | otherwise = Nothing

-- | From the 0.19.0.0 version. Can be used to parse also into infinite lists. 
parseVInf :: V.Vector String -> Maybe [Int]
parseVInf v
 | V.findIndices (== "..") v == V.singleton 2 && V.length v == 4 =
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 3 == "]"
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int in
        case ins1 of
          Just ins -> Just [ins..]
          Nothing  -> Nothing
      else Nothing
 | V.findIndices (== "..") v == V.singleton 2 && V.length v == 5 =
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 4 == "]"
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int
               ins2 = readMaybe (V.unsafeIndex v 3)::Maybe Int in
        case (ins1,ins2) of
          (Just ins01,Just ins02) -> if ins02 >= ins01 then Just [ins01..ins02] else Nothing
          _                       -> Nothing
      else Nothing
 | V.findIndices (== "..") v == V.singleton 4 && V.length v == 6 =
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 2 == "," && V.unsafeIndex v 5 == "]"
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int
               ins2 = readMaybe (V.unsafeIndex v 3)::Maybe Int in
        case (ins1,ins2) of
          (Just ins01,Just ins02) -> Just [ins01,ins02..]
          _                       -> Nothing
      else Nothing
 | V.findIndices (== "..") v == V.singleton 4 && V.length v == 7 =
    if V.unsafeIndex v 0 == "[" && V.unsafeIndex v 2 == "," && V.unsafeIndex v 6 == "]"
      then let ins1 = readMaybe (V.unsafeIndex v 1)::Maybe Int
               ins2 = readMaybe (V.unsafeIndex v 3)::Maybe Int
               ins3 = readMaybe (V.unsafeIndex v 5)::Maybe Int in
        case (ins1,ins2,ins3) of
          (Just ins01,Just ins02,Just ins03) -> if null [ins01,ins02..ins03] then Nothing else Just [ins01,ins02..ins03]
          _                       -> Nothing
      else Nothing
 | V.unsafeIndex v 0 == "[" && V.unsafeIndex v (V.length v - 1) == "]" && V.length v `rem` 2 == 1 &&
    (V.toList . V.findIndices (== ",") $ v) == [2,4..(V.length v - 2)] =
      let insV1 = V.imap (\i _ -> readMaybe (V.unsafeIndex v (2 * i + 1))::Maybe Int) (V.unsafeSlice 0 (V.length v `quot` 2) v) in
       if V.any isNothing insV1
         then Nothing
         else Just (V.toList . V.mapMaybe id $ insV1)
 | otherwise = Nothing 

-- | Parses a 'String' being a list of Ints written with Haskell rules, e. g. \"[1..]\", \"[2,4..45]\", \"[3,5,6,7,8,3]\" etc. into a list of 'Int'.
-- If it is not possible or list is empty, returns []. Preceding whitespaces are ignored. An 'Int' argument is used as a delimiter to avoid infinite lists.
parseStoLInts :: Int -> String -> [Int]
parseStoLInts n xs
  | canBePreParseV . parseTupV . dropWhile isSpace $ xs = fromMaybe [] (parseV n (parseTupV . dropWhile isSpace $ xs))
  | otherwise = []
