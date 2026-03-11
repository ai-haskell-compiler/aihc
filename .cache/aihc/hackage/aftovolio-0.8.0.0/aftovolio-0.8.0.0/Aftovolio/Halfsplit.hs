{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK -show-extensions #-}

{- |
Module      :  Aftovolio.Halfsplit
Copyright   :  (c) OleksandrZhabenko 2023, 2025
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com
-}
module Aftovolio.Halfsplit where

import Data.Char (isDigit)
import Data.List hiding (foldr)
import Data.Lists.FLines (newLineEnding)
import Data.Tuple (fst)
import GHC.Base
import GHC.Enum (fromEnum)
import GHC.Int (Int8)
import GHC.Num (abs, (+), (-))
import GHC.Real (quot, quotRem, odd)
import System.IO (getLine, putStr, putStrLn)
import Text.Read (readMaybe)
import Data.Maybe (Maybe,fromMaybe)
import Text.Show (Show (..))
--import Debug.Trace

-- | Converts the data that is an instance of 'Show' typeclass to be printed in two-column way.
halfsplit ::
    (Show a, Eq b) =>
    (a -> b) ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    Int8 ->
    [a] ->
    String
halfsplit g filtering = halfsplit1G g filtering ""
{-# INLINE halfsplit #-}

{- | Converts the data that is an instance of 'Show' typeclass to be printed in two-column way with
customizable ending of each line. Filters out all groups of \'={digits}\' from the lines.
-}
halfsplit1G ::
    (Show a, Eq b) =>
    (a -> b) ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    -- | Additional 'String' added to every line before the newline character.
    String ->
    Int8 ->
    [a] ->
    String
halfsplit1G g filtering appendstr m xs
    | null xs = []
    | otherwise =
        let (n, rr2) = quotRem (fromEnum m) (if m < 0 then -10 else 10)
            rr3 = abs rr2
            noChange = odd rr3
            l0 = length . showFiltering filtering . head $ xs -- For empty lines
            r 
              | rr3 > 1 && rr3 < 6 = (noChangeReverse noChange ws, noChangeReverse (not noChange) w2s)
              | otherwise = splitTwoBasic filtering (rr3 == 1) l0 xs
                 where (ws,w2s) = splitTwo filtering (rr3 > 3) l0 g xs
         in ((\(rs, qs) -> mergePartsLine n (appendstr `mappend` newLineEnding) rs qs) r)
                    `mappend` appendstr

showFiltering 
  :: (Show a) 
  => Bool
  -> a
  -> String
showFiltering filtering = (if filtering then removeChangesOfDurations else id) . show
{-# INLINE showFiltering #-}  

splitTwoBasic 
  :: (Show a) => Bool
  -> Bool
  -> Int 
  -> [a]
  -> ([String],[String])
splitTwoBasic filtering first l0 xs = ((if rrr == 0 then id else (replicate l0 ' ' :)) . map (showFiltering filtering) . noChangeReverse first $ ys, map (showFiltering filtering) . noChangeReverse (not first) $ ts)
  where
    (ys, ts) = splitAt l xs -- Is used for basic splitting
    (l, rrr) = length xs `quotRem` 2 -- For basic splitting
{-# INLINABLE splitTwoBasic #-}

noChangeF 
  :: Bool
  -> ([a] -> [a])
  -> [a]
  -> [a]
noChangeF nochange f xs = if nochange then xs else f xs
{-# INLINE noChangeF #-}
    
noChangeReverse 
  :: Bool 
  -> [a]
  -> [a]
noChangeReverse nochange = noChangeF nochange (reverse)
{-# INLINE noChangeReverse #-}

splitTwo 
  :: (Show a, Eq b) 
  -- | Whether to filter out all groups of \'={digits}\' from the lines.
  => Bool
  -> Bool
  -> Int
  -> (a -> b) 
  -> [a] 
  -> ([String],[String])
splitTwo filtering emptyline l0 g xs = (y10s, t10s) 
  where
    rss = (if emptyline then intersperse [""] else id) . map (map (showFiltering filtering)) . groupBy (\x y -> g x == g y) $ xs
    (l2, r20) = (sum . map length $ rss) `quotRem` 2
    (y100s, t10s) = splitAt l2 . concat $ rss
    y10s 
        | r20 == 0 = y100s
        | otherwise = "": y100s
{-# INLINABLE splitTwo #-}

-- | A generalized version of 'halfsplit3G' with the possibility to prepend and append strings to it. These 'String's are not filtered out for the groups of \'={digits}\' from the prepending and appending 'String's.
halfsplit2G ::
    (Show a, Eq b) =>
    (a -> b) ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    -- | Additional 'String' added to every line before the newline character.
    String ->
    -- | A 'String' that is prepended to the 'halfsplit1G' result.
    String ->
    -- | A 'String' that is appended to the 'halfsplit1G' result.
    String ->
    Int8 ->
    [a] ->
    String
halfsplit2G g filtering appendstr prestr poststr m xs = prestr `mappend` halfsplit1G g filtering appendstr m xs `mappend` poststr
{-# INLINABLE halfsplit2G #-}

-- | Filters out all groups of \"={digits}\" and \"_{digits}\" from the 'String'
removeChangesOfDurations :: String -> String
removeChangesOfDurations ('=' : xs) = removeChangesOfDurations (dropWhile isDigit xs)
removeChangesOfDurations ('_' : xs) = removeChangesOfDurations (dropWhile isDigit xs) -- added since aftovolio-0.8.0.0
removeChangesOfDurations (x : xs) = x : removeChangesOfDurations xs
removeChangesOfDurations _ = []

mergePartsLine :: Int -> String -> [String] -> [String] -> String
mergePartsLine n newlined xs ys =
    intercalate newlined
        .  showWithSpacesBefore (replicate n (if n < 0 then '\t' else ' ')) xs $ ys

splitGroups :: Int -> [[a]] -> ([[a]], [[a]], Int)
splitGroups l tss = foldr h ([], [], 0) tss
  where
    h js (rss, mss, k)
        | k < l = (rss, js : mss, k + length js)
        | otherwise = (js : rss, mss, k + length js)

showWithSpaces :: (Show a) => Int -> a -> String
showWithSpaces n x
    | l < n = xs `mappend` replicate (n - l) ' '
    | otherwise = xs
  where
    xs = show x
    l = length xs

showWithSpacesBefore 
  :: String 
  -> [String] 
  -> [String] 
  -> [String]
showWithSpacesBefore inserts (ts:xs:xss) (us:ys:yss) 
  | null xs = ws : (replicate (length ts) ' ' `mappend` inserts `mappend` ys) : showWithSpacesBefore inserts xss yss
  | otherwise = ws : showWithSpacesBefore inserts (xs:xss) (ys:yss)
    where ws = ts `mappend` inserts `mappend` us
showWithSpacesBefore inserts [xs] (ys:_) = [(if null xs then replicate (length ys) ' ' else xs) `mappend` inserts `mappend` ys]
showWithSpacesBefore inserts _ _ = []

print23 :: Bool -> String -> String -> Int -> [String] -> IO ()
print23 filtering prestr poststr n xss = do
    putStrLn prestr
    let linez = zip xss [1 ..]
    if n >= 2 && n <= l - 1
        then do
            let linez3 =
                    (\(x : y : t : xs) -> x : (' ' : y) : (' ' : ' ' : t) : xs)
                        . map fst
                        . filter (\(ts, m) -> m `elem` [n - 1 .. n + 1]) $
                        linez
            mapM putSLn linez3 >> putStrLn poststr
        else
            ( case n of
                1 -> putStr " " >> mapM putSLn (take 2 xss)
                m ->
                    if m == l
                        then mapM putSLn ((\(x : y : xs) -> x : (' ' : y) : xs) . drop (l - 2) $ xss)
                        else pure []
            )
                >> putStrLn poststr
  where
    l = length xss
    putSLn = putStrLn . (if filtering then removeChangesOfDurations else id)

readNums :: [String] -> [Int]
readNums = concatMap readNs
  where readNs :: String -> [Int]
        readNs xs 
            | any (== '-') xs = 
                 let (ys, ts) = break (== '-') xs 
                     us = dropWhile (not . isDigit) ts
                 in [(fromMaybe 1 (readMaybe ys:: Maybe Int))..(fromMaybe 1 (readMaybe us:: Maybe Int))]
            | otherwise = [fromMaybe 1 (readMaybe xs:: Maybe Int)]
{-# INLINE readNums #-}
