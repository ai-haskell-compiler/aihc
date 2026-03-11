{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
Module      :  Aftovolio.General.Datatype3
Copyright   :  (c) OleksandrZhabenko 2023-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com
-}
module Aftovolio.General.Datatype3 (
    Read0,
    isA,
    isB,
    isC,
    readU2,
    readSimple3,
    basicSplit,
    line2Strings,
    read3,
    readEq4G,
    readEq4,
    zippedDouble2Word8,
) where

import Data.Char (isDigit, isLetter, isSpace)
import qualified Data.Foldable as F (foldr)
import Data.List (find, groupBy, maximumBy, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Sequence as S
import Data.Tuple (fst, snd)
import GHC.Base
import GHC.Float (int2Double)
import GHC.List
import GHC.Num ((*), (+), (-))
import GHC.Real (floor, fromIntegral, (/))
import GHC.Word
import ListQuantizer (round2GL)
import Text.Read (readMaybe)
import Text.Show (Show (..))

-- | Is a way to read duration of the additional added time period into the line.
readU2 :: String -> Double
readU2 (y : ys) = fromMaybe 1.0 (readMaybe (y : '.' : (if null ys then "0" else ys)) :: Maybe Double)
readU2 _ = 1.0
{-# INLINE readU2 #-}

-- | Splits a 'String' into list of 'String' so that they can be read by other functions here into respective datatypes.
splitL0 :: String -> [String]
splitL0 =
    groupBy
        ( \x y ->
            (isDigit x && isDigit y)
                || ( x
                        /= '_'
                        && x
                        /= '='
                        && not (isDigit x)
                        && y
                        /= '_'
                        && y
                        /= '='
                        && not (isDigit y)
                   )
                || ((x == '=' || x == '_') && isDigit y)
        )
{-# INLINE splitL0 #-}

data Read0 = A {-# UNPACK #-} !Double | B {-# UNPACK #-} !Double | C String
    deriving (Eq, Show)

-- | Converts a specially formatted 'String' into a 'Read0' value.
reRead3 :: String -> Read0
reRead3 xs =
    case uncons xs of
        Just ('=', ts) -> A (readU2 ts)
        Just ('_', ts) -> B (readU2 ts)
        _ -> C xs

isA :: Read0 -> Bool
isA (A _) = True
isA _ = False

isB :: Read0 -> Bool
isB (B _) = True
isB _ = False

isC :: Read0 -> Bool
isC (C _) = True
isC _ = False

filterReads :: [Read0] -> S.Seq Read0
filterReads xs@(B y : A t : us) = B y S.<| filterReads (dropWhile isA us)
filterReads xs@(A y : A t : us) = A y S.<| filterReads (dropWhile isA us)
filterReads xs@(t : ts) = t S.<| filterReads ts
filterReads _ = S.empty

-- | A preparatory function for the further ones here.
basicSplit :: String -> S.Seq Read0
basicSplit = filterReads . map reRead3 . splitL0
{-# INLINE basicSplit #-}

readSimple3 ::
    -- | A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
    (String -> Bool) ->
    Double ->
    (String -> [Word8]) ->
    -- | Is should be obtained using 'basicSplit' function here.
    S.Seq Read0 ->
    [Word8]
readSimple3 p temp fConvA rs@(C xs S.:<| A x S.:<| ts) -- This branch is fixed in the version 0.6.0.0 because earlier it has an issue.
    | null qs = readSimple3 p temp fConvA ts
    | null q1 = floor xl1 : readSimple3 p xl1 fConvA ts
    | otherwise = q1 `mappend` (floor xl1 : readSimple3 p xl1 fConvA ts)
  where
    qs
        | p xs = fConvA xs
        | otherwise = []
    (q1, q2s) = splitAtEnd 1 qs
    ql1 = head q2s
    xl1 = min (x * word8ToDouble ql1) 255.0
readSimple3 p temp fConvA rs@(C xs S.:<| ys@(B x S.:<| ts)) = qs `mappend` qqs `mappend` readSimple3 p ql fConvA ws
  where
    (!ks, ws) = S.spanl isB ys
    !qs
        | p xs = fConvA xs
        | otherwise = []
    !ql
        | null qs = 0.0
        | otherwise = word8ToDouble . last $ qs
    qqs = F.foldr (\(B k) js -> double2Word8 (k * ql) : js) [] ks
readSimple3 p temp fConvA rs@(B x S.:<| ts) = qqs `mappend` readSimple3 p temp fConvA ws
  where
    (ks, ws) = S.spanl isB rs
    qqs = F.foldr (\(B k) js -> double2Word8 (k * temp) : js) [] ks
readSimple3 p temp fConvA (C xs S.:<| _) = qs
  where
    qs
        | p xs = fConvA xs
        | otherwise = []
readSimple3 _ _ _ _ = []

-- | Is intended to 'floor' the values greater than 255.0 to 255::'Word8' and to be used for non-negative 'Double'.
double2Word8 :: Double -> Word8
double2Word8 = floor . min 255.0
{-# INLINE double2Word8 #-}

-- | Is done using intermediate 'Int' representation.
word8ToDouble :: Word8 -> Double
word8ToDouble = int2Double . fromIntegral
{-# INLINE word8ToDouble #-}

read3 ::
    -- | A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
    (String -> Bool) ->
    Double ->
    (String -> [Word8]) ->
    String ->
    [Word8]
read3 p temp fConvA = filter (/= 0) . readSimple3 p temp fConvA . basicSplit
{-# INLINE read3 #-}

splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd n = (\(x, y, _) -> (y, x)) . foldr f v
  where
    v = ([], [], 0)
    f x (zs, ts, k)
        | k < n = (x : zs, [], k + 1)
        | otherwise = (zs, x : ts, k + 1)

{- | Is a specialized version of 'Data.InsertLeft.dropFromEndG' function variant from the @subG@ package. Is taken from there to
reduce the dependencies. Is not intended to be exported at all.
-}
dropFromEnd :: Int -> [a] -> [a]
dropFromEnd n = (\(xs, _) -> xs) . foldr f v
  where
    v = ([], 0)
    f x (zs, k)
        | k < n = ([], k + 1)
        | otherwise = (x : zs, k)

line2Strings ::
    -- | A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
    (String -> Bool) ->
    (String -> [String]) ->
    -- | Is should be obtained using 'basicSplit' function here.
    S.Seq Read0 ->
    [String]
line2Strings p gConvC xs@(C ts S.:<| tt@(A x) S.:<| ys)
    | null qs = ks `mappend` line2Strings p gConvC ys
    | otherwise =
        ks
            `mappend` ((ql `mappend` ('=' : showRead0AsInsert tt)) : line2Strings p gConvC ys)
  where
    (ks, qs)
        | p ts = splitAtEnd 1 . gConvC $ ts
        | otherwise = ([], [])
    ql = head qs
line2Strings p gConvC xs@(C ys S.:<| ts) = gConvC ys `mappend` line2Strings p gConvC ts
line2Strings p gConvC xs@(y@(B x) S.:<| ts) = showRead0AsInsert y : line2Strings p gConvC ts
line2Strings _ _ _ = []

-- | Is intended to be used in the "music" mode for AFTOVolio.
readEq4G ::
    -- | A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
    (String -> Bool) ->
    (String -> [Word8]) ->
    (String -> [String]) ->
    -- | Is should be obtained using 'basicSplit' function here.
    S.Seq Read0 ->
    [(String, Word8)]
readEq4G p fConvA gConvC xs = zip ks rs
  where
    ks = line2Strings p gConvC xs
    rs = filter (/= 0) . readSimple3 p 1.0 fConvA $ xs
{-# INLINE readEq4G #-}

readEq4 ::
    (String -> [Word8]) ->
    (String -> [String]) ->
    -- | Is should be obtained using 'basicSplit' function here.
    S.Seq Read0 ->
    [(String, Word8)]
readEq4 = readEq4G (not . null . filter (not . isSpace))
{-# INLINE readEq4 #-}

showRead0AsInsert :: Read0 -> String
showRead0AsInsert d@(A t) = '=' : (filter (/= '.') . show $ t)
showRead0AsInsert d@(B t) = '_' : (filter (/= '.') . show $ t)
showRead0AsInsert d@(C ts) = ts
{-# INLINE showRead0AsInsert #-}

-- | Is intended to be used to transform the earlier data for AFTOVolio representations durations from 'Double' to 'Word8' values. It was used during the transition from the ukrainian-phonetics-basic-array-0.7.1.1 to ukrainian-phonetics-basic-array-0.10.0.0.
zippedDouble2Word8 xs = map (\(t, u) -> (t, fromMaybe 15 . hh $ u)) xs
  where
    !h = snd . minimumBy (comparing snd) $ xs
    !lt = snd . maximumBy (comparing snd) $ xs
    !del = (lt - h) / 14.0
    !ys = take 15 . iterate (+ del) $ h
    !zs = zip [1 .. 15] ys
    gg !u = fromMaybe lt . round2GL True (\_ _ -> EQ) ys $ u
    hh !u = fmap fst . find ((== gg u) . snd) $ zs
