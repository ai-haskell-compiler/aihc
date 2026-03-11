-- |
-- Module      :  DobutokO.Sound.Functional.Split
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Functional.Split (
  -- * Splitting and concatenating OvertonesO
  splitO
  , splitO2
  , overConcat
  -- ** Generalization of the previous ones splitting functions
  , splitHelp1
  , splitHelp2
  , splitOG1
  , splitOG2
  , splitOG12
  , splitOG12S
  , splitOG22
  , splitOG22S
) where

import CaseBi.Arr (getBFstLSorted')
import Data.Char (isAsciiLower)
import Data.List (sortBy)
import qualified Data.Vector as V
import DobutokO.Sound.Functional.Basics


-- | Splits (with addition of the new overtones) a given 'OvertonesO' into a number @n@ (specified by the first 'Int' argument) of 'OvertonesO' 
-- (represented finally as a 'V.Vector' of them respectively) so that all except the first @n@ greatest by the absolute value of the amplitude 
-- tuples of Floats are considered overtones for the greatest by the absolute value one in the given 'OvertonesO' and all the next @n - 1@ 
-- are treated as the greatest by the absolute value and each of them produces the similar by the @f :: Float -> OvertonesO@ function overtones.
-- 
-- It is expected to obtain by such a conversion a splitted one sound into several simultaneous similar ones with different heights. 
-- To provide a rich result, the given first argument must be strictly less than the length of the given 'OvertonesO' minus one.
splitO :: Int -> OvertonesO -> V.Vector OvertonesO
splitO n v0 
 | compare (V.length v0) (n + 1) == GT = 
    let v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        (x0, y0) = V.unsafeIndex v1 0
        v2 = V.unsafeSlice 1 (n - 1) v1
        v31 = V.map (\t -> (fst t) / x0) v2
        v32 = V.map (\t -> (snd t) / y0) v2
        v3 = V.zip v31 v32
        f1Tup (t1, w2) = V.imap (\ i _ -> (fst (V.unsafeIndex v3 i) * t1, snd (V.unsafeIndex v3 i) * w2)) v3
          in V.map f1Tup (V.unsafeSlice 0 n v1)
 | otherwise = V.singleton v0

-- | Splits (with addition of the new overtones) a given 'OvertonesO' into a number of 'OvertonesO' (represented finally as a 'V.Vector' of them repsectively) 
-- so that it intermediately uses a special function before applying the \"similarization\" splitting function. Is a generalization of the 'splitO', 
-- which can be considered a 'splitO2' with a first command line argument equals to 'id'.
-- 
-- It is expected to obtain by such a conversion a splitted one sound into several simultaneous similar (less or more, depending on @h :: OvertonesO -> OvertonesO@) 
-- ones with different heights. To provide a rich result, the given first argument must be strictly less than the length of the given 'OvertonesO' minus one.
splitO2 :: (OvertonesO -> OvertonesO) -> Int -> OvertonesO -> V.Vector OvertonesO
splitO2 h n v0
 | compare (V.length v0) (n + 1) == GT = 
    let v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        (x0, y0) = V.unsafeIndex v1 0
        v2 = V.unsafeSlice 1 (n - 1) v1
        v31 = V.map (\t -> (fst t) / x0) v2
        v32 = V.map (\t -> (snd t) / y0) v2
        v3 = V.zip v31 v32
        f1Tup (t1, w2) = V.imap (\ i _ -> (fst (V.unsafeIndex v3 i) * t1, snd (V.unsafeIndex v3 i) * w2)) v3
          in V.map f1Tup (h . V.unsafeSlice 0 n $ v1)
 | otherwise = V.singleton v0

-- | Generalized variant of the 'splitO' with the different splitting variants depending on the first two ASCII lower case letters in the 'String' argument.
splitOG1 :: String -> Int -> OvertonesO -> V.Vector OvertonesO
splitOG1 xs n v0 
 | compare (V.length v0) (n + 1) == GT = 
    let c1s = take 2 . filter isAsciiLower $ xs
        v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        (x0, y0) = V.unsafeIndex v1 0 in
          case c1s of
            "ab" -> let (k1,k2,k3,k4) = (n - 1,V.length v0 - n,n - 1,V.length v0 - n) in splitHelp1 k1 k2 k3 k4 v1 (x0,y0) 
            "ac" -> let (k1,k2,k3,k4) = (1,n - 1,n - 1,V.length v0 - n) in splitHelp1 k1 k2 k3 k4 v1 (x0,y0) 
            "ad" -> let (k1,k2,k3,k4) = (n - 1,V.length v0 - n,0,n) in splitHelp1 k1 k2 k3 k4 v1 (x0,y0) 
            _    -> let (k1,k2,k3,k4) = (1,n - 1,0,n) in splitHelp1 k1 k2 k3 k4 v1 (x0,y0) 
 | otherwise = V.singleton v0

-- | Auxiliary function that is used inside 'splitOG1'.
splitHelp1 :: Int -> Int -> Int -> Int -> OvertonesO -> (Float,Float) -> V.Vector OvertonesO
splitHelp1 x1 x2 x3 x4 v00 (y5,y6) = 
  let v2 = V.unsafeSlice x1 x2 v00
      v31 = V.map (\t -> (fst t) / y5) v2
      v32 = V.map (\t -> (snd t) / y6) v2
      v3 = V.zip v31 v32
      f1Tup (t1, w2) = V.imap (\ i _ -> (fst (V.unsafeIndex v3 i) * t1, snd (V.unsafeIndex v3 i) * w2)) v3
        in V.map f1Tup (V.unsafeSlice x3 x4 v00)

-- | Auxiliary function that is used inside 'splitOG2'.
splitHelp2 :: (OvertonesO -> OvertonesO) -> Int -> Int -> Int -> Int -> OvertonesO -> (Float,Float) -> V.Vector OvertonesO
splitHelp2 h1 x1 x2 x3 x4 v00 (y5,y6) = 
  let v2 = V.unsafeSlice x1 x2 v00
      v31 = V.map (\t -> (fst t) / y5) v2
      v32 = V.map (\t -> (snd t) / y6) v2
      v3 = V.zip v31 v32
      f1Tup (t1, w2) = V.imap (\ i _ -> (fst (V.unsafeIndex v3 i) * t1, snd (V.unsafeIndex v3 i) * w2)) v3
        in V.map f1Tup (h1 . V.unsafeSlice x3 x4 $ v00)        

-- | Generalized variant of the 'splitO2' with the different splitting variants depending on the first two ASCII lower case letters in the 'String' argument.
splitOG2 :: (OvertonesO -> OvertonesO) -> String -> Int -> OvertonesO -> V.Vector OvertonesO
splitOG2 h xs n v0
 | compare (V.length v0) (n + 1) == GT = 
    let c1s = take 2 . filter isAsciiLower $ xs
        v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        (x0, y0) = V.unsafeIndex v1 0 in
          case c1s of
            "ab" -> let (k1,k2,k3,k4) = (n - 1,V.length v0 - n,n - 1,V.length v0 - n) in splitHelp2 h k1 k2 k3 k4 v1 (x0,y0) 
            "ac" -> let (k1,k2,k3,k4) = (1,n - 1,n - 1,V.length v0 - n) in splitHelp2 h k1 k2 k3 k4 v1 (x0,y0) 
            "ad" -> let (k1,k2,k3,k4) = (n - 1,V.length v0 - n,0,n) in splitHelp2 h k1 k2 k3 k4 v1 (x0,y0) 
            _    -> let (k1,k2,k3,k4) = (1,n - 1,0,n) in splitHelp2 h k1 k2 k3 k4 v1 (x0,y0) 
 | otherwise = V.singleton v0    

-- | Generalized variant of the 'splitOG1' with a possibility to specify a default value for splitting parameters as the first argument 
-- @(Int,Int,Int,Int)@ and the sorted by the first element in the tuple (actually a 'String') in ascending order 'V.Vector' (the second one). 
-- Each 'String' in the 'V.Vector' must be unique and consist of lowercase ASCII letters.
splitOG12 :: (Int,Int,Int,Int) -> V.Vector (String,Int -> OvertonesO -> (Int,Int,Int,Int)) -> String -> Int -> OvertonesO -> V.Vector OvertonesO
splitOG12 (x1,x2,x3,x4) vf xs n v0
 | compare (V.length v0) (n + 1) == GT && not (V.null vf) = 
    let c1s = filter isAsciiLower $ xs
        v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        (x0, y0) = V.unsafeIndex v1 0 in let (k1,k2,k3,k4) = getBFstLSorted' (x1,x2,x3,x4) (V.toList . V.map (\(ys,g) -> (ys,g n v1)) $ vf) c1s in 
          splitHelp1 k1 k2 k3 k4 v1 (x0,y0) 
 | otherwise = V.singleton v0    

-- | Variant of the 'splitOG12' applied to the unsorted second argument. It sorts it internally. If you specify the already sorted second argument 
-- then it is better to use 'splitOG12'. Each 'String' in the 'V.Vector' must be unique and consist of lowercase ASCII letters.
splitOG12S :: (Int,Int,Int,Int) -> V.Vector (String,Int -> OvertonesO -> (Int,Int,Int,Int)) -> String -> Int -> OvertonesO -> V.Vector OvertonesO
splitOG12S (x1,x2,x3,x4) vf xs n v0
 | compare (V.length v0) (n + 1) == GT && not (V.null vf) = 
    let c1s = filter isAsciiLower $ xs
        v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        v2 = V.fromList . sortBy (\(x1s,_) (x2s,_) -> compare x1s x2s) . V.toList $ vf
        (x0, y0) = V.unsafeIndex v1 0 in let (k1,k2,k3,k4) = getBFstLSorted' (x1,x2,x3,x4) (V.toList . V.map (\(ys,g) -> (ys,g n v1)) $ v2) c1s in 
          splitHelp1 k1 k2 k3 k4 v1 (x0,y0) 
 | otherwise = V.singleton v0    

-- | Generalized variant of the 'splitOG2' with a possibility to specify a default value for splitting parameters as the first argument 
-- @(Int,Int,Int,Int)@ and the sorted by the first element in the tuple (actually a 'String') in ascending order 'V.Vector' (the second one). 
-- Each 'String' in the 'V.Vector' must be unique and consist of lowercase ASCII letters.
splitOG22 :: (Int,Int,Int,Int) -> V.Vector (String,Int -> OvertonesO -> (Int,Int,Int,Int)) -> (OvertonesO -> OvertonesO) -> String -> Int -> 
  OvertonesO -> V.Vector OvertonesO
splitOG22 (x1,x2,x3,x4) vf h xs n v0
 | compare (V.length v0) (n + 1) == GT && not (V.null vf) = 
    let c1s = filter isAsciiLower $ xs
        v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        (x0, y0) = V.unsafeIndex v1 0 in let (k1,k2,k3,k4) = getBFstLSorted' (x1,x2,x3,x4) (V.toList . V.map (\(ys,g) -> (ys,g n v1)) $ vf) c1s in 
          splitHelp2 h k1 k2 k3 k4 v1 (x0,y0) 
 | otherwise = V.singleton v0    
 
-- | Variant of the 'splitOG22' applied to the unsorted second argument. It sorts it internally. If you specify the already sorted second argument 
-- then it is better to use 'splitOG22'. Each 'String' in the 'V.Vector' must be unique and consist of lowercase ASCII letters.
splitOG22S :: (Int,Int,Int,Int) -> V.Vector (String,Int -> OvertonesO -> (Int,Int,Int,Int)) -> (OvertonesO -> OvertonesO) -> String -> Int -> 
  OvertonesO -> V.Vector OvertonesO
splitOG22S (x1,x2,x3,x4) vf h xs n v0
 | compare (V.length v0) (n + 1) == GT && not (V.null vf) = 
    let c1s = filter isAsciiLower $ xs
        v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare (abs x2) (abs x1)) . V.toList $ v0
        v2 = V.fromList . sortBy (\(x1s,_) (x2s,_) -> compare x1s x2s) . V.toList $ vf
        (x0, y0) = V.unsafeIndex v1 0 in let (k1,k2,k3,k4) = getBFstLSorted' (x1,x2,x3,x4) (V.toList . V.map (\(ys,g) -> (ys,g n v1)) $ v2) c1s in 
          splitHelp2 h k1 k2 k3 k4 v1 (x0,y0) 
 | otherwise = V.singleton v0    

-- | Concatenates a 'V.Vector' of 'OvertonesO' into a single 'OvertonesO'. Can be easily used with 'splitO'.
overConcat :: V.Vector OvertonesO -> OvertonesO
overConcat = V.concat . V.toList
