{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Ukrainian.Syllable
Copyright   :  (c) Oleksandr Zhabenko 2021-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

This module works with syllable segmentation in Ukrainian. It is rewritten
module MMSyn7.Syllable from the @mmsyn7s@ package : https://hackage.haskell.org/package/mmsyn7s
The information on Ukrainian syllable segmentation is taken from the:
 https://msn.khnu.km.ua/pluginfile.php/302375/mod_resource/content/1/%D0%9B.3.%D0%86%D0%86.%20%D0%A1%D0%BA%D0%BB%D0%B0%D0%B4.%D0%9D%D0%B0%D0%B3%D0%BE%D0%BB%D0%BE%D1%81.pdf
-}
module Aftovolio.Ukrainian.Syllable (
    -- * Basic functionality
    isVowel1,
    isSonorous1,
    isVoicedC1,
    isVoicelessC1,
    isNotVowel2,
    isNotVowel2',
    sndGroups,
    groupSnds,
    divCnsnts,
    reSyllableCntnts,
    divVwls,
    createSyllablesUkrS,
    notEqC,
    representProlonged,
    showS8,
    showFS,

    -- * With additional data used (probably for speed up)
    notEqCTup,
    divCnsntsTup,
    reSyllableCntntsTup,
    createSyllablesUkrSTup,
) where

import Aftovolio.Ukrainian.Melodics
import CaseBi.Arr
import Data.IntermediateStructures1 (mapI)
import qualified Data.List as L (groupBy)
import Data.Maybe (mapMaybe)
import Data.Tuple
import Data.Typeable
import GHC.Arr
import GHC.Base
import GHC.Int
import GHC.List
import GHC.Num (abs, (-))

-- Inspired by: https://github.com/OleksandrZhabenko/mm1/releases/tag/0.2.0.0

-- | Function-predicate 'isVowel1' checks whether its argument is a vowel representation in the 'Sound8' format.
isVowel1 :: Sound8 -> Bool
isVowel1 x = x < 7
{-# INLINE isVowel1 #-}

-- | Function-predicate 'isSonorous1' checks whether its argument is a sonorous consonant representation in the 'Sound8' format.
isSonorous1 :: Sound8 -> Bool
isSonorous1 x = x > 26 && x < 38
{-# INLINE isSonorous1 #-}

-- | Function-predicate 'isVoicedC1' checks whether its argument is a voiced consonant representation in the 'Sound8' format.
isVoicedC1 :: Sound8 -> Bool
isVoicedC1 x = x > 7 && x < 27
{-# INLINE isVoicedC1 #-}

-- | Function-predicate 'isVoiceless1' checks whether its argument is a voiceless consonant representation in the 'Sound8' format.
isVoicelessC1 :: Sound8 -> Bool
isVoicelessC1 x = x > 37 && x < 54
{-# INLINE isVoicelessC1 #-}

{- | Binary function-predicate 'isNotVowel2' checks whether its arguments are both consonant representations in the 'Sound8' format.
Starting from the version 0.6.0.0 variants of either of arguments is greater than 99 is also included.
-}
isNotVowel2 :: Sound8 -> Sound8 -> Bool
isNotVowel2 x y = x > 6 && y > 6
{-# INLINE isNotVowel2 #-}

{- | Binary function-predicate 'isNotVowel2'' checks whether its arguments are both consonant representations in the 'Sound8' format.
Starting from the version 0.6.0.0 variants of either of arguments is greater than 99 are not included (so its behaviour is equivalent  to the
'isNotVowel2' till the 0.5.3.0 version).
-}
isNotVowel2' :: Sound8 -> Sound8 -> Bool
isNotVowel2' x y = x < 100 && y < 100 && x > 6 && y > 6
{-# INLINE isNotVowel2' #-}

{- | Function 'sndGroups' converts a Ukrainian word being a list of 'Sound8' to the list of phonetically similar (consonants grouped with consonants and each vowel separately)
sounds representations in 'Sound8' format.
-}
sndGroups :: FlowSound -> [FlowSound]
sndGroups ys@(_ : _) = L.groupBy isNotVowel2 ys
sndGroups _ = []

groupSnds :: FlowSound -> [FlowSound]
groupSnds = L.groupBy (\x y -> isVowel1 x == isVowel1 y)

{- | Function 'divCnsnts' is used to divide groups of Ukrainian consonants into two-elements lists that later are made belonging to
different neighbour syllables if the group is between two vowels in a word. The group must be not empty, but this is not checked.
The phonetical information for the proper performance is taken from the:
https://msn.khnu.km.ua/pluginfile.php/302375/mod_resource/content/1/%D0%9B.3.%D0%86%D0%86.%20%D0%A1%D0%BA%D0%BB%D0%B0%D0%B4.%D0%9D%D0%B0%D0%B3%D0%BE%D0%BB%D0%BE%D1%81.pdf
-}
divCnsnts :: FlowSound -> (FlowSound -> FlowSound, FlowSound -> FlowSound)
divCnsnts xs@(x : ys@(y : zs@(z : ts@(_ : _))))
    | isSonorous1 x || isVoicedC1 x =
        case y of
            7 -> ((`mappend` [x, 7]), mappend zs) -- "рибаль-ство"
            _ -> ((`mappend` [x]), mappend ys)
    | isSonorous1 y =
        case z of
            7 -> ((`mappend` [x, y, 7]), mappend ts) -- "рокль-ський" (?), "супрасль-ський"
            _ -> ((`mappend` [x, y]), mappend zs) -- "дофр-ський" (?)
    | otherwise = (id, mappend xs)
divCnsnts xs@(x : ys@(y : zs@(z : ts)))
    | isSonorous1 x =
        case y of
            7 -> ((`mappend` [x, 7]), mappend zs) -- "поль-ка", "каль-ка"
            _ -> ((`mappend` [x]), mappend ys)
    | isSonorous1 y =
        case z of
            7 -> (id, mappend xs) -- "сього-дні"
            _ -> ((`mappend` [x, y]), mappend zs)
    | otherwise = (id, mappend xs)
divCnsnts xs@(x : ys@(y : zs))
    | (isSonorous1 x && notEqC x y && y /= 7) || (isVoicedC1 x && isVoicelessC1 y) =
        ((`mappend` [x]), mappend ys)
    | otherwise = (id, mappend xs)
divCnsnts xs = (id, mappend xs)

-- | Function 'divCnsntsTup' is a variant of the 'divCnsts' where you can provide the tuple element for 'getBFst'' inside.
divCnsntsTup ::
    Array Int (Int8, Bool) ->
    FlowSound ->
    (FlowSound -> FlowSound, FlowSound -> FlowSound)
divCnsntsTup !tup17 xs@(x : ys@(y : zs@(z : ts@(_ : _))))
    | isSonorous1 x || isVoicedC1 x =
        case y of
            7 -> ((`mappend` [x, 7]), mappend zs) -- "рибаль-ство"
            _ -> ((`mappend` [x]), mappend ys)
    | isSonorous1 y =
        case z of
            7 -> ((`mappend` [x, y, 7]), mappend ts) -- "рокль-ський" (?), "супрасль-ський"
            _ -> ((`mappend` [x, y]), mappend zs) -- "дофр-ський" (?)
    | otherwise = (id, mappend xs)
divCnsntsTup !tup17 xs@(x : ys@(y : zs@(z : ts)))
    | isSonorous1 x =
        case y of
            7 -> ((`mappend` [x, 7]), mappend zs) -- "поль-ка", "каль-ка"
            _ -> ((`mappend` [x]), mappend ys)
    | isSonorous1 y =
        case z of
            7 -> (id, mappend xs) -- "сього-дні"
            _ -> ((`mappend` [x, y]), mappend zs)
    | otherwise = (id, mappend xs)
divCnsntsTup !tup17 xs@(x : ys@(y : _))
    | (isSonorous1 x && (notEqCTup tup17 x y) && y /= 7)
        || (isVoicedC1 x && isVoicelessC1 y) =
        ((`mappend` [x]), mappend ys)
    | otherwise = (id, mappend xs)
divCnsntsTup _ xs = (id, mappend xs)

reSyllableCntntsTup :: Array Int (Int8, Bool) -> [FlowSound] -> [FlowSound]
reSyllableCntntsTup !tup17 (xs : ys : zs : xss)
    | (> 6) . last $ ys =
        fst (divCnsntsTup tup17 ys) xs
            : reSyllableCntntsTup tup17 (snd (divCnsntsTup tup17 ys) zs : xss)
    | otherwise = reSyllableCntntsTup tup17 ((xs `mappend` ys) : zs : xss)
reSyllableCntntsTup !tup17 (xs : ys : _) = [xs `mappend` ys]
reSyllableCntntsTup !tup17 xss = xss

reSyllableCntnts :: [FlowSound] -> [FlowSound]
reSyllableCntnts (xs : ys : zs : xss)
    | (> 6) . last $ ys =
        fst (divCnsnts ys) xs : reSyllableCntnts (snd (divCnsnts ys) zs : xss)
    | otherwise = reSyllableCntnts ((xs `mappend` ys) : zs : xss)
reSyllableCntnts (xs : ys : _) = [xs `mappend` ys]
reSyllableCntnts xss = xss

divVwls :: [FlowSound] -> [FlowSound]
divVwls = mapI (\ws -> (length . filter isVowel1 $ ws) > 1) h3
  where
    h3 us =
        [ys `mappend` take 1 zs]
            `mappend` (L.groupBy (\x y -> isVowel1 x && y > 6) . drop 1 $ zs)
      where
        (ys, zs) = span (> 6) us

createSyllablesUkrS :: String -> [[FlowSound]]
createSyllablesUkrS =
    map (divVwls . reSyllableCntnts . groupSnds)
        . words1
        . convertToProperUkrainianI8
{-# INLINE createSyllablesUkrS #-}

createSyllablesUkrSTup ::
    Array Int (Int8, Bool) ->
    Array Int (Int8, Bool) ->
    Array Int (Int8, Bool) ->
    Array Int (Int8, Bool) ->
    Array Int ([Int8], Bool) ->
    Array Int ([Int8], Int8) ->
    Array Int (Int8, FlowSound -> Sound8) ->
    Array Int (Int8, Bool) ->
    Array Int ([Int8], Bool) ->
    Array Int ([Int8], Bool) ->
    Array Int ([Int8], Bool) ->
    Array Int (Int8, [Int8]) ->
    Array Int (Char, Int8) ->
    Array Int (Int8, [Int8]) ->
    Array Int (Char, Bool) ->
    Array Int (Char, Bool) ->
    Array Int (Int8, Bool) ->
    String ->
    [[FlowSound]]
createSyllablesUkrSTup !tup1 !tup2 !tup3 !tup4 !tup5 !tup6 !tup7 !tup8 !tup9 !tup10 !tup11 !tup12 !tup13 !tup14 !tup15 !tup16 !tup17 =
    map (divVwls . reSyllableCntntsTup tup17 . groupSnds)
        . words1
        . convertToProperUkrainianI8WithTuples
            tup1
            tup2
            tup3
            tup4
            tup5
            tup6
            tup7
            tup8
            tup9
            tup10
            tup11
            tup12
            tup13
            tup14
            tup15
            tup16
{-# INLINE createSyllablesUkrSTup #-}

-- | Practically this is an optimized version for this case 'words' function from Prelude.
words1 :: FlowSound -> [FlowSound]
words1 xs = if null ts then [] else w : words1 s''
  where
    ts = dropWhile (> 99) xs
    (w, s'') = span (< 100) ts
{-# NOINLINE words1 #-}

-----------------------------------------------------

-- | Binary function-predicate 'notEqC' checks whether its arguments are not the same consonant sound representations (not taking palatalization into account).
notEqC :: Sound8 -> Sound8 -> Bool
notEqC x y
    | x == 49 || x == 54 =
        case y of
            49 -> False
            54 -> False
            _ -> True
    | x == 66 || x == 38 =
        case y of
            38 -> False
            66 -> False
            _ -> True
    | x == y = False
    | abs (x - y) == 1 =
        getBFstLSorted'
            True
            ( [ (8, False)
              , (10, False)
              , (15, False)
              , (17, False)
              , (19, False)
              , (21, False)
              , (23, False)
              , (25, False)
              , (28, False)
              , (30, False)
              , (32, False)
              , (34, False)
              , (36, False)
              , (39, False)
              , (41, False)
              , (43, False)
              , (45, False)
              , (47, False)
              , (50, False)
              , (52, False)
              ]
            )
            . min x
            $ y
    | otherwise = True
{-# INLINE notEqC #-}

-- | Binary function-predicate 'notEqC' checks whether its arguments are not the same consonant sound representations (not taking palatalization into account).
notEqCTup :: Array Int (Int8, Bool) -> Sound8 -> Sound8 -> Bool
notEqCTup !tup17 x y
    | x == 49 || x == 54 =
        case y of
            49 -> False
            54 -> False
            _ -> True
    | x == 66 || x == 38 =
        case y of
            38 -> False
            66 -> False
            _ -> True
    | x == y = False
    | abs (x - y) == 1 = getBFst' (True, tup17) . min x $ y
    | otherwise = True
{-# INLINE notEqCTup #-}

{- | Function 'representProlonged' converts duplicated consequent in the syllable consonants
so that they are represented by just one 'Sound8'. After applying the function to the list of 'Sound8' being a syllable all groups of duplicated consequent consonants
in every syllable are represented with only one 'Sound8' respectively.
-}
representProlonged :: FlowSound -> FlowSound
representProlonged (x : y : xs)
    | isVowel1 x = x : representProlonged (y : xs)
    | not . notEqC x $ y = y : representProlonged xs
    | otherwise = x : representProlonged (y : xs)
representProlonged xs = xs

showS8 :: Sound8 -> String
showS8 =
    getBFstLSorted'
        " "
        [ (1, "\1072")
        , (2, "\1077")
        , (3, "\1086")
        , (4, "\1091")
        , (5, "\1080")
        , (6, "\1110")
        , (7, "\1100")
        , (8, "\1076\1079")
        , (9, "\1076\1079")
        , (10, "\1078")
        , (11, "\1078")
        , (15, "\1073")
        , (16, "\1073")
        , (17, "\1076")
        , (18, "\1076")
        , (19, "\1169")
        , (20, "\1169")
        , (21, "\1075")
        , (22, "\1075")
        , (23, "\1076\1078")
        , (24, "\1076\1078")
        , (25, "\1079")
        , (26, "\1079")
        , (27, "\1081")
        , (28, "\1083")
        , (29, "\1083")
        , (30, "\1084")
        , (31, "\1084")
        , (32, "\1085")
        , (33, "\1085")
        , (34, "\1088")
        , (35, "\1088")
        , (36, "\1074")
        , (37, "\1074")
        , (38, "\1094")
        , (39, "\1095")
        , (40, "\1095")
        , (41, "\1096")
        , (42, "\1096")
        , (43, "\1092")
        , (44, "\1092")
        , (45, "\1082")
        , (46, "\1082")
        , (47, "\1087")
        , (48, "\1087")
        , (49, "\1089")
        , (50, "\1090")
        , (51, "\1090")
        , (52, "\1093")
        , (53, "\1093")
        , (54, "\1089\1100")
        , (66, "\1094\1100")
        ]
{-# INLINE showS8 #-}

showFS :: FlowSound -> String
showFS = concatMap showS8 -- Probably, it is better to transform several consequent spaces into the combination smth like \", \" (but not in this version)
{-# INLINE showFS #-}
