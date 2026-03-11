{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Ukrainian.Melodics
Copyright   :  (c) OleksandrZhabenko 2021-2024
License     :  MIT
Maintainer  :  oleksandr.zhabenko@yahoo.com

Functions provide functionality of a musical instrument synthesizer or for Ukrainian speech synthesis
especially for poets, translators and writers. Is rewritten from the module Melodics.ByteString.Ukrainian.Arr
for optimization purposes.
Phonetic material is taken from the :

Solomija Buk, Ján Mačutek, Andrij Rovenchak. Some properties of
the Ukrainian writing system. [Electronic resource] https://arxiv.org/ftp/arxiv/papers/0802/0802.4198.pdf
-}
module Aftovolio.Ukrainian.Melodics (
    -- * Basic functions
    Sound8,
    FlowSound,
    convertToProperUkrainianI8WithTuples,
    convertToProperUkrainianI8,
    isUkrainianL,
    linkFileNameI8,

    -- * Transformation functions
    дзT,
    жT,
    дT,
    гT,
    зT,
    цT,
    чT,
    шT,
    фT,
    кT,
    пT,
    сT,
    тT,
    хT,
    сьT,
    нтT,
    стT,
    тьT,
    цьT,
) where

import Aftovolio.Ukrainian.Common2
import CaseBi.Arr
import Data.Char
import Data.List (uncons)
import Data.Maybe (fromJust)
import GHC.Arr
import GHC.Base
import GHC.Int
import GHC.List
import GHC.Num ((-))

-- | Is used to signify the optimization data type of 'Int8'.
type Sound8 = Int8

type FlowSound = [Sound8]

{- | The function that uses the following correspondence between the previous data type UZPP2 and the 'Sound8'.
See for more implementation information:
<https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#ability-to-use-your-own-durations-of-representations-of-sounds-or-phonetic-phenomena>

Starting from the version 0.6.0.0:

-2 -> 102
-1 -> 101
0 -> 100
-}
convertToProperUkrainianI8 :: String -> FlowSound
convertToProperUkrainianI8 =
    let !tup1 =
            listArray
                (0, 13)
                [ (10, True)
                , (17, True)
                , (21, True)
                , (25, True)
                , (32, True)
                , (38, True)
                , (39, True)
                , (41, True)
                , (43, True)
                , (45, True)
                , (47, True)
                , (49, True)
                , (50, True)
                , (52, True)
                ]
        !tup2 =
            listArray
                (0, 19)
                [ (10, True)
                , (15, True)
                , (17, True)
                , (19, True)
                , (21, True)
                , (25, True)
                , (28, True)
                , (30, True)
                , (32, True)
                , (34, True)
                , (36, True)
                , (38, True)
                , (39, True)
                , (41, True)
                , (43, True)
                , (45, True)
                , (47, True)
                , (49, True)
                , (50, True)
                , (52, True)
                ]
        !tup3 =
            listArray
                (0, 13)
                [ (10, False)
                , (17, False)
                , (21, False)
                , (25, False)
                , (32, False)
                , (38, False)
                , (39, False)
                , (41, False)
                , (43, False)
                , (45, False)
                , (47, False)
                , (49, False)
                , (50, False)
                , (52, False)
                ]
        !tup4 = listArray (0, 5) [(17, True), (32, True), (38, True), (49, True), (50, True), (52, True)]
        !tup5 =
            listArray
                (0, 8)
                [ ([17, 10], True)
                , ([17, 25], True)
                , ([32, 50], True)
                , ([38, 7], True)
                , ([49, 7], True)
                , ([49, 50], True)
                , ([50, 7], True)
                , ([50, 49], True)
                , ([52, 21], True)
                ]
        !tup6 =
            listArray
                (0, 8)
                [ ([17, 10], 23)
                , ([17, 25], 8)
                , ([32, 50], 62)
                , ([38, 7], 66)
                , ([49, 7], 54)
                , ([49, 50], 63)
                , ([50, 7], 64)
                , ([50, 49], 38)
                , ([52, 21], 21)
                ]
        !tup8 =
            listArray
                (0, 7)
                [ (8, True)
                , (10, True)
                , (15, True)
                , (17, True)
                , (19, True)
                , (21, True)
                , (23, True)
                , (25, True)
                ]
        !tup9 =
            listArray
                (0, 10)
                [ ([15, 7], True)
                , ([17, 7], True)
                , ([28, 7], True)
                , ([30, 7], True)
                , ([32, 7], True)
                , ([36, 7], True)
                , ([38, 7], True)
                , ([43, 7], True)
                , ([47, 7], True)
                , ([49, 7], True)
                , ([50, 7], True)
                ]
        !tup10 = listArray (0, 4) [([12], True), ([13], True), ([14], True), ([64], True), ([65], True)]
        !tup11 =
            listArray
                (0, 7)
                [ ([8, 7], True)
                , ([17, 7], True)
                , ([25, 7], True)
                , ([28, 7], True)
                , ([32, 7], True)
                , ([38, 7], True)
                , ([49, 7], True)
                , ([50, 7], True)
                ]
        tup7 =
            listArray
                (0, 18)
                [ (8, дзT tup9 tup10)
                , (10, жT)
                , (17, дT)
                , (21, гT)
                , (25, зT tup9 tup10)
                , (38, цT tup8 tup9 tup10)
                , (39, чT)
                , (41, шT)
                , (43, фT)
                , (45, кT)
                , (47, пT)
                , (49, сT tup8 tup9 tup10)
                , (50, тT tup8 tup11 tup10)
                , (52, хT)
                , (54, сьT)
                , (62, нтT)
                , (63, стT)
                , (64, тьT)
                , (66, цьT)
                ]
        !tup12 =
            listArray
                (0, 6)
                [ (12, [8, 7])
                , (13, [25, 7])
                , (14, [17, 7])
                , (62, [32, 50])
                , (63, [49, 50])
                , (64, [50, 7])
                , (65, [32, 7])
                ]
        !tup13 =
            listArray
                (0, 36)
                [ ('\'', 102)
                , ('-', 101)
                , ('\700', 60)
                , ('\1072', 1)
                , ('\1073', 15)
                , ('\1074', 36)
                , ('\1075', 21)
                , ('\1076', 17)
                , ('\1077', 2)
                , ('\1078', 10)
                , ('\1079', 25)
                , ('\1080', 5)
                , ('\1081', 27)
                , ('\1082', 45)
                , ('\1083', 28)
                , ('\1084', 30)
                , ('\1085', 32)
                , ('\1086', 3)
                , ('\1087', 47)
                , ('\1088', 34)
                , ('\1089', 49)
                , ('\1090', 50)
                , ('\1091', 4)
                , ('\1092', 43)
                , ('\1093', 52)
                , ('\1094', 38)
                , ('\1095', 39)
                , ('\1096', 41)
                , ('\1097', 55)
                , ('\1100', 7)
                , ('\1102', 56)
                , ('\1103', 57)
                , ('\1108', 58)
                , ('\1110', 6)
                , ('\1111', 59)
                , ('\1169', 19)
                , ('\8217', 61)
                ]
        !tup14 =
            listArray
                (0, 8)
                [ (55, [41, 39])
                , (56, [27, 4])
                , (57, [27, 1])
                , (58, [27, 2])
                , (59, [27, 6])
                , (60, [101])
                , (61, [101])
                , (101, [101])
                , (102, [101])
                ]
        !tup15 =
            listArray
                (0, 15)
                [ ('\'', True)
                , ('-', True)
                , ('\700', True)
                , ('\1028', True)
                , ('\1030', True)
                , ('\1031', True)
                , ('\1068', True)
                , ('\1100', True)
                , ('\1102', True)
                , ('\1103', True)
                , ('\1108', True)
                , ('\1110', True)
                , ('\1111', True)
                , ('\1168', True)
                , ('\1169', True)
                , ('\8217', True)
                ]
        !tup16 =
            listArray
                (0, 20)
                [ ('\1073', True)
                , ('\1074', True)
                , ('\1075', True)
                , ('\1076', True)
                , ('\1078', True)
                , ('\1079', True)
                , ('\1082', True)
                , ('\1083', True)
                , ('\1084', True)
                , ('\1085', True)
                , ('\1087', True)
                , ('\1088', True)
                , ('\1089', True)
                , ('\1090', True)
                , ('\1092', True)
                , ('\1093', True)
                , ('\1094', True)
                , ('\1095', True)
                , ('\1096', True)
                , ('\1097', True)
                , ('\1169', True)
                ]
     in correctB
            . correctA tup12
            . applyChanges tup7
            . bsToCharUkr tup6
            . createTuplesByAnalysis tup1 tup2 tup3 tup4 tup5
            . secondConv tup14
            . filterUkr tup13
            . changeIotated tup16
            . filter
                (\x -> isUkrainianLTup tup15 x || isSpace x || isControl x || isPunctuation x)
            . map toLower
{-# INLINE convertToProperUkrainianI8 #-}

{- | A full variant of the 'convertToProperUkrainianI8' function with all the elements for the 'getBFst'' function being
provided as 'Array' 'Int' (data tuple). Can be useful to reduce number of calculations in the complex usage scenarios.
-}
convertToProperUkrainianI8WithTuples ::
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
    [Char] ->
    FlowSound
convertToProperUkrainianI8WithTuples !tup1 !tup2 !tup3 !tup4 !tup5 !tup6 tup7 !tup8 !tup9 !tup10 !tup11 !tup12 !tup13 !tup14 !tup15 !tup16 =
    correctB
        . correctA tup12
        . applyChanges tup7
        . bsToCharUkr tup6
        . createTuplesByAnalysis tup1 tup2 tup3 tup4 tup5
        . secondConv tup14
        . filterUkr tup13
        . changeIotated tup16
        . filter
            (\x -> isUkrainianLTup tup15 x || isSpace x || isControl x || isPunctuation x)
        . map toLower
{-# INLINE convertToProperUkrainianI8WithTuples #-}

changeIotated :: Array Int (Char, Bool) -> String -> String
changeIotated !tup16 (x : y : zs)
    | (y `elem` ("\1102\1103\1108\1110" :: String)) && isConsNotJTup tup16 x =
        x
            : '\1100'
            : ( case y of
                    '\1102' -> '\1091'
                    '\1103' -> '\1072'
                    '\1108' -> '\1077'
                    ~r -> '\1110'
              )
            : changeIotated tup16 zs
    | x == '\'' || x == '\x2019' || x == '\x02BC' || x == '-' =
        if (y `elem` ("\1102\1103\1108\1110" :: String))
            then
                '\1081'
                    : ( case y of
                            '\1102' -> '\1091'
                            '\1103' -> '\1072'
                            '\1108' -> '\1077'
                            ~r -> '\1110'
                      )
                    : changeIotated tup16 zs
            else changeIotated tup16 (y : zs)
    | otherwise = x : changeIotated tup16 (y : zs)
changeIotated _ xs = xs

filterUkr :: Array Int (Char, Int8) -> String -> FlowSound
filterUkr tup13 = let !tup = (100, tup13) in map (getBFst' tup)
{-# INLINE filterUkr #-}

secondConv :: Array Int (Int8, [Int8]) -> FlowSound -> FlowSound
secondConv tup14 = concatMap (\y -> getBFst' ([y], tup14) y)
{-# INLINE secondConv #-}

createTuplesByAnalysis ::
    Array Int (Int8, Bool) ->
    Array Int (Int8, Bool) ->
    Array Int (Int8, Bool) ->
    Array Int (Int8, Bool) ->
    Array Int ([Int8], Bool) ->
    FlowSound ->
    [FlowSound]
createTuplesByAnalysis tup1 tup2 tup3 tup4 tup5 x@(h : ta)
    | getBFst' (False, tup1) h = initialA tup3 tup4 tup5 x
    | not (null ta) && (head ta == 27 && getBFst' (False, tup2) h) =
        [h] : [7] : createTuplesByAnalysis tup1 tup2 tup3 tup4 tup5 (drop 1 ta)
    | otherwise = [h] : createTuplesByAnalysis tup1 tup2 tup3 tup4 tup5 ta
createTuplesByAnalysis _ _ _ _ _ _ = []

initialA ::
    Array Int (Int8, Bool) ->
    Array Int (Int8, Bool) ->
    Array Int ([Int8], Bool) ->
    FlowSound ->
    [FlowSound]
initialA tup3 tup4 tup5 t1@(t : ts)
    | t < 1 || t > 99 = [100] : initialA tup3 tup4 tup5 ts
    | getBFst' (True, tup3) t = [t] : initialA tup3 tup4 tup5 ts
    | getBFst' (False, tup4) t =
        let (us, vs) = splitAt 2 t1
         in if getBFst' (False, tup5) us
                then us : initialA tup3 tup4 tup5 vs
                else [t] : initialA tup3 tup4 tup5 ts
    | otherwise = [t] : initialA tup3 tup4 tup5 ts
initialA _ _ _ _ = []

bsToCharUkr :: Array Int ([Int8], Int8) -> [FlowSound] -> FlowSound
bsToCharUkr tup6 zs@(_ : _) = map (g tup6) zs
  where
    g tup6 ts@(t : _) = getBFst' (t, tup6) ts
    g _ _ = 101
bsToCharUkr _ _ = []
{-# INLINE bsToCharUkr #-}

applyChanges ::
    Array Int (Int8, FlowSound -> Sound8) ->
    FlowSound ->
    FlowSound
applyChanges tup7 ys = foldr f v ys
  where
    v = []
    f x xs@(_ : _) = getBFst' (\_ -> x, tup7) x xs : xs
    f x _ = [x]
{-# INLINE applyChanges #-}

isVoicedObstruent :: FlowSound -> Bool
isVoicedObstruent (x : _) = x > 7 && x < 27
isVoicedObstruent _ = False
{-# INLINE isVoicedObstruent #-}

isVoicedObstruentH :: Array Int (Int8, Bool) -> FlowSound -> Bool
isVoicedObstruentH tup8 (x : _) = getBFst' (False, tup8) x
isVoicedObstruentH _ _ = False
{-# INLINE isVoicedObstruentH #-}

isVoicedObstruentS :: FlowSound -> Bool
isVoicedObstruentS (x : _) = x > 11 && x < 15
isVoicedObstruentS _ = False
{-# INLINE isVoicedObstruentS #-}

isSoftDOrL ::
    Array Int ([Int8], Bool) -> Array Int ([Int8], Bool) -> FlowSound -> Bool
isSoftDOrL tup9 tup10 xs =
    getBFst' (False, tup9) (takeFromFT_ 2 xs)
        || getBFst' (False, tup10) (takeFromFT_ 1 xs)
{-# INLINE isSoftDOrL #-}

isSoftDen ::
    Array Int ([Int8], Bool) -> Array Int ([Int8], Bool) -> FlowSound -> Bool
isSoftDen tup11 tup10 xs =
    getBFst' (False, tup11) (takeFromFT_ 2 xs)
        || getBFst' (False, tup10) (takeFromFT_ 1 xs)
{-# INLINE isSoftDen #-}

гT :: FlowSound -> Sound8
гT (t : _)
    | t == 45 || t == 50 = 52 -- г х
    | otherwise = 21
гT _ = 21
{-# INLINE гT #-}

дT :: FlowSound -> Sound8
дT t1@(_ : _)
    | takeFromFT_ 1 t1 `elem` [[10], [39], [41]] = 23 --  д дж
    | takeFromFT_ 2 t1 `elem` [[49, 7], [38, 7]] = 12 --  д дзь
    | takeFromFT_ 1 t1 `elem` [[54], [66]] = 12 --  д дзь
    | takeFromFT_ 1 t1 `elem` [[25], [49], [38]] = 8 --   д дз
    | otherwise = 17
дT _ = 17
{-# INLINE дT #-}

дзT :: Array Int ([Int8], Bool) -> Array Int ([Int8], Bool) -> FlowSound -> Sound8
дзT tup9 tup10 t1@(_ : _)
    | isSoftDOrL tup9 tup10 t1 = 12 -- дз дзь
    | otherwise = 8
дзT _ _ _ = 8
{-# INLINE дзT #-}

жT :: FlowSound -> Sound8
жT t1@(_ : _)
    | takeFromFT 2 t1 `elem` [[49, 7], [38, 7]] = 13 -- ж зь
    | takeFromFT 1 t1 `elem` [[54], [66]] = 13
    | otherwise = 10
жT _ = 10
{-# INLINE жT #-}

зT :: Array Int ([Int8], Bool) -> Array Int ([Int8], Bool) -> FlowSound -> Sound8
зT tup9 tup10 t1@(_ : _)
    | takeFromFT_ 1 t1
        `elem` [[10], [39], [41]] || takeFromFT_ 2 t1 == [17, 10] || takeFromFT_ 1 t1 == [23] =
        10 -- з ж
    | isSoftDOrL tup9 tup10 t1 = 13 -- з зь
    | takeFromFT 1 t1 `elem` [[39], [41]] = 41 --  з ш
    | takeFromFT 1 t1
        `elem` [[49], [38]] || takeFromFT_ 1 t1
        `elem` [[45], [47], [50], [43], [52]] =
        49 --  з с
    | otherwise = 25
зT _ _ _ = 25
{-# INLINE зT #-}

кT :: FlowSound -> Sound8
кT t1@(_ : _)
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 19
    | otherwise = 45
кT _ = 45
{-# INLINE кT #-}

нтT :: FlowSound -> Sound8
нтT t1@(_ : _)
    | takeFromFT 2 t1 == [49, 50] || takeFromFT 1 t1 == [63] = 32
    | takeFromFT 3 t1 == [49, 7, 45] || takeFromFT 2 t1 == [54, 45] = 65
    | otherwise = 62
нтT _ = 62
{-# INLINE нтT #-}

пT :: FlowSound -> Sound8
пT t1@(_ : _)
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 15
    | otherwise = 47
пT _ = 47
{-# INLINE пT #-}

сT ::
    Array Int (Int8, Bool) ->
    Array Int ([Int8], Bool) ->
    Array Int ([Int8], Bool) ->
    FlowSound ->
    Sound8
сT tup8 tup9 tup10 t1@(_ : _)
    | ( (isVoicedObstruentH tup8 . takeFromFT_ 1 $ t1)
            && drop 1 (takeFromFT_ 2 t1)
            == [7]
      )
        || isVoicedObstruentS (takeFromFT_ 1 t1) =
        13
    | isVoicedObstruentH tup8 . takeFromFT_ 1 $ t1 = 25
    | isSoftDOrL tup9 tup10 t1 = 54
    | takeFromFT_ 1 t1 == [41] = 41
    | otherwise = 49
сT _ _ _ _ = 49
{-# INLINE сT #-}

стT :: FlowSound -> Sound8
стT t1@(_ : _)
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 25
    | takeFromFT_ 3 t1
        == [49, 7, 45]
        || (takeFromFT_ 2 t1 `elem` [[54, 45], [38, 7]])
        || takeFromFT_ 1 t1
        == [66] =
        54
    | takeFromFT_ 1 t1 `elem` [[49], [32]] = 49
    | takeFromFT_ 1 t1 == [39] = 41
    | otherwise = 63
стT _ = 63
{-# INLINE стT #-}

сьT :: FlowSound -> Sound8
сьT t1@(_ : _)
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 13
    | otherwise = 54
сьT _ = 54
{-# INLINE сьT #-}

тT ::
    Array Int (Int8, Bool) ->
    Array Int ([Int8], Bool) ->
    Array Int ([Int8], Bool) ->
    FlowSound ->
    Sound8
тT tup8 tup11 tup10 t1@(_ : _)
    | ( (isVoicedObstruentH tup8 . takeFromFT_ 1 $ t1)
            && drop 1 (takeFromFT_ 2 t1)
            == [7]
      )
        || isVoicedObstruentS (takeFromFT_ 1 t1) =
        14
    | isVoicedObstruentH tup8 . takeFromFT_ 1 $ t1 = 17
    | takeFromFT_ 2 t1 == [38, 7] || takeFromFT_ 1 t1 == [66] = 66
    | takeFromFT_ 1 t1 == [38] = 38
    | isSoftDen tup11 tup10 t1 = 64
    | takeFromFT_ 1 t1 `elem` [[39], [41]] = 39
    | otherwise = 50
тT _ _ _ _ = 50
{-# INLINE тT #-}

тьT :: FlowSound -> Sound8
тьT t1@(_ : _)
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 14
    | takeFromFT_ 3 t1 == [49, 7, 1] || takeFromFT_ 2 t1 == [54, 1] = 66
    | otherwise = 64
тьT _ = 64
{-# INLINE тьT #-}

фT :: FlowSound -> Sound8
фT t1@(_ : _)
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 36
    | otherwise = 43
фT _ = 43
{-# INLINE фT #-}

хT :: FlowSound -> Sound8
хT t1@(_ : _)
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 21
    | otherwise = 52
хT _ = 52
{-# INLINE хT #-}

цT ::
    Array Int (Int8, Bool) ->
    Array Int ([Int8], Bool) ->
    Array Int ([Int8], Bool) ->
    FlowSound ->
    Sound8
цT tup8 tup9 tup10 t1@(_ : _)
    | ( (isVoicedObstruentH tup8 . takeFromFT_ 1 $ t1)
            && drop 1 (takeFromFT_ 2 t1)
            == [7]
      )
        || isVoicedObstruentS (takeFromFT_ 1 t1) =
        12
    | isSoftDOrL tup9 tup10 t1 = 66
    | isVoicedObstruentH tup8 . takeFromFT_ 1 $ t1 = 8
    | otherwise = 38
цT _ _ _ _ = 38
{-# INLINE цT #-}

цьT :: FlowSound -> Sound8
цьT t1@(_ : _)
    | (isVoicedObstruent . takeFromFT_ 1 $ t1) && drop 1 (takeFromFT_ 2 t1) == [7] =
        12
    | otherwise = 66
цьT _ = 66
{-# INLINE цьT #-}

чT :: FlowSound -> Sound8
чT t1@(_ : _)
    | takeFromFT_ 2 t1 `elem` [[49, 7], [38, 7]] || takeFromFT_ 1 t1 `elem` [[54], [66]] =
        66
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 23
    | otherwise = 39
чT _ = 39
{-# INLINE чT #-}

шT :: FlowSound -> Sound8
шT t1@(_ : _)
    | takeFromFT_ 2 t1 `elem` [[49, 7], [38, 7]] || takeFromFT_ 1 t1 `elem` [[54], [66]] =
        54
    | isVoicedObstruent . takeFromFT_ 1 $ t1 = 10
    | otherwise = 41
шT _ = 41
{-# INLINE шT #-}

takeFromFT :: Int -> FlowSound -> FlowSound
takeFromFT n ts
    | if n < 1 then True else null ts = []
    | n < 2 = [k]
    | otherwise = k : takeFromFT (n - 1) (take (n - 1) ts)
  where
    k = head ts

takeFromFT2 :: Int -> FlowSound -> FlowSound
takeFromFT2 n ts
    | if n < 1 then True else null ts = []
    | n < 2 = [ks]
    | otherwise = ks : takeFromFT2 (n - 1) (tail ts)
  where
    ks = head ts

dropFromFT2 :: Int -> FlowSound -> FlowSound
dropFromFT2 n ts
    | if n < 1 then True else null ts = []
    | n < 2 = tail ts
    | otherwise = dropFromFT2 (n - 1) (tail ts)

takeFromFT_ :: Int -> FlowSound -> FlowSound
takeFromFT_ n = takeFromFT n . filter (\t -> t > 0 && t < 100)
{-# INLINE takeFromFT_ #-}

correctA :: Array Int (Int8, [Int8]) -> FlowSound -> FlowSound
correctA tup12 = correctSomeW . separateSoftS tup12
{-# INLINE correctA #-}

separateSoftS :: Array Int (Int8, [Int8]) -> FlowSound -> FlowSound
separateSoftS tup12 = concatMap (\x -> getBFst' ([x], tup12) x)
{-# INLINE separateSoftS #-}

correctSomeW :: FlowSound -> FlowSound
correctSomeW (x : y : z : xs@(t : ys))
    | x == 50 && y == 7 && z == 54 && t == 1 = 66 : 66 : 1 : correctSomeW ys
    | (x > 99) && y == 27 && z == 1 =
        if take 2 xs == [39, 32]
            then x : y : z : 41 : correctSomeW ys
            else x : correctSomeW (y : z : xs)
    | otherwise = x : correctSomeW (y : z : xs)
correctSomeW zs = zs

correctB :: FlowSound -> FlowSound
correctB ys@(x : xs)
    | (length . filter (== 100) . takeFromFT2 6 $ ys) > 1 =
        map (\t -> if t >= 100 then 101 else t) (takeFromFT2 6 ys)
            `mappend` correctB (dropFromFT2 6 ys)
    | otherwise = (if x > 100 then 101 else x) : correctB xs
correctB _ = []

-- | Can be used to map the 'Sound8' representation and the mmsyn6ukr-array files with some recordings.
linkFileNameI8 :: Sound8 -> Char
linkFileNameI8 =
    getBFstLSorted'
        '0'
        ( [ (1, 'A')
          , (2, 'H')
          , (3, 'Q')
          , (4, 'W')
          , (5, 'K')
          , (6, 'e')
          , (7, 'd')
          , (8, 'G')
          , (10, 'I')
          , (15, 'B')
          , (17, 'E')
          , (19, 'f')
          , (21, 'D')
          , (23, 'F')
          , (25, 'J')
          , (27, 'L')
          , (28, 'N')
          , (30, 'O')
          , (32, 'P')
          , (34, 'S')
          , (36, 'C')
          , (38, 'Z')
          , (39, 'b')
          , (41, 'c')
          , (43, 'X')
          , (45, 'M')
          , (47, 'R')
          , (49, 'T')
          , (50, 'V')
          , (52, 'Y')
          , (54, 'U')
          , (60, '0')
          , (61, '0')
          , (66, 'a')
          ]
        )
{-# INLINE linkFileNameI8 #-}
