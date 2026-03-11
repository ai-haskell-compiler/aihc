{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK -show-extensions #-}

{- |
Module      :  Aftovolio.Ukrainian.PrepareText
Copyright   :  (c) OleksandrZhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Earlier it has been a module DobutokO.Poetry.Ukrainian.PrepareText
from the @dobutokO-poetry@ package.
In particular, this module can be used to prepare the Ukrainian text
by applying the most needed grammar for AFTOVolio to avoid misunderstanding
for the produced text. The attention is paid to the prepositions, pronouns, conjunctions
and particles that are most commonly connected (or not) in a significant way
with the next text.
Uses the information from:
https://uk.wikipedia.org/wiki/%D0%A1%D0%BF%D0%BE%D0%BB%D1%83%D1%87%D0%BD%D0%B8%D0%BA
and
https://uk.wikipedia.org/wiki/%D0%A7%D0%B0%D1%81%D1%82%D0%BA%D0%B0_(%D0%BC%D0%BE%D0%B2%D0%BE%D0%B7%D0%BD%D0%B0%D0%B2%D1%81%D1%82%D0%B2%D0%BE)

Uses arrays instead of vectors.
-}
module Aftovolio.Ukrainian.PrepareText (
    -- * Basic functions
    complexWords,
    participleConc,
    splitLines,
    splitLinesN,
    auxiliary1,
    isPreposition,
    isParticipleAppended,
    isPrepended,
    isConcatenated,
    isSpC,
    isUkrainianL,
    concatenated2,
    jottedConv,
    jottedCnv,

    -- * The end-user functions
    prepareText,
    prepareTextN,
    prepareTextN2,
    prepareTextN3,
    prepareTextNG,
    growLinesN,
    prepareGrowTextMN,
    prepareGrowTextMNG,
    tuneLinesN,
    prepareTuneTextMN,
    prepareTuneTextMNG,

    -- * Used to transform after convertToProperUkrainian from mmsyn6ukr package
    aux4,
) where

import CaseBi.Arr (getBFstLSorted')
import Data.Bits (shiftR)
import Data.Char (isAlpha, isDigit, toLower)
import Data.IntermediateStructures1 (mapI)
import Data.List
import GHC.Arr
import GHC.Base
import GHC.Num ((+))

{- | Is used to convert a Ukrainian text into list of 'String' each of which is ready to be
used by the functions of the modules for the phonetic languages approach.
It applies minimal grammar links and connections between the most commonly used Ukrainian
words that \"should\" be paired and not dealt with separately
to avoid the misinterpretation and preserve maximum of the semantics for the
\"phonetic\" language on the Ukrainian basis.
-}
prepareText :: String -> [String]
prepareText = prepareTextN 7
{-# INLINE prepareText #-}

{- | Concatenates complex words in Ukrainian so that they are not separated further by possible words order rearrangements (because they are treated
as a single word). This is needed to preserve basic grammar in phonetic languages.
-}
complexWords :: [String] -> [String]
complexWords wwss@(xs : ys : zs : ts : xss) =
    getBFstLSorted'
        (xs : complexWords (ys : zs : ts : xss))
        ( [
              ( "\1074"
              , if ys
                    == "\1084\1110\1088\1091"
                    && zs
                    == "\1090\1086\1075\1086"
                    && ts
                    == "\1103\1082"
                    then auxiliary2Inner (xs `mappend` ys `mappend` zs `mappend` ts) xss
                    else auxiliary4 wwss
              )
          ,
              ( "\1076\1072\1088\1084\1072"
              , if ys == "\1097\1086"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1076\1083\1103"
              , if ys == "\1090\1086\1075\1086" && zs == "\1097\1086\1073"
                    then auxiliary2Inner (xs `mappend` ys `mappend` zs `mappend` ts) xss
                    else auxiliary4 wwss
              )
          ,
              ( "\1079"
              , case ys of
                    "\1090\1080\1084" ->
                        if zs == "\1097\1086\1073"
                            then auxiliary2Inner (xs `mappend` ys `mappend` zs) (ts : xss)
                            else auxiliary4 wwss
                    "\1090\1086\1075\1086" ->
                        if zs == "\1095\1072\1089\1091" && ts == "\1103\1082"
                            then auxiliary2Inner (xs `mappend` ys `mappend` zs `mappend` "\1081\1072\1082") xss
                            else auxiliary4 wwss
                    _ -> auxiliary4 wwss
              )
          ,
              ( "\1079\1072\1083\1077\1078\1085\1086"
              , if ys == "\1074\1110\1076" then auxiliary3 wwss else auxiliary4 wwss
              )
          ,
              ( "\1079\1072\1084\1110\1089\1090\1100"
              , if ys == "\1090\1086\1075\1086" && zs == "\1097\1086\1073"
                    then auxiliary2Inner (xs `mappend` ys `mappend` zs) (ts : xss)
                    else auxiliary4 wwss
              )
          ,
              ( "\1087\1086\1087\1088\1080"
              , if ys == "\1090\1077" && zs == "\1097\1086"
                    then auxiliary2Inner (xs `mappend` ys `mappend` zs) (ts : xss)
                    else auxiliary4 wwss
              )
          ,
              ( "\1085\1077\1079\1072\1083\1077\1078\1085\1086"
              , if ys == "\1074\1110\1076" then auxiliary3 wwss else auxiliary4 wwss
              )
          ,
              ( "\1085\1077\1079\1074\1072\1078\1072\1102\1095\1080"
              , if ys == "\1085\1072"
                    then
                        if zs == "\1090\1077" && ts == "\1097\1086"
                            then auxiliary2Inner (xs `mappend` ys `mappend` zs `mappend` ts) xss
                            else auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1087\1088\1080"
              , if ys == "\1094\1100\1086\1084\1091"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1087\1110\1089\1083\1103"
              , if ys == "\1090\1086\1075\1086" && zs == "\1103\1082"
                    then auxiliary2Inner (xs `mappend` ys `mappend` zs) (ts : xss)
                    else auxiliary4 wwss
              )
          ,
              ( "\1090\1072\1082"
              , if (ys == "\1097\1086") || (ys == "\1103\1082")
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1090\1080\1084\1095\1072\1089\1086\1084"
              , if ys == "\1103\1082"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1090\1086\1084\1091"
              , if ys == "\1103\1082"
                    then auxiliary2Inner (xs `mappend` "\1081\1072\1082") (zs : ts : xss)
                    else auxiliary4 wwss
              )
          ,
              ( "\1091"
              , if ( ys
                        == "\1079\1074'\1103\1079\1082\1091"
                        || ys
                        == "\1079\1074\x02BC\1103\1079\1082\1091"
                   )
                    && zs
                    == "\1079"
                    then
                        auxiliary2Inner
                            (xs `mappend` "\1079\1074\1081\1072\1079\1082\1091" `mappend` zs)
                            (ts : xss)
                    else
                        if ys
                            == "\1084\1110\1088\1091"
                            && zs
                            == "\1090\1086\1075\1086"
                            && ts
                            == "\1103\1082"
                            then auxiliary2Inner (xs `mappend` ys `mappend` zs `mappend` ts) xss
                            else auxiliary4 wwss
              )
          ,
              ( "\1093\1086\1095"
              , if ys == "\1073\1080"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1093\1086\1095\1072"
              , if ys == "\1073"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1095\1077\1088\1077\1079"
              , if ys == "\1090\1077" && zs == "\1097\1086"
                    then auxiliary2Inner (xs `mappend` ys `mappend` zs) (ts : xss)
                    else auxiliary4 wwss
              )
          ]
        )
        xs
complexWords wwss@(xs : ys : zs : xss) =
    getBFstLSorted'
        (xs : complexWords [ys, zs])
        ( [
              ( "\1076\1072\1088\1084\1072"
              , if ys == "\1097\1086"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1079\1072\1083\1077\1078\1085\1086"
              , if ys == "\1074\1110\1076" then auxiliary3 wwss else auxiliary4 wwss
              )
          ,
              ( "\1085\1077\1079\1072\1083\1077\1078\1085\1086"
              , if ys == "\1074\1110\1076" then auxiliary3 wwss else auxiliary4 wwss
              )
          ,
              ( "\1087\1088\1080"
              , if ys == "\1094\1100\1086\1084\1091"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1090\1072\1082"
              , if ys == "\1097\1086" || ys == "\1103\1082"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1090\1080\1084\1095\1072\1089\1086\1084"
              , if ys == "\1103\1082"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1090\1086\1084\1091"
              , if ys == "\1103\1082"
                    then auxiliary2Inner (xs `mappend` "\1081\1072\1082") (zs : xss)
                    else auxiliary4 wwss
              )
          ,
              ( "\1093\1086\1095"
              , if ys == "\1073\1080"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ,
              ( "\1093\1086\1095\1072"
              , if ys == "\1073"
                    then auxiliary3 wwss
                    else auxiliary4 wwss
              )
          ]
        )
        xs
complexWords [xs, ys]
    | ys `elem` ["\1073", "\1073\1060", "\1078\1077", "\1078"] = [xs `mappend` ys]
    | xs == "\1076\1072\1088\1084\1072" && ys == "\1097\1086" = [xs `mappend` ys]
    | otherwise = [xs, ys]
complexWords xss = xss

auxiliary2Inner :: String -> [String] -> [String]
auxiliary2Inner ts (xs : ys : xss)
    | (concatenated2 . auxiliary1 $ [xs, ys]) /= [xs, ys] =
        let (rs, ws) = splitAt 1 (concatenated2 . auxiliary1 $ [xs, ys])
         in (ts `mappend` head rs) : complexWords (concat [ws, xss])
    | otherwise = (ts `mappend` xs) : complexWords (ys : xss)
auxiliary2Inner ts [xs] = [ts `mappend` xs]
auxiliary2Inner ts _ = [ts]

auxiliary3 :: [String] -> [String]
auxiliary3 (xs : ys : xss) = auxiliary2Inner (xs `mappend` ys) xss
auxiliary3 xss = xss
{-# INLINE auxiliary3 #-}

auxiliary4 :: [String] -> [String]
auxiliary4 (xs : xss) = auxiliary2Inner xs xss
auxiliary4 xss = xss
{-# INLINE auxiliary4 #-}

-- | Since 0.2.1.0 version the function is recursive and is applied so that all returned elements ('String') are no longer than 7 words in them.
splitLines :: [String] -> [String]
splitLines = splitLinesN 7
{-# INLINE splitLines #-}

-- | A generalized variant of the 'splitLines' with the arbitrary maximum number of the words in the lines given as the first argument.
splitLinesN :: Int -> [String] -> [String]
splitLinesN n xss
    | null xss || n <= 0 = []
    | otherwise =
        mapI
            (\xs -> compare (length . words $ xs) n == GT)
            ( \xs ->
                let yss = words xs
                 in splitLines . map unwords . (\(q, r) -> [q, r]) . splitAt (shiftR (length yss) 1) $
                        yss
            )
            $ xss

-- | A generalized variant of the 'prepareText' with the arbitrary maximum number of the words in the lines given as the first argument.
prepareTextN :: Int -> String -> [String]
prepareTextN = prepareTextNG (\t -> isAlpha t || isSpC t)
{-# INLINE prepareTextN #-}

-- | A generalized variant of the 'prepareText' with the arbitrary maximum number of the words in the lines given as the first argument. The \'_\' is not filtered out.
prepareTextN2 :: Int -> String -> [String]
prepareTextN2 = prepareTextNG (\t -> isAlpha t || isSpC t || t == '_' || isDigit t)
{-# INLINE prepareTextN2 #-}

-- | A generalized variant of the 'prepareText' with the arbitrary maximum number of the words in the lines given as the first argument. Both \'_\' and \'=\' are not filtered out.
prepareTextN3 :: Int -> String -> [String]
prepareTextN3 =
    prepareTextNG (\t -> isAlpha t || isSpC t || t == '_' || t == '=' || isDigit t)
{-# INLINE prepareTextN3 #-}

-- | An even more generalized variant of the 'prepareTextN' with the arbitrary maximum number of the words in the lines given as the second argument and the possibility to provide custom function for filtering.
prepareTextNG ::
    -- | A predicate to filter the symbols during preparation.
    (Char -> Bool) ->
    Int ->
    String ->
    [String]
prepareTextNG f n =
    filter (any isUkrainianL)
        . splitLinesN n
        . map
            ( unwords
                . concatenated2
                . participleConc
                . auxiliary1
                . complexWords
                . words
                . filter f
            )
        . filter (not . null)
        . lines
{-# INLINE prepareTextNG #-}

participleConc :: [String] -> [String]
participleConc (xs : ys : zs : yss)
    | isParticipleAppended ys =
        if isParticipleAppended zs
            then (xs `mappend` ys `mappend` zs) : participleConc yss
            else participleConc (xs `mappend` ys : zs : yss)
    | otherwise = xs : participleConc (ys : zs : yss)
participleConc (xs : ys : yss)
    | isParticipleAppended ys = (xs `mappend` ys) : participleConc yss
    | otherwise = xs : participleConc (ys : yss)
participleConc yss = yss

isParticipleAppended :: String -> Bool
isParticipleAppended xs = xs `elem` ["\1073", "\1073\1080", "\1078\1077", "\1078"]

isPrepended :: String -> Bool
isPrepended xs
    | isParticipleAppended xs = True
    | isPreposition xs = True
    | otherwise = isConcatenated xs

jottedCnv :: String -> String
jottedCnv xs = drop 1 . jottedConv $ ' ' : xs

auxiliary1 :: [String] -> [String]
auxiliary1 yss@(xs : zs : xss)
    | isParticipleAppended xs = xs : auxiliary1 (zs : xss)
    | isPreposition xs || isConcatenated xs =
        auxiliary1 (concatMap jottedCnv (tss `mappend` wss) : vss)
    | otherwise = xs : auxiliary1 (zs : xss)
  where
    (tss, uss) = span isPrepended yss
    (wss, vss) = splitAt 1 uss
auxiliary1 xss = xss

isPreposition :: String -> Bool
isPreposition ts =
    getBFstLSorted'
        False
        ( zip
            [ "\1030\1079"
            , "\1041\1077\1079"
            , "\1041\1110\1083\1103"
            , "\1042"
            , "\1042\1110\1076"
            , "\1044\1083\1103"
            , "\1044\1086"
            , "\1047"
            , "\1047\1072"
            , "\1047\1072\1088\1072\1076\1080"
            , "\1047\1110"
            , "\1050"
            , "\1050\1086\1083\1086"
            , "\1050\1088\1110\1079\1100"
            , "\1050\1088\1110\1084"
            , "\1052\1077\1078"
            , "\1052\1077\1078\1080"
            , "\1052\1110\1078"
            , "\1053\1072"
            , "\1053\1072\1076"
            , "\1054"
            , "\1054\1073"
            , "\1054\1076"
            , "\1054\1082\1088\1110\1084"
            , "\1055\1077\1088\1077\1076"
            , "\1055\1086"
            , "\1055\1088\1080"
            , "\1055\1088\1086"
            , "\1055\1088\1086\1090\1080"
            , "\1055\1110\1076"
            , "\1055\1110\1089\1083\1103"
            , "\1057\1077\1088\1077\1076"
            , "\1057\1077\1088\1077\1076\1080"
            , "\1059"
            , "\1063\1077\1088\1077\1079"
            , "\1073\1077\1079"
            , "\1073\1110\1083\1103"
            , "\1074"
            , "\1074\1110\1076"
            , "\1076\1083\1103"
            , "\1076\1086"
            , "\1079"
            , "\1079\1072"
            , "\1079\1072\1088\1072\1076\1080"
            , "\1079\1110"
            , "\1082"
            , "\1082\1086\1083\1086"
            , "\1082\1088\1110\1079\1100"
            , "\1082\1088\1110\1084"
            , "\1084\1077\1078"
            , "\1084\1077\1078\1080"
            , "\1084\1110\1078"
            , "\1085\1072"
            , "\1085\1072\1076"
            , "\1086"
            , "\1086\1073"
            , "\1086\1076"
            , "\1086\1082\1088\1110\1084"
            , "\1087\1077\1088\1077\1076"
            , "\1087\1086"
            , "\1087\1088\1080"
            , "\1087\1088\1086"
            , "\1087\1088\1086\1090\1080"
            , "\1087\1110\1076"
            , "\1087\1110\1089\1083\1103"
            , "\1089\1077\1088\1077\1076"
            , "\1089\1077\1088\1077\1076\1080"
            , "\1091"
            , "\1095\1077\1088\1077\1079"
            , "\1110\1079"
            ]
            $ replicate 200 True
        )
        ts
{-# INLINE isPreposition #-}

{- | Since the dobutokO-poetry version 0.16.3.0 the (||) operator has been changed to the (&&).
The idea is that these words are the ones that are pronouns and they \"should\" be treated
(by the author's understanding) as independent words.
-}
isConcatenated :: String -> Bool
isConcatenated ts
    | null ts = False
    | otherwise =
        compare (length ts) 2
            /= GT
            && getBFstLSorted'
                True
                ( zip
                    [ "\1028"
                    , "\1042\1080"
                    , "\1052\1080"
                    , "\1058\1080"
                    , "\1058\1110"
                    , "\1062\1110"
                    , "\1071"
                    , "\1074\1080"
                    , "\1084\1080"
                    , "\1090\1080"
                    , "\1090\1110"
                    , "\1094\1110"
                    , "\1103"
                    , "\1108"
                    ]
                    $ replicate 14 False
                )
                ts
            && (head ts `notElem` "\1031\1111")
{-# INLINE isConcatenated #-}

concatenated2 :: [String] -> [String]
concatenated2 (xs : ys : xss) =
    getBFstLSorted'
        (xs : concatenated2 (ys : xss))
        ( zip
            [ "\1040\1073\1086"
            , "\1040\1076\1078\1077"
            , "\1040\1083\1077"
            , "\1040\1085\1110\1078"
            , "\1041\1086\1076\1072\1081"
            , "\1041\1091\1094\1110\1084\1090\1086"
            , "\1042\1078\1077"
            , "\1042\1080\1082\1083\1102\1095\1085\1086"
            , "\1042\1083\1072\1089\1085\1077"
            , "\1042\1090\1110\1084"
            , "\1044\1072\1074\1072\1081"
            , "\1047\1072\1090\1077"
            , "\1050\1086\1083\1080"
            , "\1051\1077\1076\1074\1077"
            , "\1051\1080\1096\1077"
            , "\1052\1072\1081\1078\1077"
            , "\1052\1086\1074"
            , "\1052\1086\1074\1073\1080"
            , "\1052\1086\1074\1073\1080\1090\1086"
            , "\1053\1072\1074\1110\1090\1100"
            , "\1053\1072\1089\1082\1110\1083\1100\1082\1080"
            , "\1053\1072\1095\1077"
            , "\1053\1072\1095\1077\1073"
            , "\1053\1072\1095\1077\1073\1090\1086"
            , "\1053\1077\1074\1078\1077"
            , "\1053\1077\1084\1086\1074"
            , "\1053\1077\1084\1086\1074\1073\1080"
            , "\1053\1077\1084\1086\1074\1073\1080\1090\1086"
            , "\1053\1077\1085\1072\1095\1077"
            , "\1053\1077\1085\1072\1095\1077\1073\1090\1086"
            , "\1053\1077\1093\1072\1081"
            , "\1053\1090\1078\1077"
            , "\1053\1110\1073\1080"
            , "\1053\1110\1073\1080\1090\1086"
            , "\1053\1110\1078"
            , "\1054\1090\1086\1078"
            , "\1055\1088\1080\1090\1086\1084\1091"
            , "\1055\1088\1080\1090\1110\1084"
            , "\1055\1088\1080\1095\1086\1084\1091"
            , "\1055\1088\1080\1095\1110\1084"
            , "\1055\1088\1086\1090\1077"
            , "\1057\1072\1084\1077"
            , "\1057\1077\1073\1090\1086"
            , "\1058\1072\1082\1080"
            , "\1058\1086\1073\1090\1086"
            , "\1058\1110\1083\1100\1082\1080"
            , "\1061\1072\1081"
            , "\1061\1086\1095"
            , "\1061\1110\1073\1072"
            , "\1062\1077\1073\1090\1086"
            , "\1065\1086\1073"
            , "\1071\1082\1073\1080"
            , "\1071\1082\1088\1072\1079"
            , "\1071\1082\1097\1086"
            , "\1072\1073\1086"
            , "\1072\1076\1078\1077"
            , "\1072\1083\1077"
            , "\1072\1085\1110\1078"
            , "\1073\1086\1076\1072\1081"
            , "\1073\1091\1094\1110\1084\1090\1086"
            , "\1074\1078\1077"
            , "\1074\1080\1082\1083\1102\1095\1085\1086"
            , "\1074\1083\1072\1089\1085\1077"
            , "\1074\1090\1110\1084"
            , "\1076\1072\1074\1072\1081"
            , "\1079\1072\1090\1077"
            , "\1082\1086\1083\1080"
            , "\1083\1077\1076\1074\1077"
            , "\1083\1080\1096\1077"
            , "\1084\1072\1081\1078\1077"
            , "\1084\1086\1074"
            , "\1084\1086\1074\1073\1080"
            , "\1084\1086\1074\1073\1080\1090\1086"
            , "\1085\1072\1074\1110\1090\1100"
            , "\1085\1072\1089\1082\1110\1083\1100\1082\1080"
            , "\1085\1072\1095\1077"
            , "\1085\1072\1095\1077\1073"
            , "\1085\1072\1095\1077\1073\1090\1086"
            , "\1085\1077\1074\1078\1077"
            , "\1085\1077\1084\1086\1074"
            , "\1085\1077\1084\1086\1074\1073\1080"
            , "\1085\1077\1084\1086\1074\1073\1080\1090\1086"
            , "\1085\1077\1085\1072\1095\1077"
            , "\1085\1077\1085\1072\1095\1077\1073\1090\1086"
            , "\1085\1077\1093\1072\1081"
            , "\1085\1110\1073\1080"
            , "\1085\1110\1073\1080\1090\1086"
            , "\1085\1110\1078"
            , "\1086\1090\1078\1077"
            , "\1086\1090\1086\1078"
            , "\1087\1088\1080\1090\1086\1084\1091"
            , "\1087\1088\1080\1090\1110\1084"
            , "\1087\1088\1080\1095\1086\1084\1091"
            , "\1087\1088\1080\1095\1110\1084"
            , "\1087\1088\1086\1090\1077"
            , "\1089\1072\1084\1077"
            , "\1089\1077\1073\1090\1086"
            , "\1090\1072\1082\1080"
            , "\1090\1086\1073\1090\1086"
            , "\1090\1110\1083\1100\1082\1080"
            , "\1093\1072\1081"
            , "\1093\1086\1095"
            , "\1093\1110\1073\1072"
            , "\1094\1077\1073\1090\1086"
            , "\1097\1086\1073"
            , "\1103\1082\1073\1080"
            , "\1103\1082\1088\1072\1079"
            , "\1103\1082\1097\1086"
            ]
            $ replicate 200 ((xs `mappend` jottedCnv ys) : concatenated2 xss)
        )
        xs
concatenated2 xss = xss

isSpC :: Char -> Bool
isSpC x = x == '\'' || x == ' ' || x == '\x2019' || x == '\x02BC' || x == '-'
{-# INLINE isSpC #-}

jottedConv :: String -> String
jottedConv (x : y : xs)
    | isSpC x =
        x
            : ( getBFstLSorted'
                    (jottedConv (y : xs))
                    [ ('\1028', '\1049' : '\1077' : jottedConv xs)
                    , ('\1031', '\1049' : '\1110' : jottedConv xs)
                    , ('\1070', '\1049' : '\1091' : jottedConv xs)
                    , ('\1071', '\1049' : '\1072' : jottedConv xs)
                    , ('\1102', '\1081' : '\1091' : jottedConv xs)
                    , ('\1103', '\1081' : '\1072' : jottedConv xs)
                    , ('\1108', '\1081' : '\1077' : jottedConv xs)
                    , ('\1111', '\1081' : '\1110' : jottedConv xs)
                    ]
                    y
              )
    | otherwise = x : jottedConv (y : xs)
jottedConv xs = xs

{- | Can be used to prepare the text after 'convertToProperUkrainian' from 'Melodics.Ukrainian' module from @mmsyn6ukr@ package so that all the 'String' can
be represented as unique 'Char'.
-}
aux4 :: String -> Char
aux4 xs
    | xs == "\1076\1078" = 'j'
    | xs == "\1076\1079" = 'z'
    | xs == "\1089\1100" = 's'
    | xs == "\1094\1100" = 'c'
    | null xs = error "Aftovolio.Ukrainian.PrepareText.aux4: Empty String. "
    | otherwise = head xs

-- | Is taken from the @mmsyn6ukr@ package version 0.8.1.0 so that the amount of dependencies are reduced (and was slightly modified).
isUkrainianL :: Char -> Bool
isUkrainianL y
    | (y >= '\1040' && y <= '\1065') || (y >= '\1070' && y <= '\1097') = True
    | otherwise =
        getBFstLSorted'
            False
            ( map (\x -> (x, True)) $
                "\1028\1030\1031\1068\1100\1102\1103\1108\1110\1111\1168\1169\8217"
            )
            y

-------------------------------------

{- | @ since 0.2.0.0
Given a positive number and a list tries to rearrange the list's 'String's by concatenation of the several elements of the list
so that the number of words in every new 'String' in the resulting list is not greater than the 'Int' argument. If some of the
'String's have more than that number quantity of the words then these 'String's are preserved.
-}
growLinesN :: Int -> [String] -> [String]
growLinesN n xss
    | null xss || n < 0 = []
    | otherwise = unwords yss : growLinesN n zss
  where
    l = length . takeWhile (<= n) . scanl1 (+) . map (length . words) $ xss -- the maximum number of lines to be taken
    (yss, zss) = splitAt (max l 1) xss

{- | @ since 0.2.0.0
The function combines the 'prepareTextN' and 'growLinesN' function. Applies needed phonetic language preparations
to the Ukrainian text and tries to \'grow\' the resulting 'String's in the list so that the number of the words in every
of them is no greater than the given first 'Int' number.
-}
prepareGrowTextMN ::
    -- | A maximum number of the words or their concatenations in the resulting list of 'String's.
    Int ->
    -- | A number of words in every 'String' that the function firstly forms. To have some sense of usage, must be less than the first argument.
    Int ->
    String ->
    [String]
prepareGrowTextMN m n = growLinesN m . prepareTextN n
{-# INLINE prepareGrowTextMN #-}

{- | @ since 0.11.0.0
The generalized version of the 'prepareGrowTextMN' with additional possibility to provide custom function for symbols filtering inside.
-}
prepareGrowTextMNG ::
    -- | A predicate to filter the symbols during preparation.
    (Char -> Bool) ->
    -- | A maximum number of the words or their concatenations in the resulting list of 'String's.
    Int ->
    -- | A number of words in every 'String' that the function firstly forms. To have some sense of usage, must be less than the first argument.
    Int ->
    String ->
    [String]
prepareGrowTextMNG f m n = growLinesN m . prepareTextNG f n
{-# INLINE prepareGrowTextMNG #-}

-------------------------------------

{- | @ since 0.6.0.0
Recursively splits the concatenated list of lines of words so that in every resulting 'String' in the list
except the last one there is just 'Int' -- the first argument -- words.
-}
tuneLinesN :: Int -> [String] -> [String]
tuneLinesN n xss
    | null xss || n < 0 = []
    | otherwise =
        let wss = words . unwords $ xss
            (yss, zss) = splitAt n wss
         in unwords yss : tuneLinesN n zss

{- | @ since 0.6.0.0
The function combines the 'prepareTextN' and 'tuneLinesN' functions. Applies needed phonetic language preparations
to the Ukrainian text and splits the list of 'String's so that the number of the words in each of them (except the last one)
is equal the given first 'Int' number.
-}
prepareTuneTextMN ::
    -- | A number of the words or their concatenations in the resulting list of 'String's (except probably the last one).
    Int ->
    -- | A number of words in every 'String' that the function firstly forms. To have some sense of usage, must be less than the first argument.
    Int ->
    String ->
    [String]
prepareTuneTextMN m n = tuneLinesN m . prepareTextN n
{-# INLINE prepareTuneTextMN #-}

{- | @ since 0.11.0.0
The generalized version of the 'prepareTuneTextMN' with additional possibility to provide custom function for symbols filtering inside.
-}
prepareTuneTextMNG ::
    -- | A predicate to filter the symbols during preparation.
    (Char -> Bool) ->
    -- | A number of the words or their concatenations in the resulting list of 'String's (except probably the last one).
    Int ->
    -- | A number of words in every 'String' that the function firstly forms. To have some sense of usage, must be less than the first argument.
    Int ->
    String ->
    [String]
prepareTuneTextMNG f m n = tuneLinesN m . prepareTextNG f n
{-# INLINE prepareTuneTextMNG #-}
