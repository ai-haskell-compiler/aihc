{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -threaded -rtsopts #-}
{-# OPTIONS_HADDOCK -show-extensions #-}

{- |
Module      :  Aftovolio.Ukrainian.ReverseConcatenations
Copyright   :  (c) OleksandrZhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Contains several functions that helps to reverse many of the phonetic languages approach concatenations
for the Ukrainian language.
-}
module Aftovolio.Ukrainian.ReverseConcatenations where

import CaseBi.Arr (getBFstLSorted')
import Data.IntermediateStructures1 (mapI)
import Data.List
import Data.Tuple (snd)
import GHC.Base

{- | Reverses many phonetic languages approach related concatenations for the Ukrainian text. Is intended to be used
with the text on several lines.
-}
reverseConcatenations :: Int -> String -> String
reverseConcatenations n xs
    | null xs = []
    | otherwise =
        unlines . map (unwords . mapI (const True) (reverseConcat1 n) . words) . lines $
            xs
  where
    yss = lines xs
    zsss = map words yss

{- | Reverses many phonetic languages approach related concatenations for just one Ukrainian word. Is used
internally in the 'reverseConcatenations'.
-}
reverseConcat1 :: Int -> String -> [String]
reverseConcat1 n xs
    | null xs = []
    | otherwise =
        getBFstLSorted'
            (if n == 2 then reverseConcat2 n xs ts us else [xs])
            [ howConcat1WordEntirely n "\1040\1073\1086" us
            , ("\1040\1076\1078", howConcat1Word n 1 us "\1077" "\1040\1076\1078\1077" xs)
            , howConcat1WordEntirely n "\1040\1083\1077" us
            , ("\1040\1085\1110", howConcat1Word n 1 us "\1078" "\1040\1085\1110\1078" xs)
            ,
                ( "\1041\1086\1076"
                , howConcat1Word n 2 us "\1072\1081" "\1041\1086\1076\1072\1081" xs
                )
            ,
                ( "\1041\1091\1094"
                , howConcat1Word
                    n
                    4
                    us
                    "\1110\1084\1090\1086"
                    "\1041\1091\1094\1110\1084\1090\1086"
                    xs
                )
            , howConcat1WordEntirely n "\1042\1078\1077" us
            ,
                ( "\1042\1083\1072"
                , howConcat1Word n 3 us "\1089\1085\1077" "\1042\1083\1072\1089\1085\1077" xs
                )
            , ("\1042\1090\1110", howConcat1Word n 1 us "\1084" "\1042\1090\1110\1084" xs)
            , howConcat1WordEntirely n "\1044\1083\1103" us
            ,
                ( "\1047\1072\1088"
                , howConcat1Word n 3 us "\1072\1076\1080" "\1047\1072\1088\1072\1076\1080" xs
                )
            ,
                ( "\1050\1088\1110"
                , if take 2 us == "\1079\1100"
                    then "\1050\1088\1110\1079\1100" : reverseConcat1 n (drop 2 us)
                    else
                        if take 1 us == "\1084"
                            then "\1050\1088\1110\1084" : reverseConcat1 n (drop 1 us)
                            else [xs]
                )
            ,
                ( "\1053\1110\1073"
                , if take 3 us == "\1080\1090\1086"
                    then
                        "\1053\1110\1073\1080\1090\1086"
                            : reverseConcat1 n (drop 3 us)
                    else
                        if take 1 us == "\1080"
                            then "\1053\1110\1073\1080" : reverseConcat1 n (drop 1 us)
                            else [xs]
                )
            ,
                ( "\1054\1082\1088"
                , howConcat1Word n 2 us "\1110\1084" "\1054\1082\1088\1110\1084" xs
                )
            , ("\1054\1090\1078", howConcat1Word n 1 us "\1077" "\1054\1090\1078\1077" xs)
            , ("\1054\1090\1086", howConcat1Word n 1 us "\1078" "\1054\1090\1086\1078" xs)
            ,
                ( "\1055\1088\1080"
                , if take 4 us == "\1090\1086\1084\1091"
                    then
                        "\1055\1088\1080\1090\1086\1084\1091"
                            : reverseConcat1 n (drop 4 us)
                    else
                        if take 3 us == "\1090\1110\1084"
                            then "\1055\1088\1080\1090\1110\1084" : reverseConcat1 n (drop 3 us)
                            else
                                if take 4 us == "\1095\1086\1084\1091"
                                    then "\1055\1088\1080\1095\1086\1084\1091" : reverseConcat1 n (drop 4 us)
                                    else
                                        if take 3 us == "\1095\1110\1084"
                                            then "\1055\1088\1080\1095\1110\1084" : reverseConcat1 n (drop 3 us)
                                            else [xs]
                )
            , ("\1057\1072\1084", howConcat1Word n 1 us "\1077" "\1057\1072\1084\1077" xs)
            ,
                ( "\1057\1077\1073"
                , howConcat1Word n 2 us "\1090\1086" "\1057\1077\1073\1090\1086" xs
                )
            , ("\1058\1072\1082", howConcat1Word n 1 us "\1080" "\1058\1072\1082\1080" xs)
            ,
                ( "\1058\1086\1073"
                , howConcat1Word n 2 us "\1090\1086" "\1058\1086\1073\1090\1086" xs
                )
            , howConcat1WordEntirely n "\1061\1072\1081" us
            , ("\1061\1110\1073", howConcat1Word n 1 us "\1072" "\1061\1110\1073\1072" xs)
            ,
                ( "\1062\1077\1073"
                , howConcat1Word n 2 us "\1090\1086" "\1062\1077\1073\1090\1086" xs
                )
            , howConcat1WordEntirely n "\1065\1086\1073" us
            , ("\1071\1082\1073", howConcat1Word n 1 us "\1080" "\1071\1082\1073\1080" xs)
            ,
                ( "\1071\1082\1088"
                , howConcat1Word n 2 us "\1072\1079" "\1071\1082\1088\1072\1079" xs
                )
            , ("\1071\1082\1097", howConcat1Word n 1 us "\1086" "\1071\1082\1097\1086" xs)
            , howConcat1WordEntirely n "\1072\1073\1086" us
            ,
                ( "\1072\1076\1078"
                , howConcat1Word n 1 us "\1077" "\1072\1076\1078\1077" xs
                )
            , howConcat1WordEntirely n "\1072\1083\1077" us
            , ("\1072\1085\1110", howConcat1Word n 1 us "\1078" "\1072\1085\1110\1078" xs)
            ,
                ( "\1073\1086\1076"
                , howConcat1Word n 2 us "\1072\1081" "\1073\1086\1076\1072\1081" xs
                )
            ,
                ( "\1073\1091\1094"
                , howConcat1Word
                    n
                    4
                    us
                    "\1110\1084\1090\1086"
                    "\1073\1091\1094\1110\1084\1090\1086"
                    xs
                )
            , howConcat1WordEntirely n "\1074\1078\1077" us
            ,
                ( "\1074\1080\1082"
                , howConcat1Word
                    n
                    5
                    us
                    "\1083\1102\1095\1085\1086"
                    "\1074\1080\1082\1083\1102\1095\1085\1086"
                    xs
                )
            ,
                ( "\1074\1083\1072"
                , howConcat1Word n 3 us "\1089\1085\1077" "\1074\1083\1072\1089\1085\1077" xs
                )
            ,
                ( "\1074\1084\1110"
                , if take 8 us == "\1088\1091\1090\1086\1075\1086\1103\1082"
                    then
                        "\1074"
                            : "\1084\1110\1088\1091"
                            : "\1090\1086\1075\1086"
                            : "\1103\1082"
                            : reverseConcat1 n (drop 11 xs)
                    else [xs]
                )
            , ("\1074\1090\1110", howConcat1Word n 1 us "\1084" "\1074\1090\1110\1084" xs)
            ,
                ( "\1076\1072\1074"
                , howConcat1Word n 2 us "\1072\1081" "\1076\1072\1074\1072\1081" xs
                )
            ,
                ( "\1076\1072\1088"
                , howConcat1Word
                    n
                    4
                    us
                    "\1084\1072\1097\1086"
                    "\1076\1072\1088\1084\1072\1097\1086"
                    xs
                )
            ,
                ( "\1076\1083\1103"
                , if take 7 us == "\1090\1086\1075\1086\1097\1086\1073"
                    then
                        "\1076\1083\1103"
                            : "\1090\1086\1075\1086"
                            : "\1097\1086\1073"
                            : reverseConcat1 n (drop 7 xs)
                    else [xs]
                )
            ,
                ( "\1079\1072\1083"
                , if take 7 us == "\1077\1078\1085\1086\1074\1110\1076"
                    then
                        "\1079\1072\1083\1077\1078\1085\1086"
                            : "\1074\1110\1076"
                            : reverseConcat1 n (drop 7 xs)
                    else [xs]
                )
            ,
                ( "\1079\1072\1084"
                , if take 11 us == "\1110\1089\1090\1100\1090\1086\1075\1086\1097\1086\1073"
                    then
                        "\1079\1072\1084\1110\1089\1090\1100"
                            : "\1090\1086\1075\1086"
                            : "\1097\1086\1073"
                            : reverseConcat1 n (drop 11 xs)
                    else [xs]
                )
            ,
                ( "\1079\1072\1088"
                , howConcat1Word n 3 us "\1072\1076\1080" "\1079\1072\1088\1072\1076\1080" xs
                )
            , ("\1079\1072\1090", howConcat1Word n 1 us "\1077" "\1079\1072\1090\1077" xs)
            ,
                ( "\1079\1090\1080"
                , if take 4 us == "\1084\1097\1086\1073"
                    then
                        "\1079"
                            : "\1090\1080\1084"
                            : "\1097\1086\1073"
                            : reverseConcat1 n (drop 4 xs)
                    else [xs]
                )
            ,
                ( "\1079\1090\1086"
                , if take 8 us == "\1075\1086\1095\1072\1089\1091\1103\1082"
                    then
                        "\1079"
                            : "\1090\1086\1075\1086"
                            : "\1095\1072\1089\1091"
                            : "\1103\1082"
                            : reverseConcat1 n (drop 8 xs)
                    else [xs]
                )
            , ("\1082\1086\1083", howConcat1Word n 1 us "\1080" "\1082\1086\1083\1080" xs)
            ,
                ( "\1082\1088\1110"
                , if take 2 us == "\1079\1100"
                    then "\1082\1088\1110\1079\1100" : reverseConcat1 n (drop 2 us)
                    else
                        if take 1 us == "\1084"
                            then "\1082\1088\1110\1084" : reverseConcat1 n (drop 1 us)
                            else [xs]
                )
            ,
                ( "\1083\1077\1076"
                , howConcat1Word n 2 us "\1074\1077" "\1083\1077\1076\1074\1077" xs
                )
            , ("\1083\1080\1096", howConcat1Word n 1 us "\1077" "\1083\1080\1096\1077" xs)
            ,
                ( "\1084\1072\1081"
                , howConcat1Word n 2 us "\1078\1077" "\1084\1072\1081\1078\1077" xs
                )
            ,
                ( "\1084\1086\1074"
                , if take 4 us == "\1073\1080\1090\1086"
                    then
                        "\1084\1086\1074\1073\1080\1090\1086"
                            : reverseConcat1 n (drop 4 us)
                    else
                        if take 2 us == "\1073\1080"
                            then "\1084\1086\1074\1073\1080" : reverseConcat1 n (drop 2 us)
                            else snd (howConcat1WordEntirely n "\1084\1086\1074" us)
                )
            ,
                ( "\1085\1072\1074"
                , howConcat1Word n 3 us "\1110\1090\1100" "\1085\1072\1074\1110\1090\1100" xs
                )
            ,
                ( "\1085\1072\1089"
                , howConcat1Word
                    n
                    6
                    us
                    "\1082\1110\1083\1100\1082\1080"
                    "\1085\1072\1089\1082\1110\1083\1100\1082\1080"
                    xs
                )
            ,
                ( "\1085\1072\1095"
                , if take 4 us == "\1077\1073\1090\1086"
                    then
                        "\1085\1072\1095\1077\1073\1090\1086"
                            : reverseConcat1 n (drop 4 us)
                    else
                        if take 2 us == "\1077\1073"
                            then "\1085\1072\1095\1077\1073" : reverseConcat1 n (drop 2 us)
                            else
                                if take 1 us == "\1077"
                                    then "\1085\1072\1095\1077" : reverseConcat1 n (drop 1 us)
                                    else [xs]
                )
            ,
                ( "\1085\1077\1074"
                , howConcat1Word n 2 us "\1078\1077" "\1085\1077\1074\1078\1077" xs
                )
            ,
                ( "\1085\1077\1079"
                , if take 9 us == "\1072\1083\1077\1078\1085\1086\1074\1110\1076"
                    then
                        "\1085\1077\1079\1072\1083\1077\1078\1085\1086\1074\1110\1076"
                            : reverseConcat1 n (drop 9 us)
                    else
                        if take 13 us
                            == "\1074\1072\1078\1072\1102\1095\1080\1085\1072\1090\1077\1097\1086"
                            then
                                "\1085\1077\1079\1074\1072\1078\1072\1102\1095\1080\1085\1072\1090\1077\1097\1086"
                                    : reverseConcat1 n (drop 13 us)
                            else [xs]
                )
            ,
                ( "\1085\1077\1084"
                , if take 6 us == "\1086\1074\1073\1080\1090\1086"
                    then "\1085\1077\1084\1086\1074\1073\1080\1090\1086" : reverseConcat1 n (drop 6 us)
                    else
                        if take 4 us == "\1086\1074\1073\1080"
                            then
                                "\1085\1077\1084\1086\1074\1073\1080"
                                    : reverseConcat1 n (drop 4 us)
                            else
                                if take 2 us == "\1086\1074"
                                    then "\1085\1077\1084\1086\1074" : reverseConcat1 n (drop 2 us)
                                    else [xs]
                )
            ,
                ( "\1085\1077\1085"
                , if take 6 us == "\1072\1095\1077\1073\1090\1086"
                    then "\1085\1077\1085\1072\1095\1077\1073\1090\1086" : reverseConcat1 n (drop 6 us)
                    else
                        if take 3 us == "\1072\1095\1077"
                            then
                                "\1085\1077\1085\1072\1095\1077"
                                    : reverseConcat1 n (drop 3 us)
                            else [xs]
                )
            ,
                ( "\1085\1077\1093"
                , howConcat1Word n 2 us "\1072\1081" "\1085\1077\1093\1072\1081" xs
                )
            ,
                ( "\1085\1110\1073"
                , if take 3 us == "\1080\1090\1086"
                    then
                        "\1085\1110\1073\1080\1090\1086"
                            : reverseConcat1 n (drop 3 us)
                    else
                        if take 1 us == "\1080"
                            then "\1085\1110\1073\1080" : reverseConcat1 n (drop 1 us)
                            else [xs]
                )
            , howConcat1WordEntirely n "\1085\1110\1078" us
            ,
                ( "\1086\1082\1088"
                , howConcat1Word n 2 us "\1110\1084" "\1086\1082\1088\1110\1084" xs
                )
            , ("\1086\1090\1078", howConcat1Word n 1 us "\1077" "\1086\1090\1078\1077" xs)
            , ("\1086\1090\1086", howConcat1Word n 1 us "\1078" "\1086\1090\1086\1078" xs)
            ,
                ( "\1087\1086\1087"
                , if take 6 us == "\1088\1080\1090\1077\1097\1086"
                    then
                        "\1087\1086\1087\1088\1080"
                            : "\1090\1077"
                            : "\1097\1086"
                            : reverseConcat1 n (drop 6 us)
                    else [xs]
                )
            ,
                ( "\1087\1088\1080"
                , if take 4 us == "\1090\1086\1084\1091"
                    then
                        "\1087\1088\1080\1090\1086\1084\1091"
                            : reverseConcat1 n (drop 4 us)
                    else
                        if take 3 us == "\1090\1110\1084"
                            then
                                "\1087\1088\1080\1090\1110\1084"
                                    : reverseConcat1 n (drop 3 us)
                            else
                                if take 5 us == "\1094\1100\1086\1084\1091"
                                    then
                                        "\1087\1088\1080\1094\1100\1086\1084\1091"
                                            : reverseConcat1 n (drop 5 us)
                                    else
                                        if take 4 us == "\1095\1086\1084\1091"
                                            then
                                                "\1087\1088\1080\1095\1086\1084\1091"
                                                    : reverseConcat1 n (drop 4 us)
                                            else
                                                if take 3 us == "\1095\1110\1084"
                                                    then "\1087\1088\1080\1095\1110\1084" : reverseConcat1 n (drop 3 us)
                                                    else [xs]
                )
            ,
                ( "\1087\1088\1086"
                , howConcat1Word n 2 us "\1090\1077" "\1087\1088\1086\1090\1077" xs
                )
            ,
                ( "\1087\1110\1089"
                , if take 8 us == "\1083\1103\1090\1086\1075\1086\1103\1082"
                    then
                        "\1087\1110\1089\1083\1103"
                            : "\1090\1086\1075\1086"
                            : "\1103\1082"
                            : reverseConcat1 n (drop 8 us)
                    else [xs]
                )
            , ("\1089\1072\1084", howConcat1Word n 1 us "\1077" "\1089\1072\1084\1077" xs)
            ,
                ( "\1089\1077\1073"
                , howConcat1Word n 2 us "\1090\1086" "\1089\1077\1073\1090\1086" xs
                )
            ,
                ( "\1090\1072\1082"
                , if take 1 us == "\1080"
                    then "\1090\1072\1082\1080" : reverseConcat1 n (drop 1 us)
                    else
                        if take 3 us == "\1081\1072\1082"
                            then "\1090\1072\1082" : "\1103\1082" : reverseConcat1 n (drop 3 us)
                            else
                                if take 2 us == "\1097\1086"
                                    then "\1090\1072\1082" : "\1097\1086" : reverseConcat1 n (drop 2 us)
                                    else [xs]
                )
            ,
                ( "\1090\1080\1084"
                , if take 8 us == "\1095\1072\1089\1086\1084\1081\1072\1082"
                    then
                        "\1090\1080\1084"
                            : "\1095\1072\1089\1086\1084"
                            : "\1103\1082"
                            : reverseConcat1 n (drop 8 us)
                    else [xs]
                )
            ,
                ( "\1090\1086\1073"
                , howConcat1Word n 2 us "\1090\1086" "\1090\1086\1073\1090\1086" xs
                )
            ,
                ( "\1090\1086\1084"
                , if take 3 us == "\1091\1097\1086"
                    then
                        "\1090\1086\1084\1091"
                            : "\1097\1086"
                            : reverseConcat1 n (drop 3 us)
                    else
                        if take 3 us == "\1091\1103\1082"
                            then
                                "\1090\1086\1084\1091"
                                    : "\1103\1082"
                                    : reverseConcat1 n (drop 3 us)
                            else [xs]
                )
            ,
                ( "\1090\1110\1083"
                , howConcat1Word n 3 us "\1100\1082\1080" "\1090\1110\1083\1100\1082\1080" xs
                )
            ,
                ( "\1091\1079\1074"
                , if take 6 us == "\1081\1072\1079\1082\1091\1079"
                    then
                        "\1091"
                            : "\1079\1074\x02BC\1103\1079\1082\1091"
                            : "\1079"
                            : reverseConcat1 n (drop 6 us)
                    else [xs]
                )
            ,
                ( "\1091\1084\1110"
                , if take 8 us == "\1088\1091\1090\1086\1075\1086\1103\1082"
                    then
                        "\1091"
                            : "\1084\1110\1088\1091"
                            : "\1090\1086\1075\1086"
                            : "\1103\1082"
                            : reverseConcat1 n (drop 8 us)
                    else [xs]
                )
            , howConcat1WordEntirely n "\1093\1072\1081" us
            ,
                ( "\1093\1086\1095"
                , if take 2 us == "\1073\1080"
                    then "\1093\1086\1095" : "\1073\1080" : reverseConcat1 n (drop 2 us)
                    else
                        if take 2 us == "\1072\1073"
                            then "\1093\1086\1095\1072" : "\1073" : reverseConcat1 n (drop 2 us)
                            else "\1093\1086\1095" : [us]
                )
            , ("\1093\1110\1073", howConcat1Word n 1 us "\1072" "\1093\1110\1073\1072" xs)
            ,
                ( "\1094\1077\1073"
                , howConcat1Word n 2 us "\1090\1086" "\1094\1077\1073\1090\1086" xs
                )
            ,
                ( "\1095\1077\1088"
                , if take 6 us == "\1077\1079\1090\1077\1097\1086"
                    then
                        "\1095\1077\1088\1077\1079"
                            : "\1090\1077"
                            : "\1097\1086"
                            : reverseConcat1 n (drop 6 us)
                    else [xs]
                )
            , howConcat1WordEntirely n "\1097\1086\1073" us
            ,
                ( "\1103\1082\1073"
                , howConcat1Word n 1 us "\1080" "\1103\1082\1073\1080" xs
                )
            ,
                ( "\1103\1082\1088"
                , howConcat1Word n 2 us "\1072\1079" "\1103\1082\1088\1072\1079" xs
                )
            , ("\1103\1082\1097", howConcat1Word n 1 us "\1086" "\1103\1082\1097\1086" xs)
            ]
            ts
  where
    (ts, us) = splitAt 3 xs

reverseConcat2 :: Int -> String -> String -> String -> [String]
reverseConcat2 n xs ts us =
    getBFstLSorted'
        [xs]
        [ ("\1041\1110\1083", howConcat1Word n 1 us "\1103" "\1041\1110\1083\1103" xs)
        ,
            ( "\1042\1080\1082"
            , howConcat1Word
                n
                5
                us
                "\1083\1102\1095\1085\1086"
                "\1042\1080\1082\1083\1102\1095\1085\1086"
                xs
            )
        ,
            ( "\1044\1072\1074"
            , howConcat1Word n 2 us "\1072\1081" "\1044\1072\1074\1072\1081" xs
            )
        , ("\1047\1072\1090", howConcat1Word n 1 us "\1077" "\1047\1072\1090\1077" xs)
        ,
            ( "\1050\1086\1083"
            , if take 1 us == "\1080"
                then "\1050\1086\1083\1080" : reverseConcat1 n (drop 1 us)
                else
                    if take 1 us == "\1086"
                        then "\1050\1086\1083\1086" : reverseConcat1 n (drop 1 us)
                        else [xs]
            )
        , ("\1051\1080\1096", howConcat1Word n 1 us "\1077" "\1051\1080\1096\1077" xs)
        , howConcat1WordEntirely n "\1053\1110\1078" us
        ,
            ( "\1055\1110\1089"
            , howConcat1Word n 2 us "\1083\1103" "\1055\1110\1089\1083\1103" xs
            )
        ,
            ( "\1057\1077\1088"
            , howConcat1Word n 2 us "\1077\1076" "\1057\1077\1088\1077\1076" xs
            )
        ,
            ( "\1058\1110\1083"
            , howConcat1Word n 3 us "\1100\1082\1080" "\1058\1110\1083\1100\1082\1080" xs
            )
        , ("\1073\1110\1083", howConcat1Word n 1 us "\1103" "\1073\1110\1083\1103" xs)
        ,
            ( "\1076\1072\1074"
            , howConcat1Word n 2 us "\1072\1081" "\1076\1072\1074\1072\1081" xs
            )
        , ("\1079\1072\1090", howConcat1Word n 1 us "\1077" "\1079\1072\1090\1077" xs)
        ,
            ( "\1082\1086\1083"
            , if take 1 us == "\1080"
                then "\1082\1086\1083\1080" : reverseConcat1 n xs
                else
                    if take 1 us == "\1086"
                        then "\1082\1086\1083\1086" : reverseConcat1 n xs
                        else [xs]
            )
        , ("\1083\1080\1096", howConcat1Word n 1 us "\1077" "\1083\1080\1096\1077" xs)
        , howConcat1WordEntirely n "\1085\1110\1078" us
        ,
            ( "\1087\1077\1088"
            , howConcat1Word n 2 us "\1077\1076" "\1087\1077\1088\1077\1076" xs
            )
        ,
            ( "\1087\1110\1089"
            , howConcat1Word n 2 us "\1083\1103" "\1087\1110\1089\1083\1103" xs
            )
        ,
            ( "\1089\1077\1088"
            , howConcat1Word n 2 us "\1077\1076" "\1089\1077\1088\1077\1076" xs
            )
        ]
        ts

howConcat1Word :: Int -> Int -> String -> String -> String -> String -> [String]
howConcat1Word m n us us' us'' xs
    | take n us == us' = us'' : reverseConcat1 m (drop n us)
    | otherwise = [xs]
{-# INLINE howConcat1Word #-}

howConcat1WordEntirely :: Int -> String -> String -> (String, [String])
howConcat1WordEntirely m ts us = (ts, ts : reverseConcat1 m us)
{-# INLINE howConcat1WordEntirely #-}
