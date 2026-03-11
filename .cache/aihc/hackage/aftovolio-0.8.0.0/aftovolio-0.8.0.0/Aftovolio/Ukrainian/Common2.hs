{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Ukrainian.Common2
Copyright   :  (c) Oleksandr Zhabenko 2021-2023
License     :  MIT
Maintainer  :  oleksandr.zhabenko@yahoo.com

Functions provide functionality of a musical instrument synthesizer or for Ukrainian speech synthesis
especially for poets, translators and writers. Is rewritten from the module Melodics.ByteString.Ukrainian.Arr
for optimization purposes. Contains the common for two modules definitions.
Phonetic material is taken from the :

Solomija Buk, Ján Mačutek, Andrij Rovenchak. Some properties of
the Ukrainian writing system. [Electronic resource] https://arxiv.org/ftp/arxiv/papers/0802/0802.4198.pdf
-}
module Aftovolio.Ukrainian.Common2 where

import CaseBi.Arr
import GHC.Arr (Array (..))
import GHC.Base
import GHC.List (repeat, zip)
import Text.Show (Show (..))

{-
-- Inspired by: https://mail.haskell.org/pipermail/beginners/2011-October/008649.html
-- -}

data Triple = Z | O | T
    deriving (Eq, Ord, Show)

isUkrainianL :: Char -> Bool
isUkrainianL y
    | (y >= '\1070' && y <= '\1097') = True
    | otherwise =
        getBFstLSorted'
            False
            ( map
                (\x -> (x, True))
                "'-\700\1028\1030\1031\1068\1100\1102\1103\1108\1110\1111\1168\1169\8217"
            )
            y

isUkrainianLTup :: Array Int (Char, Bool) -> Char -> Bool
isUkrainianLTup !tup15 y
    | (y >= '\1070' && y <= '\1097') = True
    | otherwise = getBFst' (False, tup15) y

isConsNotJ :: Char -> Bool
isConsNotJ =
    getBFstLSorted'
        False
        ( zip
            "\1073\1074\1075\1076\1078\1079\1082\1083\1084\1085\1087\1088\1089\1090\1092\1093\1094\1095\1096\1097\1169"
            (repeat True)
        )

isConsNotJTup :: Array Int (Char, Bool) -> Char -> Bool
isConsNotJTup !tup16 = getBFst' (False, tup16)
