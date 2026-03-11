{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields -fobject-code #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.General.Syllables
Copyright   :  (c) Oleksandr Zhabenko 2021-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

This module works with syllable segmentation. The generalized version for the module
'Aftovolio.Ukrainian.Syllable'.
-}
module Aftovolio.General.Syllables (
    -- * Data types and type synonyms
    PRS (..),
    PhoneticPhenomenonRep,
    PhoneticType (..),
    CharPhoneticClassification,
    StringRepresentation,
    SegmentationInfo1 (..),
    SegmentationPredFunction (..),
    SegmentationPredFData (..),
    SegmentationFDP,
    Eval2Bool (..),
    DListFunctionResult,
    SegmentationLineFunction (..),
    SegmentationRules1 (..),
    SegmentRulesG,
    DListRepresentation (..),
    BasicSpaces,
    AdditionalDelimiters,

    -- * Basic functions
    str2PRSs,
    sndGroups,
    groupSnds,
    divCnsnts,
    reSyllableCntnts,
    divSylls,
    createSyllablesPL,

    -- * Auxiliary functions
    gBF4,
    findC,
    createsSyllable,
    isSonorous1,
    isVoicedC1,
    isVoicelessC1,
    notCreatesSyllable2,
    notEqC,
    fromPhoneticType,
) where

import Aftovolio.General.Base
import CaseBi.Arr
import Data.Char (isLetter)
import Data.IntermediateStructures1 (mapI)
import qualified Data.List as L (find, groupBy, intercalate, words)
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple (fst, snd)
import GHC.Arr
import GHC.Base
import GHC.Enum (fromEnum)
import GHC.Exts
import GHC.Int
import GHC.List
import GHC.Num ((-))
import Text.Read (Read (..), readMaybe)
import Text.Show (Show (..))

-- Inspired by: https://github.com/OleksandrZhabenko/mm1/releases/tag/0.2.0.0

-- CAUTION: Please, do not mix with the show7s functions, they are not interoperable.

-- | The AFTOVolio phonetic phenomenon representation. 
data PRS = SylS
    { charS :: {-# UNPACK #-} !Char
    -- ^ Phonetic languages phenomenon representation. Usually, a phoneme, but it can be otherwise something different.
    , phoneType :: {-# UNPACK #-} !PhoneticType
    -- ^ Some encoded type. For the vowels it has reserved value of 'P' 0, for the sonorous consonants - 'P' 1 and 'P' 2,
    -- for the voiced consonants - 'P' 3 and 'P' 4, for the voiceless consonants - 'P' 5 and 'P' 6. Nevertheless, it is possible to redefine the data by rewriting the
    -- respective parts of the code here.
    }
    deriving (Eq, Read)

-- | Type synonym to be used for clarity and better code readability.
type PhoneticPhenomenonRep = PRS

instance Ord PRS where
    compare (SylS x1 y1) (SylS x2 y2) =
        case compare x1 x2 of
            EQ -> compare y1 y2
            ~z -> z

instance Show PRS where
    show (SylS c (P x)) = "SylS \'" `mappend` (c : '\'' : ' ' : show x)

data PhoneticType = P {-# UNPACK #-} !Int8 deriving (Eq, Ord, Read)

instance Show PhoneticType where
    show (P x) = 'P' : ' ' : show x

fromPhoneticType :: PhoneticType -> Int
fromPhoneticType (P x) = fromEnum x

-- | The 'Array' 'Int' must be sorted in the ascending order to be used in the module correctly.
type CharPhoneticClassification = Array Int PhoneticPhenomenonRep

{- | The 'String' of converted phonetic language representation 'Char' data is converted to this type to apply syllable
segmentation or other transformations.
-}
type StringRepresentation = [PhoneticPhenomenonRep]

-- | Is somewhat rewritten from the 'CaseBi.Arr.gBF3' function (not exported) from the @mmsyn2-array@ package.
gBF4 ::
    (Ix i) =>
    (# Int#, PhoneticPhenomenonRep #) ->
    (# Int#, PhoneticPhenomenonRep #) ->
    Char ->
    Array i PhoneticPhenomenonRep ->
    Maybe PhoneticPhenomenonRep
gBF4 (# !i#, k #) (# !j#, m #) c arrayCharClassification
    | isTrue# ((j# -# i#) ># 1#) =
        case compare c (charS p) of
            GT -> gBF4 (# n#, p #) (# j#, m #) c arrayCharClassification
            LT -> gBF4 (# i#, k #) (# n#, p #) c arrayCharClassification
            _ -> Just p
    | c == charS m = Just m
    | c == charS k = Just k
    | otherwise = Nothing
  where
    !n# = (i# +# j#) `quotInt#` 2#
    !p = unsafeAt arrayCharClassification (I# n#)

findC ::
    Char ->
    Array Int PhoneticPhenomenonRep ->
    Maybe PhoneticPhenomenonRep
findC c arrayCharClassification = gBF4 (# i#, k #) (# j#, m #) c arrayCharClassification
  where
    !(I# i#, I# j#) = bounds arrayCharClassification
    !k = unsafeAt arrayCharClassification (I# i#)
    !m = unsafeAt arrayCharClassification (I# i#)
{-# INLINE findC #-}

str2PRSs :: CharPhoneticClassification -> String -> StringRepresentation
str2PRSs arrayCharClassification = map (\c -> fromJust . findC c $ arrayCharClassification)
{-# INLINE str2PRSs #-}

{- | Function-predicate 'createsSyllable' checks whether its argument is a phoneme representation that
every time being presented in the text leads to the creation of the new syllable (in the 'PhoneticPhenomenonRep' format).
Usually it is a vowel, but in some languages there can be syllabic phonemes that are not considered to be
vowels.
-}
createsSyllable :: PhoneticPhenomenonRep -> Bool
createsSyllable = (== P 0) . phoneType
{-# INLINE createsSyllable #-}

-- | Function-predicate 'isSonorous1' checks whether its argument is a sonorous consonant representation in the 'PhoneticPhenomenonRep' format.
isSonorous1 :: PhoneticPhenomenonRep -> Bool
isSonorous1 = (`elem` [1, 2]) . fromPhoneticType . phoneType
{-# INLINE isSonorous1 #-}

-- | Function-predicate 'isVoicedC1' checks whether its argument is a voiced consonant representation in the 'PhoneticPhenomenonRep' format.
isVoicedC1 :: PhoneticPhenomenonRep -> Bool
isVoicedC1 = (`elem` [3, 4]) . fromPhoneticType . phoneType
{-# INLINE isVoicedC1 #-}

-- | Function-predicate 'isVoiceless1' checks whether its argument is a voiceless consonant representation in the 'PhoneticPhenomenonRep' format.
isVoicelessC1 :: PhoneticPhenomenonRep -> Bool
isVoicelessC1 = (`elem` [5, 6]) . fromPhoneticType . phoneType
{-# INLINE isVoicelessC1 #-}

-- | Binary function-predicate 'notCreatesSyllable2' checks whether its arguments are both consonant representations in the 'PhoneticPhenomenonRep' format.
notCreatesSyllable2 :: PhoneticPhenomenonRep -> PhoneticPhenomenonRep -> Bool
notCreatesSyllable2 x y
    | phoneType x == P 0 || phoneType y == P 0 = False
    | otherwise = True
{-# INLINE notCreatesSyllable2 #-}

-- | Binary function-predicate 'notEqC' checks whether its arguments are not the same consonant sound representations (not taking palatalization into account).
notEqC ::
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    PhoneticPhenomenonRep ->
    PhoneticPhenomenonRep ->
    Bool
notEqC xs x y
    | (== cy) . getBFstLSorted' cx xs $ cx = False
    | otherwise = cx /= cy
  where
    !cx = charS x
    !cy = charS y
{-# INLINE notEqC #-}

{- | Function 'sndGroups' converts a word being a list of 'PhoneticPhenomenonRep' to the list of phonetically similar (consonants grouped with consonants and each vowel separately)
sounds representations in 'PhoneticPhenomenonRep' format.
-}
sndGroups :: [PhoneticPhenomenonRep] -> [[PhoneticPhenomenonRep]]
sndGroups ys@(_ : _) = L.groupBy notCreatesSyllable2 ys
sndGroups _ = []
{-# INLINE sndGroups #-}

groupSnds :: [PhoneticPhenomenonRep] -> [[PhoneticPhenomenonRep]]
groupSnds = L.groupBy (\x y -> createsSyllable x == createsSyllable y)
{-# INLINE groupSnds #-}

data SegmentationInfo1 = SI
    { fieldN :: !Int8
    -- ^ Number of fields in the pattern matching that are needed to apply the segmentation rules. Not less than 1.
    , predicateN :: Int8
    -- ^ Number of predicates in the definition for the 'fieldN' that are needed to apply the segmentation rules.
    }
    deriving (Eq, Read, Show)

instance PhoneticElement SegmentationInfo1 where
    readPEMaybe rs
        | not . any isLetter $ rs = Nothing
        | otherwise =
            let (ys : yss) = L.words rs
             in case ys of
                    "SI" -> case yss of
                        [xs, ts] -> case (readMaybe xs :: Maybe Int8) of
                            Just m -> case (readMaybe ts :: Maybe Int8) of
                                Just n -> Just (SI m n)
                                _ -> Nothing
                            _ -> Nothing
                        _ -> Nothing
                    _ -> Nothing

{- | We can think of 'SegmentationPredFunction' in terms of @f ('SI' fN pN) ks [x_{1},x_{2},...,x_{i},...,x_{fN}]@. Comparing with
'divCnsnts' from the 'Aftovolio.Ukrainian.Syllable' we can postulate that it consists of the following logical terms in
the symbolic form:

1) 'phoneType' x_{i} \`'elem'\` (X{...} = 'map' 'P' ['Int8'])

2) 'notEqC' ks x_{i} x_{j} (j /= i)

combined with the standard logic Boolean operations of '(&&)', '(||)' and 'not'. Further, the 'not' can be transformed into the
positive (affirmative) form using the notion of the universal set for the task. This transformation needs that the similar
phonetic phenomenae (e. g. the double sounds -- the prolonged ones) belong to the one syllable and not to the different ones
(so they are not related to different syllables, but just to the one and the same). Since such assumption has been used,
we can further represent the function by the following data type and operations with it, see 'SegmentationPredFData'.
-}
data SegmentationPredFunction
    = PF (SegmentationInfo1 -> [(Char, Char)] -> [PhoneticPhenomenonRep] -> Bool)

data SegmentationPredFData a b
    = L Int [Int] (Array Int a)
    | NEC Int Int (Array Int a) [b]
    | C (SegmentationPredFData a b) (SegmentationPredFData a b)
    | D (SegmentationPredFData a b) (SegmentationPredFData a b)
    deriving (Eq, Read, Show)

class Eval2Bool a where
    eval2Bool :: a -> Bool

type SegmentationFDP = SegmentationPredFData PhoneticPhenomenonRep (Char, Char)

instance Eval2Bool (SegmentationPredFData PhoneticPhenomenonRep (Char, Char)) where
    eval2Bool (L i js arrayCharClassification)
        | all (<= n) js && i <= n && i >= 1 && all (>= 1) js =
            fromPhoneticType (phoneType (unsafeAt arrayCharClassification $ i - 1)) `elem` js
        | otherwise =
            error
                "Aftovolio.General.Syllables.eval2Bool: 'L' element is not properly defined. "
      where
        n = numElements arrayCharClassification
    eval2Bool (NEC i j arrayCharClassification allophones)
        | i >= 1 && j >= 1 && i /= j && i <= n && j <= n =
            notEqC allophones (unsafeAt arrayCharClassification $ i - 1) (unsafeAt arrayCharClassification $ j - 1)
        | otherwise =
            error
                "Aftovolio.General.Syllables.eval2Bool: 'NEC' element is not properly defined. "
      where
        n = numElements arrayCharClassification
    eval2Bool (C x y) = eval2Bool x && eval2Bool y
    eval2Bool (D x y) = eval2Bool x || eval2Bool y

type DListFunctionResult = ([PhoneticPhenomenonRep] -> [PhoneticPhenomenonRep], [PhoneticPhenomenonRep] -> [PhoneticPhenomenonRep])

class DListRepresentation a b where
    toDLR :: b -> [a] -> ([a] -> [a], [a] -> [a])

instance DListRepresentation PhoneticPhenomenonRep Int8 where
    toDLR left xs
        | null xs = (id, id)
        | null ts = (id, (zs `mappend`))
        | null zs = ((`mappend` ts), id)
        | otherwise = ((`mappend` ts), (zs `mappend`))
      where
        (ts, zs) = splitAt (fromEnum left) xs

data SegmentationLineFunction = LFS
    { infoSP :: SegmentationInfo1
    , predF :: SegmentationFDP
    -- ^ The predicate to check the needed rule for segmentation.
    , resF :: Int8
    -- ^ The result argument to be appended to the left of the group of consonants if the 'predF' returns 'True' for its arguments. Is an argument to the 'toDLR'.
    }
    deriving (Read, Show)

data SegmentationRules1 = SR1
    { infoS :: SegmentationInfo1
    , lineFs :: [SegmentationLineFunction]
    -- ^ The list must be sorted in the appropriate order of the guards usage for the predicates.
    -- The length of the list must be equal to the ('fromEnum' . 'predicateN' . 'infoS') value.
    }
    deriving (Read, Show)

{- | List of the 'SegmentationRules1' sorted in the descending order by the 'fieldN' 'SegmentationInfo1' data and where the
length of all the 'SegmentationPredFunction' lists of 'PhoneticPhenomenonRep' are equal to the 'fieldN' 'SegmentationInfo1' data by definition.
-}
type SegmentRulesG = [SegmentationRules1]

{- | Function 'divCnsnts' is used to divide groups of consonants into two-elements lists that later are made belonging to
different neighbour syllables if the group is between two vowels in a word. The group must be not empty, but this is not checked.
The example phonetical information for the proper performance in Ukrainian can be found from the:
https://msn.khnu.km.ua/pluginfile.php/302375/mod_resource/content/1/%D0%9B.3.%D0%86%D0%86.%20%D0%A1%D0%BA%D0%BB%D0%B0%D0%B4.%D0%9D%D0%B0%D0%B3%D0%BE%D0%BB%D0%BE%D1%81.pdf
The example of the 'divCnsnts' can be found at: https://hackage.haskell.org/package/aftovolio-0.6.2.0/docs/Aftovolio-Ukrainian-Syllable.html#v:divCnsnts
-}
divCnsnts ::
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    SegmentRulesG ->
    [PhoneticPhenomenonRep] ->
    DListFunctionResult
divCnsnts allophones segmentRules xs@(_ : _) = toDLR left xs
  where
    !js = fromJust . L.find ((== length xs) . fromEnum . fieldN . infoS) $ segmentRules -- js :: SegmentationRules1
    !left = resF . fromJust . L.find (eval2Bool . predF) . lineFs $ js
divCnsnts _ _ [] = (id, id)

reSyllableCntnts ::
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    SegmentRulesG ->
    [[PhoneticPhenomenonRep]] ->
    [[PhoneticPhenomenonRep]]
reSyllableCntnts allophones segmentRules (xs : ys : zs : xss)
    | (/= P 0) . phoneType . last $ ys =
        fst (divCnsnts allophones segmentRules ys) xs
            : reSyllableCntnts allophones segmentRules (snd (divCnsnts allophones segmentRules ys) zs : xss)
    | otherwise = reSyllableCntnts allophones segmentRules ((xs `mappend` ys) : zs : xss)
reSyllableCntnts _ _ (xs : ys : _) = [(xs `mappend` ys)]
reSyllableCntnts _ _ xss = xss

divSylls :: [[PhoneticPhenomenonRep]] -> [[PhoneticPhenomenonRep]]
divSylls = mapI (\ws -> (length . filter createsSyllable $ ws) > 1) h3
  where
    h3 us =
        [ys `mappend` take 1 zs]
            `mappend` (L.groupBy (\x y -> createsSyllable x && phoneType y /= P 0) . drop 1 $ zs)
      where
        (ys, zs) = break createsSyllable us

-- | Basic group of delimiters in the 'String' that are not converted to the phonemes or not silent sounds. Silent sounds or their absense, e. g. word gaps.
type BasicSpaces = String

-- | Is used for additional delimiters that are not included in the 'BasicSpaces' group for some reason.
type AdditionalDelimiters = String

{- | The function actually creates syllables using the provided data. Each resulting inner-most list is a phonetic language representation
of the syllable according to the rules provided.
-}
createSyllablesPL ::
    -- | Data used to obtain the phonetic language representation of the text.
    GWritingSystemPRPLX ->
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    CharPhoneticClassification ->
    SegmentRulesG ->
    -- | Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
    BasicSpaces ->
    -- | Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
    AdditionalDelimiters ->
    -- | Actually the converted 'String'.
    String ->
    [[[PhoneticPhenomenonRep]]]
createSyllablesPL writingSystem allophones arrayCharClassification segmentRules basicSpaces additionalDelims =
    map (divSylls . reSyllableCntnts allophones segmentRules . groupSnds . str2PRSs arrayCharClassification)
        . words1
        . mapMaybe g
        . convertToProperPL
        . map (\x -> if x == '-' then ' ' else x)
  where
    g x
        | x `elem` basicSpaces = Nothing
        | x `notElem` additionalDelims = Just x
        | otherwise = Just ' '
    words1 xs = if null ts then [] else w : words1 s'' -- Practically this is an optimized version for this case 'words' function from Prelude.
      where
        ts = dropWhile (== ' ') xs
        (w, s'') = break (== ' ') ts
    {-# NOINLINE words1 #-}
    convertToProperPL = concatMap string1 . stringToXG writingSystem
{-# INLINE createSyllablesPL #-}
