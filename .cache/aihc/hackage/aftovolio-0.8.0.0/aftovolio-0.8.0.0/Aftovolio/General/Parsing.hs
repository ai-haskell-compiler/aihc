{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -threaded -rtsopts #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.General.Parsing
Copyright   :  (c) Oleksandr Zhabenko 2021-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

The additional parsing library functions for the AFTOVolio both old and new variants.
Is taken from the Phonetic.Languages.Parsing module from the
@phonetic-languages-simplified-examples-array@ package to reduce dependencies in general case.
-}
module Aftovolio.General.Parsing (
    -- * Predicates
    isClosingCurlyBracket,
    isSlash,
    isOpeningCurlyBracket,
    variations,

    -- * Transformations
    breakGroupOfStrings,
    breakInSlashes,
    combineVariants,
    combineHeadsWithNexts,
    transformToVariations,

    -- * Files processment for specifications
    readLangSpecs,
    innerProcessmentSimple,
    argsProcessment,
) where

import Aftovolio.General.Base
import Aftovolio.General.PrepareText
import Aftovolio.General.SpecificationsRead
import Aftovolio.General.Syllables
import Data.List (lines, sort, unwords)
import Data.Maybe (fromMaybe)
import GHC.Arr
import GHC.Base
import GHC.List
import System.Environment (getArgs)
import System.IO (FilePath, readFile)
import Text.Read (read, readMaybe)

isClosingCurlyBracket :: String -> Bool
isClosingCurlyBracket = (== "}")
{-# INLINE isClosingCurlyBracket #-}

isSlash :: String -> Bool
isSlash (x : xs)
    | x /= '/' = False
    | null xs = True
    | otherwise = False
isSlash _ = False
{-# INLINE isSlash #-}

isOpeningCurlyBracket :: String -> Bool
isOpeningCurlyBracket = (== "{")
{-# INLINE isOpeningCurlyBracket #-}

breakGroupOfStrings :: [String] -> (([String], [[String]]), [String])
breakGroupOfStrings !xss = ((tss, breakInSlashes uss []), drop 1 zss)
  where
    (!yss, !zss) = break isClosingCurlyBracket xss
    (!tss, !uss) = (\(t1, t2) -> (t1, drop 1 t2)) . break isOpeningCurlyBracket $ yss
{-# INLINE breakGroupOfStrings #-}

breakInSlashes :: [String] -> [[String]] -> [[String]]
breakInSlashes !wss !usss
    | null lss = kss : usss
    | otherwise = breakInSlashes (drop 1 lss) (kss : usss)
  where
    (!kss, !lss) = break isSlash wss

combineVariants :: ([String], [[String]]) -> [[String]]
combineVariants (!xss, (!yss : ysss)) = (xss `mappend` yss) : combineVariants (xss, ysss)
combineVariants _ = []

combineHeadsWithNexts :: [[String]] -> [String] -> [[String]]
combineHeadsWithNexts !xsss !yss
    | null yss = xsss
    | otherwise =
        combineHeadsWithNexts [xss `mappend` zss | xss <- xsss, zss <- zsss] uss
  where
    (!t, !uss) = breakGroupOfStrings yss
    !zsss = combineVariants t

transformToVariations :: [String] -> [[String]]
transformToVariations !yss
    | null yss = []
    | otherwise = combineHeadsWithNexts xsss tss
  where
    (!y, !tss) = breakGroupOfStrings yss
    !xsss = combineVariants y
{-# INLINE transformToVariations #-}

variations :: [String] -> Bool
variations xss =
    any isSlash xss
        && any isOpeningCurlyBracket xss
        && any isClosingCurlyBracket xss
{-# INLINE variations #-}

innerProcessmentSimple ::
    -- | Must be a valid 'GWritingSystemPRPLX' specifications 'String' representation only (see the gwrsysExample.txt file in the @phonetic-languages-phonetics-basics@ package as a schema);
    String ->
    -- | Must be a 'String' with the 5 meaningful lines that are delimited with the \'~\' line one from another with the specifications for the possible allophones (if any), 'CharPhoneticClassification', white spaces information (two 'String's) and the 'String' of all the possible 'PLL' 'Char's;
    String ->
    -- | Must be a 'String' with the 'SegmentRulesG' specifications only;
    String ->
    -- | Must be a 'String' with the 'Concatenations' specifications only (see the data in the EnglishConcatenated.txt file in the @phonetic-languages-phonetics-basics@ package as a list of English equivalents of the needed 'String's). These are to be prepended to the next word.
    String ->
    -- | Must be a 'String' with the 'Concatenations' specifications only (see the data in the EnglishConcatenated.txt file in the @phonetic-languages-phonetics-basics@ package as a list of English equivalents of the needed 'String's). These are to be appended to the previous word.
    String ->
    ( GWritingSystemPRPLX
    , [(Char, Char)]
    , CharPhoneticClassification
    , SegmentRulesG
    , String
    , String
    , Concatenations
    , Concatenations
    , String
    )
innerProcessmentSimple gwrsCnts controlConts segmentData concatenationsFileP concatenationsFileA =
    let [allophonesGs, charClfs, jss, vss, wss] = groupBetweenChars '~' . lines $ controlConts
        wrs = getGWritingSystem '~' gwrsCnts
        ks =
            sort . fromMaybe [] $ (readMaybe (unwords allophonesGs) :: Maybe [(Char, Char)])
        arr = read (unwords charClfs) :: Array Int PRS -- The 'Array' must be previously sorted in the ascending order.
        gs = read segmentData :: SegmentRulesG
        ysss =
            sort2Concat . fromMaybe [] $ (readMaybe concatenationsFileP :: Maybe [[String]])
        zzzsss =
            sort2Concat . fromMaybe [] $ (readMaybe concatenationsFileA :: Maybe [[String]])
        js = concat jss
        vs = concat vss
        ws = sort . concat $ wss
     in (wrs, ks, arr, gs, js, vs, ysss, zzzsss, ws)
{-# INLINE innerProcessmentSimple #-}

argsProcessment ::
    -- | With the 'GWritingSystemPRPLX' specifications only (see the gwrsysExample.txt file in the @phonetic-languages-phonetics-basics@ package as a schema);
    FilePath ->
    -- | With the 5 meaningful lines that are delimited with the \'~\' line one from another with the specifications for the possible allophones (if any), 'CharPhoneticClassification', white spaces information (two 'String's) and the 'String' of all the possible 'PLL' 'Char's;
    FilePath ->
    -- | With the 'SegmentRulesG' specifications only;
    FilePath ->
    -- | With the 'Concatenations' specifications only (see the data in the EnglishConcatenated.txt file in the @phonetic-languages-phonetics-basics@ package as a list of English equivalents of the needed 'String's). These are to be prepended to the next word.
    FilePath ->
    -- | With the 'Concatenations' specifications only (see the data in the EnglishConcatenated.txt file in the @phonetic-languages-phonetics-basics@ package as a list of English equivalents of the needed 'String's). These are to be appended to the previous word.
    FilePath ->
    IO [String]
argsProcessment fileGWrSys controlFile segmentRulesFile concatenationsFileP concatenationsFileA =
    mapM
        readFile
        [ controlFile
        , fileGWrSys
        , segmentRulesFile
        , concatenationsFileP
        , concatenationsFileA
        ]
{-# INLINE argsProcessment #-}

{- | The function that is mostly intended to be used by the end user. Reads the specifications from
the5 given files and returns the data that can be used further for generalized AFTOVolio.
-}
readLangSpecs ::
    -- | With the 'GWritingSystemPRPLX' specifications only (see the gwrsysExample.txt file in the @phonetic-languages-phonetics-basics@ package as a schema);
    FilePath ->
    -- | With the 5 meaningful lines that are delimited with the \'~\' line one from another with the specifications for the possible allophones (if any), 'CharPhoneticClassification', white spaces information (two 'String's) and the 'String' of all the possible 'PLL' 'Char's;
    FilePath ->
    -- | With the 'SegmentRulesG' specifications only;
    FilePath ->
    -- | With the 'Concatenations' specifications only (see the data in the EnglishConcatenated.txt file in the @phonetic-languages-phonetics-basics@ package as a list of English equivalents of the needed 'String's). These are to be prepended to the next word.
    FilePath ->
    -- | With the 'Concatenations' specifications only (see the data in the EnglishConcatenated.txt file in the @phonetic-languages-phonetics-basics@ package as a list of English equivalents of the needed 'String's). These are to be appended to the previous word.
    FilePath ->
    IO
        ( GWritingSystemPRPLX
        , [(Char, Char)]
        , CharPhoneticClassification
        , SegmentRulesG
        , String
        , String
        , Concatenations
        , Concatenations
        , String
        )
readLangSpecs fileGWrSys controlFile segmentRulesFile concatenationsFileP concatenationsFileA =
    argsProcessment
        fileGWrSys
        controlFile
        segmentRulesFile
        concatenationsFileP
        concatenationsFileA >>= \xss ->
        let [ controlConts
                , gwrsCnts
                , segmentData
                , concatenationsFileP1
                , concatenationsFileA1
                ] = xss
         in return $
                innerProcessmentSimple
                    gwrsCnts
                    controlConts
                    segmentData
                    concatenationsFileP1
                    concatenationsFileA1
{-# INLINE readLangSpecs #-}
