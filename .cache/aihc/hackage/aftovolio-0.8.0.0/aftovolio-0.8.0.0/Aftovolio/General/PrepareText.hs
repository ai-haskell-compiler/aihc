{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Aftovolio.General.PrepareText
-- Copyright   :  (c) Oleksandr Zhabenko 2020-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to order the 7 or less phonetic language words (or their concatenations)
-- to obtain (to some extent) suitable for poetry or music text.
-- Earlier it has been a module DobutokO.Poetry.Ukrainian.PrepareText
-- from the @dobutokO-poetry@ package.
-- In particular, this module can be used to prepare the phonetic language text
-- by applying the most needed grammar to avoid misunderstanding
-- for the produced text. The attention is paid to the prepositions, pronouns, conjunctions
-- and particles that are most commonly connected (or not) in a significant way
-- with the next text.
-- Uses the information from:
-- https://uk.wikipedia.org/wiki/%D0%A1%D0%BF%D0%BE%D0%BB%D1%83%D1%87%D0%BD%D0%B8%D0%BA
-- and
-- https://uk.wikipedia.org/wiki/%D0%A7%D0%B0%D1%81%D1%82%D0%BA%D0%B0_(%D0%BC%D0%BE%D0%B2%D0%BE%D0%B7%D0%BD%D0%B0%D0%B2%D1%81%D1%82%D0%B2%D0%BE)
--
-- Uses arrays instead of vectors.
-- A list of basic (but, probably not complete and needed to be extended as needed) English words (the articles, pronouns,
-- particles, conjunctions etc.) the corresponding phonetic language translations of which are intended to be used as a
-- 'Concatenations' here is written to the file EnglishConcatenated.txt in the source tarball.

module Aftovolio.General.PrepareText (
  Concatenations
  -- * Basic functions
  , concatWordsFromLeftToRight
  , splitLines
  , splitLinesN
  , isSpC
  , sort2Concat
  -- * The end-user functions
  , prepareText
  , prepareTextN
  , growLinesN
  , prepareGrowTextMN
  , tuneLinesN
  , prepareTuneTextMN
  -- * Used to transform after convertToProperphonetic language from mmsyn6ukr package
  , isPLL
) where

import GHC.Base
import Data.List
import Data.Bits (shiftR)
import GHC.Num ((+),(-),abs)
import CaseBi.Arr (getBFstL',getBFst')
import Data.IntermediateStructures1 (mapI)
import Data.Char (isAlpha,toLower)
import GHC.Arr
import Data.Tuple (fst)

-- | The lists in the list are sorted in the descending order by the word counts in the inner 'String's. All the 'String's
-- in each inner list have the same number of words, and if there is no 'String' with some intermediate number of words (e. g. there
-- are 'String's for 4 and 2 words, but there is no one for 3 words 'String's) then such corresponding list is absent. 
-- Probably the maximum number of words can be not more than 4, and the minimum number is
-- not less than 1, but it depends. The 'String's in the inner lists must be (unlike the inner
-- lists themselves) sorted in the ascending order for the data type to work correctly in the functions of the module.
type Concatenations = [[String]]

type ConcatenationsArr = [Array Int (String,Bool)]

defaultConversion :: Concatenations -> ConcatenationsArr
defaultConversion concatenations = map (f . filter (not . null)) . filter (not . null) $ concatenations
  where f :: [String] -> Array Int (String,Bool)
        f yss = let l = length yss in listArray (0,l-1) . zip yss . cycle $ [True]

-- | Is used to convert a phonetic language text into list of 'String' each of which is ready to be
-- used by the functions from the other modules in the package.
-- It applies minimal grammar links and connections between the most commonly used phonetic language
-- words that \"should\" be paired and not dealt with separately
-- to avoid the misinterpretation and preserve maximum of the semantics for the
-- \"phonetic\" language on the phonetic language basis.
prepareText
  :: [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be prepended to the next word.
  -> [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be appended to the previous word.
  -> String -- ^ A sorted 'String' of possible characters in the phonetic language representation.
  -> String
  -> [String]
prepareText = prepareTextN 7
{-# INLINE prepareText #-}

sort2Concat
 :: [[String]]
 -> Concatenations  -- ^ Data used to concatenate the basic grammar preserving words and word sequences to the next word or
 -- to the previous word to
 -- leave the most of the meaning (semantics) of the text available to easy understanding while reading and listening to.
sort2Concat xsss
 | null xsss = []
 | otherwise = map sort . reverse . sortOn (map (length . words)) $ xsss

-----------------------------------------------------

complexWords2 :: ConcatenationsArr -> String -> (String -> String,String)
complexWords2 concatArrsss@(yss:zsss) zs@(r:rs)
 | getBFst' (False, yss) . unwords $ tss = ((uwxs `mappend`), unwords uss)
 | otherwise = complexWords2 zsss zs
      where y = length . words . fst . unsafeAt yss $ 0
            (tss,uss) = splitAt y . words $ zs
            uwxs = concat tss
complexWords2 _ zs = (id,zs)

pairCompl :: (String -> String,String) -> (String,String)
pairCompl (f,xs) = (f [],xs)

splitWords :: ConcatenationsArr -> [String] -> String -> (String,String)
splitWords concatArrsss tss zs 
  | null . words $ zs = (mconcat tss,[])
  | null ws = (\(xss,uss) -> (mconcat (tss `mappend` xss), unwords uss)) . splitAt 1 . words $ zs
  | otherwise = splitWords concatArrsss (tss `mappend` [ws]) us
        where (ws,us) = pairCompl . complexWords2 concatArrsss $ zs 

concatWordsFromLeftToRight :: ConcatenationsArr -> String -> [String]
concatWordsFromLeftToRight concatArrsss zs = let (ws,us) = splitWords concatArrsss [] zs in
  if null us then [ws] else ws : concatWordsFromLeftToRight concatArrsss us

-----------------------------------------------------

append2prependConv :: Concatenations -> Concatenations
append2prependConv = map (map (unwords . reverse . words))
{-# INLINE append2prependConv #-}

left2right :: [String] -> String
left2right = unwords . reverse . map reverse
{-# INLINE left2right #-}

-----------------------------------------------------

-- | A generalized variant of the 'prepareText' with the arbitrary maximum number of the words in the lines given as the first argument.
prepareTextN
 :: Int -- ^ A maximum number of the words or their concatenations in the resulting list of 'String's.
 -> [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be prepended to the next word.
 -> [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be appended to the previous word.
 -> String -- ^ A sorted 'String' of possible characters in the phonetic language representation.
 -> String
 -> [String]
prepareTextN maxNumWords ysss zsss xs = filter (any (isPLL xs)) . splitLinesN maxNumWords . map (left2right .
  concatWordsFromLeftToRight (defaultConversion . sort2Concat . append2prependConv $ zsss) . left2right .
  concatWordsFromLeftToRight (defaultConversion . sort2Concat $ ysss)) . filter (not . null) . lines

-- | A predicate to check whether the given character is one of the \"\' \\x2019\\x02BC-\".
isSpC :: Char -> Bool
isSpC x = x == '\'' || x == ' ' || x == '\x2019' || x == '\x02BC' || x == '-' || x == '_' || x == '='
{-# INLINE isSpC #-}
{-# DEPRECATED #-}

-- | The first argument must be a 'String' of sorted 'Char's in the ascending order of all possible symbols that can be
-- used for the text in the phonetic language selected. Can be prepared beforehand, or read from the file.
isPLL :: String -> Char -> Bool
isPLL xs y = getBFstL' False (zip xs . replicate 10000 $ True) y

-- | The function is recursive and is applied so that all returned elements ('String') are no longer than 7 words in them.
splitLines :: [String] -> [String]
splitLines = splitLinesN 7
{-# INLINE splitLines #-}

-- | A generalized variant of the 'splitLines' with the arbitrary maximum number of the words in the lines given as the first argument.
splitLinesN :: Int -> [String] -> [String]
splitLinesN maxNumWords xss
 | null xss || maxNumWords <= 0 = []
 | otherwise = mapI (\xs -> compare (length . words $ xs) maxNumWords == GT) (\xs -> let yss = words xs in
     splitLinesN maxNumWords . map unwords . (\(q,r) -> [q,r]) . splitAt (shiftR (length yss) 1) $ yss) $ xss

------------------------------------------------

{-| Given a positive number and a list tries to rearrange the list's 'String's by concatenation of the several elements of the list
so that the number of words in every new 'String' in the resulting list is not greater than the 'Int' argument. If some of the
'String's have more than that number quantity of the words then these 'String's are preserved.
-}
growLinesN :: Int -> [String] -> [String]
growLinesN maxNumWords xss
 | null xss || maxNumWords < 0 = []
 | otherwise = unwords yss : growLinesN maxNumWords zss
     where l = length . takeWhile (<= maxNumWords) . scanl1 (+) . map (length . words) $ xss -- the maximum number of lines to be taken
           (yss,zss) = splitAt (max l 1) xss

{-| The function combines the 'prepareTextN' and 'growLinesN' function. Applies needed phonetic language preparations
to the text and tries to \'grow\' the resulting 'String's in the list so that the number of the words in every
of them is no greater than the given first 'Int' number.
-}
prepareGrowTextMN
 :: Int -- ^ A maximum number of the words or their concatenations in the resulting list of 'String's.
 -> Int -- ^ A number of words in every 'String' that the function firstly forms. To have some sense of usage, must be less than the first argument.
 -> [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be prepended to the next word.
 -> [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be appended to the previous word.
 -> String -- ^ A sorted 'String' of possible characters in the phonetic language representation.
 -> String
 -> [String]
prepareGrowTextMN maxNumWords firstNumWords ysss zsss xs = growLinesN maxNumWords . prepareTextN firstNumWords ysss zsss xs
{-# INLINE prepareGrowTextMN #-}

-------------------------------------

{-| Recursively splits the concatenated list of lines of words so that in every resulting 'String' in the list
except the last one there is just 'Int' -- the first argument -- words.
-}
tuneLinesN :: Int -> [String] -> [String]
tuneLinesN maxNumWords xss
 | null xss || maxNumWords < 0 = []
 | otherwise =
    let wss = words . unwords $ xss
        (yss,zss) = splitAt maxNumWords wss
          in unwords yss : tuneLinesN maxNumWords zss

{-| The function combines the 'prepareTextN' and 'tuneLinesN' functions. Applies needed phonetic language preparations
to the phonetic language text and splits the list of 'String's so that the number of the words in each of them (except the last one)
is equal the given first 'Int' number.
-}
prepareTuneTextMN
  :: Int -- ^ A maximum number of the words or their concatenations in the resulting list of 'String's.
  -> Int -- ^ A number of words in every 'String' that the function firstly forms. To have some sense of usage, must be less than the first argument.
  -> [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be prepended to the next word.
  -> [[String]] -- ^ Is intended to become a valid 'Concatenations' that are to be appended to the previous word
  -> String -- ^ A sorted 'String' of possible characters in the phonetic language representation.
  -> String
  -> [String]
prepareTuneTextMN maxNumWords firstNumWords ysss zsss xs = tuneLinesN maxNumWords . prepareTextN firstNumWords ysss zsss xs
{-# INLINE prepareTuneTextMN #-}
