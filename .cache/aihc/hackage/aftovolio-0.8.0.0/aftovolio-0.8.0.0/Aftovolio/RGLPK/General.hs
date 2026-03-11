{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.RGLPK.General
Copyright   :  (c) Oleksandr Zhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Can be used to calculate the durations of the approximations of the phonemes
using some prepared text with its correct (at least mostly) pronunciation.
The prepared text is located in the same directory and contains lines -the
phonetic language word and its duration in seconds separated with whitespace.
The library is intended to use the functionality of the :

1) R programming language https://www.r-project.org/

2) Rglpk library https://cran.r-project.org/web/packages/Rglpk/index.html

3) GNU GLPK library https://www.gnu.org/software/glpk/glpk.html

For more information, please, see the documentation for them.

For the model correctness the js here refers to sorted list of the 'Char' representations of the phonetic language phenomenae.

The length of the 'String' js is refered to as 'lng'::'Int'. The number of 'pairs'' function elements in the lists is refered to
as 'nn'::'Int'. The number of constraints is refered here as 'nc'::'Int'. @nc == nn `quot` 2@.

Is generalized from the Numeric.Wrapper.R.GLPK.Phonetics.Ukrainian.Durations module from
the @r-glpk-phonetic-languages-ukrainian-durations@ package.
-}
module Aftovolio.RGLPK.General where

import CaseBi.Arr (getBFstL')
import Data.Bits (shiftR)
import Data.Foldable (foldl')
import Data.List
import Data.Lists.FLines (newLineEnding)
import Data.Maybe
import GHC.Arr
import GHC.Base
import GHC.Num (abs, (*), (+), (-))
import Numeric
import Text.Read
import Text.Show (Show (..))

createCoeffsObj :: Int -> [String] -> [Double]
createCoeffsObj l xss
    | length xss < l = f xss `mappend` replicate (l - length xss) 1.0
    | otherwise = f (take l xss)
  where
    f = map (\ts -> fromMaybe 1.0 (readMaybe ts :: Maybe Double))

countCharInWords :: [String] -> Char -> [Int]
countCharInWords xss x
    | null xss = []
    | otherwise = map (length . filter (== x)) xss

matrix1Column :: PairwiseC -> [String] -> String -> Char -> [Int]
matrix1Column pw xss js x = pairwiseComparings x pw . mconcat $ [countCharInWords xss x, rs, rs]
  where
    l = length js
    iX = fromMaybe (-l - 1) . elemIndex x $ js
    rs = if iX < 0 then [] else replicate iX 0 `mappend` (1 : replicate (l - 1 - iX) 0)

pairwiseComparings :: Char -> PairwiseC -> [Int] -> [Int]
pairwiseComparings x y zs = zs `mappend` pairs' y x

{- | A way to encode the pairs of the phonetic language representations that give some additional associations, connections
between elements, usually being caused by some similarity or commonality of the pronunciation act for the phenomenae
corresponding to these elements.
All ['Int'] must be equal in 'length' throughout the same namespace and this length is given as 'Int' argument in
the 'PairwisePL'. This 'Int' parameter is @nn@.
-}
data PairwisePL = PW Char Int [Int] deriving (Eq, Read, Show)

lengthPW :: PairwisePL -> Int
lengthPW (PW _ l _) = l

charPW :: PairwisePL -> Char
charPW (PW c _ _) = c

listPW :: PairwisePL -> [Int]
listPW (PW _ _ xs) = xs

data PairwiseC = LL [PairwisePL] Int deriving (Eq, Read, Show)

isCorrectPWC :: PairwiseC -> Bool
isCorrectPWC (LL xs n) = n == minimum (map lengthPW xs)

pwsC :: PairwiseC -> [PairwisePL]
pwsC (LL xs n) = map (\(PW c m ys) -> PW c n . take n $ ys) xs

pairs' :: PairwiseC -> Char -> [Int]
pairs' y@(LL xs n) x
    | isCorrectPWC y =
        let z = find ((== x) . charPW) . pwsC $ y
         in if isJust z
                then listPW . fromJust $ z
                else replicate n 0
    | otherwise =
        error "Aftovolio.RGLPK.General.pairs': Not defined for the arguments. "

-- | Actually @n@ is a 'length' bss.
matrixLine ::
    -- | The number of 'pairs'' function elements in the lists.
    Int ->
    -- | Actually the data type value that sets the behaviour of the 'pairs'' function.
    PairwiseC ->
    [String] ->
    -- | A sorted list of the 'Char' representations of the phonetic language phenomenae.
    String ->
    String
matrixLine nn pw bss js
    | null bss || n <= 0 = []
    | otherwise =
        mconcat
            [ "mat1 <- matrix(c("
            , intercalate ", "
                . map show
                . concatMap
                    (matrix1Column pw (bss `mappend` bss) js)
                $ js
            , "), nrow = "
            , show (2 * n + 2 * length js + nn)
            , ")"
            , newLineEnding
            ]
  where
    n = length bss

objLine ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | List of pairs of indices that shows how the input data is related to the representation
    -- (which coefficients relates to which representation elements).
    [(Int, Int)] ->
    -- | An array of coefficients.
    Array Int Double ->
    String
objLine lng xs arr
    | numElements arr >= lng =
        mconcat
            [ "obj1 <- c("
            , intercalate ", " . map (\t -> showFFloat Nothing t "") . objCoeffsNew lng xs $
                arr
            , ")"
            , newLineEnding
            ]
    | otherwise =
        error "Aftovolio.RGLPK.General.objLine: Not defined for the short argument. "

-- | A way to reorder the coefficients of the input and the elements representations related to each other.
objCoeffsNew ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | List of pairs of indices that shows how the input data is related to the representation
    -- (which coefficients relates to which representation elements).
    [(Int, Int)] ->
    -- | An array of coefficients.
    Array Int Double ->
    [Double]
objCoeffsNew lng xs arr =
    let lst = map (\(x, y) -> (x, unsafeAt arr y)) xs
     in map (getBFstL' 1.0 lst) [0 .. lng - 1]

maxLine :: String
maxLine = "max1 <- TRUE\n"

dirLine ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | The number of 'pairs'' function elements in the lists.
    Int ->
    -- | An argument of the 'matrixLine' function.
    [String] ->
    -- | A sorted list of the 'Char' representations of the phonetic language phenomenae.
    String ->
    String
dirLine lng nn bss js =
    mconcat
        [ "dir1 <- c(\"<"
        , g "<" bss
        , "\", \">"
        , g ">" (bss, map (: []) js)
        , "\""
        , h0 lng
        , h (shiftR nn 1)
        , ")"
        , newLineEnding
        ]
  where
    g xs ys = (intercalate ("\", \"" `mappend` xs) . replicate (length ys) $ "=")
    h n = concat . replicate n $ ", \">=\", \"<=\""
    h0 n = concat . replicate n $ ", \"<=\""

rhsLineG :: [Double] -> [Double] -> [Double] -> String
rhsLineG zs xs ys = mconcat ["rhs1 <- c(", f (mconcat [xs, ys, zs]), ")", newLineEnding]
  where
    f ts = (intercalate ", " . map (\t -> showFFloat Nothing t "") $ ts)

rhsLine ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | The number of 'pairs'' function elements in the lists.
    Int ->
    -- | Maximum duration of the phonetic language element representation in seconds.
    Double ->
    -- | A minimum positive duration value for some group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. vowels) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for some *special* group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. soft sign representation) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for all other phonetic language representations (usually, some sorts of
    -- phonemes) to set a general (common) behaviour for the set of resulting values.
    Double ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the some group of representations (e. g. vowels).
    [Int] ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the special group of representations (e. g. soft sign).
    [Int] ->
    [Double] ->
    [Double] ->
    String
rhsLine lng nn mx mn1 mnSpecial mnG xs1 sps1 =
    rhsLineG . mconcat $
        [ minDurations lng mn1 mnSpecial mnG xs1 sps1
        , maxDurations lng mx
        , constraintsR1 (shiftR nn 1)
        ]

constraintsR1 :: Int -> [Double]
constraintsR1 n = replicate (2 * n) 0.0

minDurations ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | A minimum positive duration value for some group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. vowels) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for some *special* group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. soft sign representation) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for all other phonetic language representations (usually, some sorts of
    -- phonemes) to set a general (common) behaviour for the set of resulting values.
    Double ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the some group of representations (e. g. vowels).
    [Int] ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the special group of representations (e. g. soft sign).
    [Int] ->
    [Double]
minDurations lng mn1 mnSpecial mnG xs1 sps1 = map h [0 .. lng - 1]
  where
    xs2
        | maximum xs1 <= lng - 1 = filter (>= 0) xs1
        | otherwise =
            error "Aftovolio.RGLPK.General.objLine: Not defined for these arguments. "
    sps2
        | maximum sps1 <= lng - 1 = filter (>= 0) sps1 \\ xs2
        | otherwise =
            error "Aftovolio.RGLPK.General.objLine: Not defined for these arguments. "
    h i
        | i `elem` xs2 = mn1
        | i `elem` sps2 = mnSpecial
        | otherwise = mnG

maxDurations ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | Maximum duration of the phonetic language element representation in seconds.
    Double ->
    [Double]
maxDurations lng mx = replicate lng mx

{- | A variant of the more general 'answer2' where the predefined randomization parameters are used to produce every time being run
a new result (e. g. this allows to model accents).
-}
answer ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | The number of 'pairs'' function elements in the lists.
    Int ->
    -- | Actually the data type value that sets the behaviour of the 'pairs'' function.
    PairwiseC ->
    -- | Maximum duration of the phonetic language element representation in seconds.
    Double ->
    -- | List of pairs of indices that shows how the input data is related to the representation
    -- (which coefficients relates to which representation elements).
    [(Int, Int)] ->
    -- | A minimum positive duration value for some group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. vowels) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for some *special* group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. soft sign representation) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for all other phonetic language representations (usually, some sorts of
    -- phonemes) to set a general (common) behaviour for the set of resulting values.
    Double ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the some group of representations (e. g. vowels).
    [Int] ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the special group of representations (e. g. soft sign).
    [Int] ->
    -- | An array of coefficients.
    Array Int Double ->
    -- | An argument of the 'matrixLine' function.
    [String] ->
    [Double] ->
    [Double] ->
    -- | A sorted list of the 'Char' representations of the phonetic language phenomenae.
    String ->
    String
answer lng nn pw mx ts = answer2 lng nn pw mx ts (-0.003) 0.003 (-0.0012) 0.0012

answer2 ::
    -- | The length of the 'String' js that is a sorted list of the phonetic language representations as 'Char's that
    -- appears in the file with test words and their spoken durations.
    Int ->
    -- | The number of 'pairs'' function elements in the lists.
    Int ->
    -- | Actually the data type value that sets the behaviour of the 'pairs'' function.
    PairwiseC ->
    -- | Maximum duration of the phonetic language element representation in seconds.
    Double ->
    -- | List of pairs of indices that shows how the input data is related to the representation
    -- (which coefficients relates to which representation elements).
    [(Int, Int)] ->
    -- | A maximum in absolute value (being, usually, a negative one) possible random deviation from the computed value to be additionally applied to emulate
    -- 'more natural' behaviour and to get every time while running new sets of values.
    Double ->
    -- | A maximum in absolute value (being, usually, a positive one) possible random deviation from the computed value to be additionally applied to emulate
    -- 'more natural' behaviour and to get every time while running new sets of values.
    Double ->
    -- | A minimum in absolute value (being, usually, a negative one) possible random deviation from the computed value to be
    -- additionally applied to emulate 'more natural' behaviour and to get every time while running new sets of values.
    Double ->
    -- | A minimum in absolute value (being, usually, a positive one) possible random deviation from the computed value to be
    -- additionally applied to emulate 'more natural' behaviour and to get every time while running new sets of values.
    Double ->
    -- | A minimum positive duration value for some group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. vowels) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for some *special* group of phonetic language representation (usually, some sorts of
    -- phonemes, e. g. soft sign representation) to set some peculiar behaviour for the set of resulting values.
    Double ->
    -- | A minimum positive duration value for all other phonetic language representations (usually, some sorts of
    -- phonemes) to set a general (common) behaviour for the set of resulting values.
    Double ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the some group of representations (e. g. vowels).
    [Int] ->
    -- | A list of indices of the phonetic languages representations in their sorted in ascending order sequence that
    -- corresponds to the elements from the special group of representations (e. g. soft sign).
    [Int] ->
    -- | An array of coefficients.
    Array Int Double ->
    -- | An argument of the 'matrixLine' function.
    [String] ->
    [Double] ->
    [Double] ->
    -- | A sorted list of the 'Char' representations of the phonetic language phenomenae.
    String ->
    String
answer2 lng nn pw mx ts min1 max1 min2 max2 mn1 mnSpecial mnG xs1 sps1 lsts bss xs ys js =
    mconcat
        [ "library(\"Rglpk\")"
        , newLineEnding
        , objLine lng ts lsts
        , matrixLine nn pw bss js
        , dirLine lng nn bss js
        , rhsLine lng nn mx mn1 mnSpecial mnG xs1 sps1 xs ys
        , maxLine
        , newLineEnding
        , "k <- Rglpk_solve_LP(obj = obj1, mat = mat1, dir = dir1, rhs = rhs1, max = max1)"
        , newLineEnding
        , "y <- runif("
        , show lng
        , ", min = "
        , showFFloat Nothing (-(abs min1)) ", max = "
        , showFFloat Nothing (abs max1) ")"
        , newLineEnding
        , "if (k$status == 0){k$solution / mean(k$solution)} else {c()}"
        , newLineEnding
        , "\")}"
        ]

-- read ("SylS {charS=\'k\', phoneType=P 6")::PRS
