-- |
-- Module      :  ELynx.Sequence.Alignment
-- Description :  Multi sequence alignment related types and functions
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
--
-- Portability :  portable
--
-- Creation date: Thu Oct  4 18:40:18 2018.
--
-- This module is to be imported qualified.
module ELynx.Sequence.Alignment
  ( Alignment (..),
    length,
    nSequences,
    -- | * Input, output
    fromSequences,
    toSequences,
    summarize,
    -- | * Manipulation
    join,
    concat,
    concatAlignments,
    filterColsConstant,
    filterColsConstantSoft,
    filterColsOnlyStd,
    filterColsStd,
    filterColsNoGaps,
    -- | * Analysis
    FrequencyData,
    distribution,
    toFrequencyData,
    kEffEntropy,
    kEffHomoplasy,
    countIUPACChars,
    countGaps,
    countUnknowns,
    -- | * Sub sample
    subSample,
    randomSubSample,
  )
where

import Control.Monad hiding (join)
import Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List hiding
  ( concat,
    length,
  )
import qualified Data.Matrix.Unboxed as M
import qualified Data.Vector.Unboxed as V
import qualified ELynx.Alphabet.Alphabet as A
import ELynx.Alphabet.Character
import qualified ELynx.Alphabet.DistributionDiversity as D
import ELynx.Sequence.Defaults
import qualified ELynx.Sequence.Sequence as S
import System.Random.Stateful
import Prelude hiding
  ( concat,
    length,
  )

-- | A collection of sequences.
data Alignment = Alignment
  { names :: [S.Name],
    descriptions :: [S.Description],
    alphabet :: A.Alphabet,
    matrix :: M.Matrix Character
  }
  deriving (Show, Eq)

-- | Number of sites.
length :: Alignment -> Int
length = M.cols . matrix

-- | Number of sequences.
nSequences :: Alignment -> Int
nSequences = M.rows . matrix

-- | Create 'Alignment' from a list of 'S.Sequence's.
fromSequences :: [S.Sequence] -> Either String Alignment
fromSequences ss
  | S.equalLength ss && allEqual (map S.alphabet ss) =
      Right $
        Alignment ns ds a d
  | S.equalLength ss = Left "Sequences do not have equal codes."
  | otherwise = Left "Sequences do not have equal lengths."
  where
    ns = map S.name ss
    ds = map S.description ss
    a = S.alphabet $ head ss
    bss = map S.characters ss
    d = M.fromRows bss
    allEqual [] = True
    allEqual xs = all (== head xs) $ tail xs

-- | Conversion to list of 'S.Sequence's.
toSequences :: Alignment -> [S.Sequence]
toSequences (Alignment ns ds a da) =
  zipWith3
    (\n d r -> S.Sequence n d a r)
    ns
    ds
    rows
  where
    rows = M.toRows da

header :: Alignment -> BL.ByteString
header a =
  BL.unlines $
    [ BL.pack "Multi sequence alignment.",
      BL.pack $ "Code: " ++ A.alphabetDescription (alphabet a) ++ ".",
      BL.pack $ "Length: " ++ show (length a) ++ "."
    ]
      ++ reportLengthSummary
      ++ reportNumberSummary
  where
    reportLengthSummary =
      [ BL.pack $
          "For each sequence, the "
            ++ show summaryLength
            ++ " first bases are shown."
        | length a > summaryLength
      ]
    reportNumberSummary =
      [ BL.pack $
          show summaryNSequences
            ++ " out of "
            ++ show (nSequences a)
            ++ " sequences are shown."
        | nSequences a > summaryNSequences
      ]

-- | Similar to 'S.summarizeSequenceList' but with different Header.
summarize :: Alignment -> BL.ByteString
summarize a = header a <> S.body (toSequences a)

-- Vertical concatenation.
(===) :: (V.Unbox a) => M.Matrix a -> M.Matrix a -> M.Matrix a
(===) l r = M.fromRows $ lRs ++ rRs
  where
    lRs = M.toRows l
    rRs = M.toRows r

-- Horizontal concatenation.
(|||) :: (V.Unbox a) => M.Matrix a -> M.Matrix a -> M.Matrix a
(|||) l r = M.fromColumns $ lCs ++ rCs
  where
    lCs = M.toColumns l
    rCs = M.toColumns r

-- | Join two 'Alignment's vertically. That is, add more sequences
-- to an alignment. See also 'concat'.
join :: Alignment -> Alignment -> Alignment
-- top bottom.
join t b
  | length t /= length b =
      error
        "join: Multi sequence alignments do not have equal lengths."
  | alphabet t /= alphabet b =
      error
        "join: Multi sequence alignments do not have equal alphabets."
  | otherwise = Alignment ns ds al (tD === bD)
  where
    ns = names t ++ names b
    ds = descriptions t ++ descriptions b
    tD = matrix t
    bD = matrix b
    al = alphabet t

-- | Concatenate two 'Alignment's horizontally. That is, add more
-- sites to an alignment. See also 'join'.
concat :: Alignment -> Alignment -> Alignment
-- left right.
concat l r
  | nSequences l /= nSequences r =
      error
        "concat: Multi sequence alignments do not have an equal number of sequences."
  | alphabet l /= alphabet r =
      error "concat: Multi sequence alignments do not have an equal alphabets."
  | names l /= names r =
      error "concat: Multi sequence alignments do not have an equal names."
  | descriptions l /= descriptions r =
      error "concat: Multi sequence alignments do not have an equal descriptions."
  | otherwise =
      Alignment (names l) (descriptions l) (alphabet l) (lD ||| rD)
  where
    lD = matrix l
    rD = matrix r

-- | Concatenate a list of 'Alignment's horizontally. See
-- 'concat'.
concatAlignments :: [Alignment] -> Alignment
concatAlignments [] = error "concatAlignments: Nothing to concatenate."
concatAlignments [a] = a
concatAlignments as = foldl' concat (head as) (tail as)

-- Only keep columns from alignment that satisfy given predicate.
filterColsWith :: (V.Vector Character -> Bool) -> Alignment -> Alignment
filterColsWith p a = a {matrix = m'}
  where
    m' = M.fromColumns . filter p . M.toColumns $ matrix a

-- | Only keep constant columns.
filterColsConstant :: Alignment -> Alignment
filterColsConstant = filterColsWith (\v -> V.all (== V.head v) v)

-- | Only keep constant columns, and constant columns with at least one standard
-- character as well as any number of gaps or unknowns.
filterColsConstantSoft :: Alignment -> Alignment
filterColsConstantSoft a = filterColsWith f a
  where
    al = alphabet a
    f v = case V.find (A.isStd al) v of
      Nothing -> False
      Just c -> V.all (\x -> x == c || A.isGap al x || A.isUnknown al x) v

-- | Only keep columns with standard characters. Alignment columns with IUPAC
-- characters are removed.
filterColsOnlyStd :: Alignment -> Alignment
filterColsOnlyStd a = filterColsWith (V.all $ A.isStd (alphabet a)) a

-- | Filter columns with proportion of standard character larger than given number.
filterColsStd :: Double -> Alignment -> Alignment
filterColsStd prop a =
  filterColsWith
    (\col -> prop * n <= fromIntegral (V.length (V.filter (A.isStd al) col)))
    a
  where
    al = alphabet a
    n = fromIntegral $ nSequences a

-- | Only keep columns without gaps or unknown characters.
filterColsNoGaps :: Alignment -> Alignment
filterColsNoGaps a = filterColsWith (V.all $ not . A.isGap (alphabet a)) a

-- | Frequency data; do not store the actual characters, but their frequencies.
-- The matrix is of size @N x K@, where @N@ is the number of sites, and @K@ is
-- the number of characters.
type FrequencyData = M.Matrix Double

-- Map a function on each column of a DIM2 array; parallel version with given chunk size.
fMapColParChunk ::
  (V.Unbox a, V.Unbox b) =>
  Int ->
  (V.Vector a -> V.Vector b) ->
  M.Matrix a ->
  M.Matrix b
fMapColParChunk n f m =
  M.fromColumns (map f (M.toColumns m) `using` parListChunk n rseq)

-- | Calculcate frequency of characters at each site of a multi sequence alignment.
toFrequencyData :: Alignment -> FrequencyData
toFrequencyData a = fMapColParChunk 100 (D.frequencyCharacters spec) (matrix a)
  where
    spec = A.alphabetSpec (alphabet a)

-- | Calculate the distribution of characters.
distribution :: FrequencyData -> [Double]
distribution fd =
  map (/ fromIntegral nSites) $
    V.toList $
      foldl1
        (V.zipWith (+))
        (M.toColumns fd)
  where
    nSites = M.cols fd

-- Parallel map with given chunk size.
parMapChunk :: Int -> (a -> b) -> [a] -> [b]
parMapChunk n f as = map f as `using` parListChunk n rseq

chunksize :: Int
chunksize = 500

-- | Diversity analysis. See 'kEffEntropy'.
kEffEntropy :: FrequencyData -> [Double]
kEffEntropy fd = parMapChunk chunksize D.kEffEntropy (M.toColumns fd)

-- | Diversity analysis. See 'kEffEntropy'.
kEffHomoplasy :: FrequencyData -> [Double]
kEffHomoplasy fd = parMapChunk chunksize D.kEffHomoplasy (M.toColumns fd)

-- | Count the number of standard (i.e., not extended IUPAC) characters in the
-- alignment.
countIUPACChars :: Alignment -> Int
countIUPACChars a = V.length . V.filter (A.isIUPAC (alphabet a)) $ allChars
  where
    allChars = M.flatten $ matrix a

-- | Count the number of gaps in the alignment.
countGaps :: Alignment -> Int
countGaps a = V.length . V.filter (A.isGap (alphabet a)) $ allChars
  where
    allChars = M.flatten $ matrix a

-- | Count the number of unknown characters in the alignment.
countUnknowns :: Alignment -> Int
countUnknowns a = V.length . V.filter (A.isUnknown (alphabet a)) $ allChars
  where
    allChars = M.flatten $ matrix a

-- Sample the given sites from a matrix.
subSampleMatrix :: (V.Unbox a) => [Int] -> M.Matrix a -> M.Matrix a
subSampleMatrix is m =
  M.fromColumns $ foldl' (\a i -> M.takeColumn m i : a) [] (reverse is)

-- | Sample the given sites from a multi sequence alignment.
subSample :: [Int] -> Alignment -> Alignment
subSample is a = a {matrix = m'} where m' = subSampleMatrix is $ matrix a

-- | Randomly sample a given number of sites of the multi sequence alignment.
randomSubSample :: (StatefulGen g m) => Int -> Alignment -> g -> m Alignment
randomSubSample n a g = do
  let l = length a
  is <- replicateM n $ uniformRM (0, l - 1) g
  return $ subSample is a
