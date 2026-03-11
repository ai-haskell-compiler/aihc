{-# LANGUAGE NamedFieldPuns #-}
module Common(
  Dataset(..),
  englishDataset, wikiDataset
) where

import qualified Data.Vector as V
import Data.Word

import qualified System.Random.MWC                as R
import qualified System.Random.MWC.CondensedTable as R
import Control.DeepSeq
import Data.List (sort)

data Dataset = Dataset {
    dictA :: [String],
    dictB :: [String],
    dictAUnsorted :: [String],
    gibberish :: [String]
  }
  deriving (Show)

instance NFData Dataset where
  rnf Dataset{ dictA, dictB, dictAUnsorted, gibberish } = rnf dictA `seq` rnf dictB `seq` rnf dictAUnsorted `seq` rnf gibberish

englishDataset :: IO Dataset
englishDataset = do
  dictAmEn <- lines <$> readFile "/usr/share/dict/american-english"
  dictBrEn <- lines <$> readFile "/usr/share/dict/british-english"
  dictAmEnShuffled <- lines <$> readFile "benchdata/american-english-shuf"
  rand <- randomStrs 10003
  pure $ Dataset{ dictA = dictAmEn, dictB = dictBrEn, dictAUnsorted = dictAmEnShuffled, gibberish = rand }

wikiDataset :: IO Dataset
wikiDataset = do
  wiki1 <- lines <$> readFile "benchdata/externallinks.txt.1"
  wiki2 <- lines <$> readFile "benchdata/externallinks.txt.2"
  let wiki1Sorted = sort wiki1
      wiki2Sorted = sort wiki2
  rand <- randomStrs 10007
  pure $ Dataset{ dictA = wiki1Sorted, dictB = wiki2Sorted, dictAUnsorted = wiki1, gibberish = rand }

---

numRandomStr :: Int
numRandomStr = 1000

seed :: Word32 -> V.Vector Word32
seed w = V.fromList [1573289798, 32614861, w]

randomStrs :: Word32 -> IO [String]
randomStrs s =
  do g <- R.initialize (seed s)
     revReplicateM numRandomStr $ do
       n <- R.genFromTable distN g
       revReplicateM (n+1) (uniformAlphabet g)
  where
    distN = R.tableBinomial 12 0.33
    alphabet = V.fromList ['a' .. 'z']
    numAlphabet = V.length alphabet
    uniformAlphabet g = (alphabet V.!) <$> R.uniformR (0, numAlphabet-1) g

revReplicateM :: (Monad m) => Int -> m a -> m [a]
revReplicateM n ma = loop n []
  where
    loop 0 acc = return acc
    loop i acc = ma >>= \a -> loop (i-1) (a:acc)
