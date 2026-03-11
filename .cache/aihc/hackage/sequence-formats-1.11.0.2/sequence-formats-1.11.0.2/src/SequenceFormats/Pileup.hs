{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Pileup (readPileupFromStdIn, readPileupFromFile, PileupRow(..), Strand(..)) where

import           Control.Monad.Catch              (MonadThrow)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Data.Char                        (toUpper)
import           Pipes                            (Producer)
import qualified Pipes.ByteString                 as PB
import           Pipes.Safe                       (MonadSafe)

import           SequenceFormats.Utils            (Chrom (..), consumeProducer,
                                                   readFileProd, word)

-- |A datatype to represent the strand orientation of a single base.
data Strand = ForwardStrand | ReverseStrand deriving (Eq, Show)

-- |A datatype to represent a single pileup row for multiple individuals.
-- The constructor arguments are: Chromosome, Position, Refererence Allelele,
-- Pileup String per individual
data PileupRow = PileupRow {
    pileupChrom      :: Chrom, -- ^The chromosome
    pileupPos        :: Int, -- ^The position
    pileupRef        :: Char, -- ^The reference base
    pileupBases      :: [String], -- ^The base string
    pileupStrandInfo :: [[Strand]]
 } deriving (Eq, Show)

-- |Read a pileup-formatted file from StdIn, for reading from an
-- external command `samtools mpileup`.
readPileupFromStdIn :: (MonadIO m, MonadThrow m) => Producer PileupRow m ()
readPileupFromStdIn = consumeProducer pileupParser PB.stdin

-- |Read pileup from a file.
readPileupFromFile :: (MonadSafe m) => FilePath -> Producer PileupRow m ()
readPileupFromFile = consumeProducer pileupParser . readFileProd

pileupParser :: A.Parser PileupRow
pileupParser = do
    chrom <- word
    _ <- A.space
    pos <- A.decimal
    _ <- A.space
    refA <- toUpper <$> A.letter_ascii
     -- we used to parse only ACTGN and M and the small-letter versions of those,
     -- but it turns out that there can be more letter from the IUPAC alphabet that can occur in
     -- fasta reference files, so here we're parsing them all in principle.
     -- Also, for some reason, there is an M in the human reference at
     -- position 3:60830534 (both in hs37d5 and in hg19)
    _ <- A.space
    baseAndStrandEntries <- parsePileupPerSample refA `A.sepBy1`
        A.satisfy (\c -> c == ' ' || c == '\t')
    A.endOfLine
    let baseStrings = map fst baseAndStrandEntries
        strandInfoStrings = map snd baseAndStrandEntries
    let ret = PileupRow (Chrom chrom) pos refA baseStrings strandInfoStrings
    --trace (show ret) $ return ret
    return ret
  where
    parsePileupPerSample refA =
        processPileupEntry refA <$> A.decimal <* A.space <*> word <* A.space <* word

processPileupEntry :: Char -> Int -> B.ByteString -> (String, [Strand])
processPileupEntry refA cov readBaseString =
    if cov == 0 then ("", []) else
        let res = go (B.unpack readBaseString)
        in  (map fst res, map snd res)
  where
    go (x:xs)
        | x == '.' = (refA, ForwardStrand) : go xs
        | x == ',' = (refA, ReverseStrand) : go xs
        | x `elem` ("ACTGN" :: String) = (x, ForwardStrand) : go xs
        | x `elem` ("actgn" :: String) = (toUpper x, ReverseStrand) : go xs
        | x `elem` ("$*#<>" :: String) = go xs
        | x == '^' = go (drop 1 xs) -- skip the next character, which is the mapping quality
        | x == '+' || x == '-' =  -- insertions or deletions, followed by a decimal number
            case reads xs of
                [(num, rest)] -> go (drop num rest)
                _ -> error $ "cannot parse read base string: " ++ (x:xs)
        | otherwise = error $ "cannot parse read base string: " ++ (x:xs)
    go [] = []
