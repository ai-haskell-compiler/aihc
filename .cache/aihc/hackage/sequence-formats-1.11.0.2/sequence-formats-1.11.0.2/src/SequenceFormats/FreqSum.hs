{-# LANGUAGE OverloadedStrings #-}

{-| Module to parse and write freqSum files. The freqsum format is defined here:
<https://rarecoal-docs.readthedocs.io/en/latest/rarecoal-tools.html#vcf2freqsum>
-}

module SequenceFormats.FreqSum (readFreqSumStdIn, readFreqSumFile, FreqSumEntry(..),
    FreqSumHeader(..), printFreqSumStdOut, printFreqSumFile, freqSumEntryToText) where

import           SequenceFormats.Utils            (Chrom (..), consumeProducer,
                                                   readFileProd)

import           Control.Applicative              ((<|>))
import           Control.Monad.Catch              (MonadThrow, throwM)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Data.Char                        (isAlphaNum, isSpace)
import           Pipes                            (Consumer, Producer, (>->))
import           Pipes.Attoparsec                 (ParsingError (..), parse)
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (MonadSafe)
import           Pipes.Safe.Prelude               (withFile)
import           Prelude                          hiding (putStr)
import           System.IO                        (IOMode (..))

-- |A Datatype representing the Header
data FreqSumHeader = FreqSumHeader {
    fshNames  :: [String], -- ^A list of individual or group names
    fshCounts :: [Int] -- ^A list of haplotype counts per individual/group.
} deriving (Eq, Show)

freqSumHeaderToText :: FreqSumHeader -> B.ByteString
freqSumHeaderToText (FreqSumHeader names nCounts) =
    "#CHROM\tPOS\tREF\tALT\t" <> B.intercalate "\t" tuples <> "\n"
  where
    tuples = zipWith (\n c -> B.pack n <> "(" <> B.pack (show c) <> ")") names nCounts

-- |A Datatype to denote a single freqSum line
data FreqSumEntry = FreqSumEntry {
    fsChrom      :: Chrom, -- ^The chromosome of the site
    fsPos        :: Int, -- ^The position of the site
    fsSnpId      :: Maybe B.ByteString, -- ^An optional parameter to take the snpId. This is not parsed from or printed to freqSum format but is used in internal conversions from Eigenstrat.
    fsGeneticPos :: Maybe Double, -- ^An optional parameter to take the genetic pos. This is not parsed from or printed to freqSum format but is used in internal conversions from Eigenstrat.
    fsRef        :: Char, -- ^The reference allele
    fsAlt        :: Char, -- ^The alternative allele
    fsCounts     :: [Maybe (Int, Int)] -- ^A list of tuples with non-reference allele counts in each group and the total allele count. Nothing denotes missing data.
} deriving (Eq, Show)

-- |This function converts a single freqSum entry to a printable freqSum line.
freqSumEntryToText :: FreqSumEntry -> B.ByteString
freqSumEntryToText (FreqSumEntry chrom pos _ _ ref alt maybeCounts) =
    B.intercalate "\t" [unChrom chrom, B.pack (show pos), B.singleton ref, B.singleton alt, countStr] <> "\n"
  where
    countStr = B.intercalate "\t" . map (B.pack . show . maybe (-1) fst) $ maybeCounts

readFreqSumProd :: (MonadThrow m) =>
    Producer B.ByteString m () -> m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumProd prod = do
    (res, rest) <- runStateT (parse parseFreqSumHeader) prod
    header <- case res of
        Nothing        -> throwM $ ParsingError [] "freqSum file exhausted"
        Just (Left e)  -> throwM e
        Just (Right h) -> return h
    return (header, consumeProducer (parseFreqSumEntry (fshCounts header)) rest)

-- |A function to read a freqsum file from StdIn. Returns a pair of a freqSum Header and a Producer over all lines.
readFreqSumStdIn :: (MonadIO m, MonadThrow m) => m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumStdIn = readFreqSumProd PB.stdin

-- |A function to read a freqsum file from a file. Returns a pair of a freqSum Header and a Producer over all lines.
readFreqSumFile :: (MonadSafe m) => FilePath -> m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumFile = readFreqSumProd . readFileProd

parseFreqSumHeader :: A.Parser FreqSumHeader
parseFreqSumHeader = do
    tuples <- A.string "#CHROM\tPOS\tREF\tALT\t" >> A.sepBy' tuple A.space <* A.endOfLine
    let names = map fst tuples
        counts = map snd tuples
    return $ FreqSumHeader (map B.unpack names) counts
  where
    tuple = (,) <$> A.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-') <* A.char '(' <*> A.decimal <* A.char ')'

parseFreqSumEntry :: [Int] -> A.Parser FreqSumEntry
parseFreqSumEntry denom = FreqSumEntry <$> (Chrom <$> A.takeTill isSpace) <* A.skipSpace <*> A.decimal <*
    A.skipSpace <*> pure Nothing <*> pure Nothing <*> base <* A.skipSpace <*> baseOrDot <* A.skipSpace <*> counts <* A.endOfLine
  where
    counts = do
        rawCounts <- (parseMissing <|> parseCount) `A.sepBy` A.char '\t'
        return $ map (\(c, d) -> fmap (\n -> (n, d)) c) (zip rawCounts denom)
    parseMissing = A.string "-1" *> pure Nothing
    parseCount = Just <$> A.decimal
    base = A.satisfy (A.inClass "ACTGN")
    baseOrDot = A.satisfy (A.inClass "ACTG.")

-- |A function to write freqSum data to StdOut. Expects the freqSum header as argument, and then returns a Consumer that accepts freqSum entries.
printFreqSumStdOut :: (MonadIO m) => FreqSumHeader -> Consumer FreqSumEntry m ()
printFreqSumStdOut fsh = do
    liftIO . B.putStr . freqSumHeaderToText $ fsh
    P.map freqSumEntryToText >-> PB.stdout

-- |A function that writes a freqSum file. Expects the FilePath and the freqSum header as arguments, and then returns a Consumer that accepts freqSum entries.
printFreqSumFile :: (MonadSafe m) => FilePath -> FreqSumHeader -> Consumer FreqSumEntry m ()
printFreqSumFile outFile fsh = withFile outFile WriteMode go
  where
    go h = do
        liftIO . B.hPutStr h . freqSumHeaderToText $ fsh
        P.map freqSumEntryToText >-> PB.toHandle h
