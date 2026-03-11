{-# LANGUAGE OverloadedStrings #-}

{-| A module to help with parsing VCF files. The VCF format is defined here:
<https://en.wikipedia.org/wiki/Variant_Call_Format>
-}

module SequenceFormats.VCF (VCFheader(..),
                     VCFentry(..),
                     vcfHeaderParser,
                     readVCFfromStdIn,
                     readVCFfromFile,
                     getGenotypes,
                     getDosages,
                     isTransversionSnp,
                     vcfToFreqSumEntry,
                     isBiallelicSnp,
                     printVCFtoStdOut,
                     writeVCFfile) where

import           SequenceFormats.FreqSum          (FreqSumEntry (..))
import           SequenceFormats.Utils            (Chrom (..),
                                                   SeqFormatException (..),
                                                   consumeProducer,
                                                   deflateFinaliser,
                                                   gzipConsumer,
                                                   readFileProdCheckCompress,
                                                   word, writeFromPopper)

import           Control.Applicative              ((<|>))
import           Control.Error                    (atErr)
import           Control.Monad                    (forM, unless, void)
import           Control.Monad.Catch              (MonadThrow, throwM)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Data.List                        (isSuffixOf)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Streaming.Zlib              as Z
import           Pipes                            (Consumer, Producer, (>->))
import           Pipes.Attoparsec                 (parse)
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (MonadSafe, register)
import qualified Pipes.Safe.Prelude               as PS
import           System.IO                        (IOMode (..))

-- |A datatype to represent the VCF Header. Most comments are simply parsed as entire lines, but the very last comment line, containing the sample names, is separated out
data VCFheader = VCFheader {
    vcfHeaderComments :: [B.ByteString], -- ^A list of containing all comments starting with a single '#'
    vcfSampleNames    :: [B.ByteString] -- ^The list of sample names parsed from the last comment line
                             -- starting with '##'
} deriving (Show, Eq)

-- |A Datatype representing a single VCF entry.
data VCFentry = VCFentry {
    vcfChrom        :: Chrom, -- ^The chromosome
    vcfPos          :: Int, -- ^The position
    vcfId           :: Maybe B.ByteString, -- ^The SNP ID if non-missing
    vcfRef          :: B.ByteString, -- ^ The reference allele (supports also multi-character alleles for Indels)
    vcfAlt          :: [B.ByteString], -- ^The alternative alleles, each one possible of multiple characters
    vcfQual         :: Maybe Double, -- ^The quality value
    vcfFilter       :: Maybe B.ByteString, -- ^The Filter value, if non-missing.
    vcfInfo         :: [B.ByteString], -- ^A list of Info fields
    vcfGenotypeInfo :: Maybe ([B.ByteString], [[B.ByteString]]) -- ^An optional tuple of format tags and genotype format fields for each sample.
} deriving (Show, Eq)

-- |reads a VCFheader and VCFentries from a text producer.
readVCFfromProd :: (MonadThrow m) =>
    Producer B.ByteString m () -> m (VCFheader, Producer VCFentry m ())
readVCFfromProd prod = do
    (res, rest) <- runStateT (parse vcfHeaderParser) prod
    header <- case res of
        Nothing        -> throwM $ SeqFormatException "VCF file exhausted prematurely"
        Just (Left e)  -> throwM (SeqFormatException (show e))
        Just (Right h) -> return h
    return (header, consumeProducer vcfEntryParser rest)

-- |Reading a VCF from StdIn. Returns a VCFHeader and a Producer over VCFentries.
readVCFfromStdIn :: (MonadIO m, MonadThrow m) => m (VCFheader, Producer VCFentry m ())
readVCFfromStdIn = readVCFfromProd PB.stdin

-- |Reading a VCF from a file. Returns a VCFHeader and a Producer over VCFentries.
readVCFfromFile :: (MonadSafe m) => FilePath -> m (VCFheader, Producer VCFentry m ())
readVCFfromFile = readVCFfromProd . readFileProdCheckCompress

vcfHeaderParser :: A.Parser VCFheader
vcfHeaderParser = VCFheader <$> A.many1' doubleCommentLine <*> (headerLineWithSamples <|> headerLineNoSamples)
  where
    doubleCommentLine = do
        c1 <- A.string "##"
        s_ <- A.takeWhile1 (/='\n')
        A.endOfLine
        return $ c1 <> s_
    headerLineWithSamples = do
        void $ A.string "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\t"
        sampleNames <- word `A.sepBy1'` A.char '\t'
        A.endOfLine
        return sampleNames
    headerLineNoSamples = A.string "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\n" *> pure []

vcfEntryParser :: A.Parser VCFentry
vcfEntryParser = vcfEntryParserFull <|> vcfEntryParserTruncated
  where
    vcfEntryParserFull = VCFentry <$> (Chrom <$> word) <* sp <*> A.decimal <* sp <*> parseId <*
        sp <*> word <* sp <*> parseAlternativeAlleles <* sp <*> parseQual <* sp <*> parseFilter <*
        sp <*> parseInfoFields <* sp <*> parseFormatStringsAndGenotypes <* A.endOfLine
    vcfEntryParserTruncated = VCFentry <$> (Chrom <$> word) <* sp <*> A.decimal <* sp <*> parseId <*
        sp <*> word <* sp <*> parseAlternativeAlleles <* sp <*> parseQual <* sp <*> parseFilter <*
        sp <*> parseInfoFields <*> pure Nothing <* A.endOfLine
    sp = A.satisfy (\c -> c == '\t')
    parseId = (parseDot *> pure Nothing) <|> (Just <$> word)
    parseDot = A.char '.'
    parseAlternativeAlleles = (parseDot *> pure []) <|> (parseAllele `A.sepBy1` A.char ',')
    parseAllele = A.takeTill (\c -> c == ',' || c == '\t')
    parseQual = (parseDot *> pure Nothing) <|> (Just <$> A.double)
    parseFilter = (parseDot *> pure Nothing) <|> (Just <$> word)
    parseInfoFields = (parseDot *> pure []) <|> (parseInfoField `A.sepBy1` A.char ';')
    parseInfoField = A.takeTill (\c -> c == ';' || c == '\t')
    parseFormatStringsAndGenotypes = (\f g -> Just (f, g)) <$> parseFormatStrings <* sp <*> parseGenotypeInfos 
    parseFormatStrings = parseFormatString `A.sepBy1` A.char ':'
    parseFormatString = A.takeTill (\c -> c == ':' || c == '\t')
    parseGenotypeInfos = parseGenotype `A.sepBy1` sp
    parseGenotype = parseGenoField `A.sepBy1` A.char ':'
    parseGenoField = A.takeTill (\c -> c == ':' || c == '\t' || c == '\n')

-- |returns True if the SNP is biallelic.
isBiallelicSnp :: B.ByteString -> [B.ByteString] -> Bool
isBiallelicSnp ref alt = validRef && validAlt
  where
    validRef = (ref `elem` ["A", "C", "G", "T"])
    validAlt = case alt of
        [alt'] -> alt' `elem` ["A", "C", "G", "T"]
        _      -> False

-- |returns True if the SNp is a biallelic Transversion SNP (i.e. one of G/T, G/C, A/T, A/C)
isTransversionSnp :: B.ByteString -> [B.ByteString] -> Bool
isTransversionSnp ref alt =
    case alt of
        [alt'] -> isBiallelicSnp ref alt && (not $ isTransition ref alt')
        _      -> False
  where
    isTransition r a = ((r == "A") && (a == "G")) || ((r == "G") && (a == "A")) ||
                       ((r == "C") && (a == "T")) || ((r == "T") && (a == "C"))

-- |Extracts the genotype fields (for each sapmle) from a VCF entry
getGenotypes :: (MonadThrow m) => VCFentry -> m [B.ByteString]
getGenotypes vcfEntry = case vcfGenotypeInfo vcfEntry of
    Nothing -> throwM $ SeqFormatException "No Genotypes in this VCF"
    Just (formatField, genotypeFields) -> do
        gtIndex <- case filter ((=="GT") . snd) . zip [0..] $ formatField of
            []  -> throwM $ SeqFormatException "GT format field not found"
            [i] -> return . fst $ i
            _   -> throwM $ SeqFormatException "Multiple GT fields specified in VCF format field"
        forM genotypeFields $ \indInfo ->
            case atErr ("cannot find genotype from " ++ show indInfo) indInfo gtIndex of
                Left e  -> throwM . SeqFormatException $ e
                Right g -> return g

-- |Extracts the dosages (the sum of non-reference alleles) and ploidies per sample
getDosages :: (MonadThrow m) => VCFentry -> m [Maybe (Int, Int)]
getDosages vcfEntry = do
    genotypes <- getGenotypes vcfEntry
    return $ do
        gen <- genotypes
        case B.splitWith (\c -> c == '|' || c == '/') gen of
            ["0"]      -> return $ Just (0, 1)
            ["1"]      -> return $ Just (1, 1)
            ["0", "0"] -> return $ Just (0, 2)
            ["0", "1"] -> return $ Just (1, 2)
            ["1", "0"] -> return $ Just (1, 2)
            ["1", "1"] -> return $ Just (2, 2)
            _          -> return Nothing

-- |Converts a VCFentry to the simpler FreqSum format
vcfToFreqSumEntry :: (MonadThrow m) => VCFentry -> m FreqSumEntry
vcfToFreqSumEntry vcfEntry = do
    unless (B.length (vcfRef vcfEntry) == 1) . throwM . SeqFormatException $
        "multi-site reference allele at " ++ show vcfEntry
    alt <- case vcfAlt vcfEntry of
        [] -> return 'N'
        (a:_) -> if B.length a /= 1
                 then
                    throwM . SeqFormatException $ "multi-site alternative allele at " ++ show vcfEntry
                 else
                    return $ B.head a
    let ref = B.head (vcfRef vcfEntry)
    dosages <- getDosages vcfEntry
    return $ FreqSumEntry (vcfChrom vcfEntry) (vcfPos vcfEntry) (vcfId vcfEntry) Nothing ref alt dosages

printVCFtoStdOut :: (MonadIO m) => VCFheader -> Consumer VCFentry m ()
printVCFtoStdOut vcfh = do
    liftIO . B.putStr . vcfHeaderToText $ vcfh
    P.map vcfEntryToText >-> PB.stdout

vcfHeaderToText :: VCFheader -> B.ByteString
vcfHeaderToText (VCFheader comments names) =
    let commentsBlock = B.intercalate "\n" comments
        namesLine = case names of
            [] -> "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO"
            _  -> "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\t" <> (B.intercalate "\t" names)
    in  commentsBlock <> "\n" <> namesLine <> "\n"

vcfEntryToText :: VCFentry -> B.ByteString
vcfEntryToText e =
    let baseFieldList = [
            unChrom . vcfChrom     $ e,
            B.pack . show . vcfPos $ e,
            fromMaybe "." . vcfId  $ e,
            vcfRef e,
            if null (vcfAlt e) then "." else B.intercalate "," . vcfAlt $ e,
            maybe "." (B.pack . show) . vcfQual $ e,
            fromMaybe "." . vcfFilter $ e,
            if null (vcfInfo e) then "." else B.intercalate ";" . vcfInfo $ e]
        genotypeFieldList = case vcfGenotypeInfo e of
            Nothing -> []
            Just (f, gs) -> [B.intercalate ":" f] ++ map (B.intercalate ":") gs
    in  (<> "\n") . B.intercalate "\t" $ baseFieldList ++ genotypeFieldList

writeVCFfile :: (MonadSafe m) => FilePath -> VCFheader -> Consumer VCFentry m ()
writeVCFfile vcfFile vcfh = do
    (_, vcfFileH) <- lift $ PS.openFile vcfFile WriteMode
    vcfOutConsumer <- if ".gz" `isSuffixOf` vcfFile then do
            def <- liftIO $ Z.initDeflate 6 (Z.WindowBits 31)
            _ <- register (deflateFinaliser def vcfFileH)
            pop <- liftIO (Z.feedDeflate def (vcfHeaderToText vcfh))
            liftIO (writeFromPopper pop vcfFileH)
            return $ gzipConsumer def vcfFileH
        else do
            liftIO $ B.hPut vcfFileH (vcfHeaderToText vcfh)
            return $ PB.toHandle vcfFileH
    P.map vcfEntryToText >-> vcfOutConsumer
