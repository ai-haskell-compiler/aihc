{-# LANGUAGE OverloadedStrings #-}

{-|Module to read and parse Eigenstrat-formatted genotype data. The Eigenstrat format is defined at <https://github.com/argriffing/eigensoft/blob/master/CONVERTF/README>.

-}

module SequenceFormats.Eigenstrat (EigenstratSnpEntry(..), EigenstratIndEntry(..),
    readEigenstratInd, GenoEntry(..), GenoLine, Sex(..), parseSex,
    readEigenstratSnpStdIn, readEigenstratSnpFile,
    readEigenstrat, writeEigenstrat, writeEigenstratIndFile, writeEigenstratSnp,
    writeEigenstratGeno) where

import           SequenceFormats.Utils            (Chrom (..),
                                                   SeqFormatException (..),
                                                   consumeProducer,
                                                   deflateFinaliser,
                                                   gzipConsumer,
                                                   readFileProdCheckCompress,
                                                   word)

import           Control.Applicative              ((<|>))
import           Control.Exception                (throw)
import           Control.Monad                    (forM_, void)
import           Control.Monad.Catch              (MonadThrow)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Class        (lift)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Data.List                        (isSuffixOf)
import qualified Data.Streaming.Zlib              as Z
import           Data.Vector                      (Vector, fromList, toList)
import           Pipes                            (Consumer, Pipe, Producer,
                                                   cat, for, yield, (>->))
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (MonadSafe, register)
import qualified Pipes.Safe.Prelude               as PS
import           System.IO                        (IOMode (..),
                                                   withFile)

-- |A datatype to represent a single genomic SNP. The constructor arguments are:
-- Chromosome, Position, Reference Allele, Alternative Allele.
data EigenstratSnpEntry = EigenstratSnpEntry
    { snpChrom      :: Chrom
    , snpPos        :: Int
    , snpGeneticPos :: Double
    , snpId         :: B.ByteString
    , snpRef        :: Char
    , snpAlt        :: Char
    }
    deriving (Eq, Show)

-- |A datatype to represent a single individual. The constructor arguments are:
-- Name, Sex and Population Name
data EigenstratIndEntry = EigenstratIndEntry B.ByteString Sex B.ByteString
    deriving (Eq, Show)

-- |A datatype to represent Sex in an Eigenstrat Individual file
data Sex = Male
    | Female
    | Unknown
    deriving (Eq, Show)

-- |A datatype to represent the genotype of an individual at a SNP.
data GenoEntry = HomRef
    | Het
    | HomAlt
    | Missing
    deriving (Eq, Show)

-- |Vector of the genotypes of all individuals at a single SNP.
type GenoLine = Vector GenoEntry

eigenstratSnpParser :: A.Parser EigenstratSnpEntry
eigenstratSnpParser = do
    snpId_ <- A.skipMany A.space >> word
    chrom <- A.skipMany1 A.space >> word
    geneticPos <- A.skipMany1 A.space >> A.double
    pos <- A.skipMany1 A.space >> A.decimal
    ref <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGNX")
    alt <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGNX")
    void A.endOfLine
    return $ EigenstratSnpEntry (Chrom chrom) pos geneticPos snpId_ ref alt

eigenstratIndParser :: A.Parser EigenstratIndEntry
eigenstratIndParser = do
    A.skipMany A.space
    name <- word
    A.skipMany1 A.space
    sex <- parseSex
    A.skipMany1 A.space
    popName <- word
    void A.endOfLine
    return $ EigenstratIndEntry name sex popName

parseSex :: A.Parser Sex
parseSex = parseMale <|> parseFemale <|> parseUnknown
  where
    parseMale = A.char 'M' >> return Male
    parseFemale = A.char 'F' >> return Female
    parseUnknown = A.char 'U' >> return Unknown

-- |Function to read an Eigenstrat individual file. Returns the Eigenstrat Individual Entries as list.
readEigenstratInd :: (MonadIO m) => FilePath -> m [EigenstratIndEntry]
readEigenstratInd fn =
    liftIO . withFile fn ReadMode $ \handle ->
        P.toListM $ consumeProducer eigenstratIndParser (PB.fromHandle handle)

eigenstratGenoParser :: A.Parser GenoLine
eigenstratGenoParser = do
    line <- A.takeWhile1 isValidNum
    void A.endOfLine
    return . fromList $ do
        l <- B.unpack line
        case l of
            '0' -> return HomAlt
            '1' -> return Het
            '2' -> return HomRef
            '9' -> return Missing
            _   -> error "this should never happen"
  where
    isValidNum c = c == '0' || c == '1' || c == '2' || c == '9'

-- |Function to read a Snp File from StdIn. Returns a Pipes-Producer over the EigenstratSnpEntries.
readEigenstratSnpStdIn :: (MonadThrow m, MonadIO m) => Producer EigenstratSnpEntry m ()
readEigenstratSnpStdIn = consumeProducer eigenstratSnpParser PB.stdin

-- |Function to read a Snp File from a file. Returns a Pipes-Producer over the EigenstratSnpEntries.
readEigenstratSnpFile :: (MonadSafe m) => FilePath -> Producer EigenstratSnpEntry m ()
readEigenstratSnpFile = consumeProducer eigenstratSnpParser . readFileProdCheckCompress

-- |Function to read a Geno File from a file. Returns a Pipes-Producer over the GenoLines.
readEigenstratGenoFile :: (MonadSafe m) => FilePath -> Producer GenoLine m ()
readEigenstratGenoFile = consumeProducer eigenstratGenoParser . readFileProdCheckCompress

-- |Function to read a full Eigenstrat database from files. Returns a pair of the Eigenstrat Individual Entries, and a joint Producer over the snp entries and the genotypes.
readEigenstrat :: (MonadSafe m) => FilePath -- ^The Genotype file
               -> FilePath -- ^The Snp File
               -> FilePath -- ^The Ind file
               -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ()) -- The return pair of individual entries and a joint Snp/Geno Producer.
readEigenstrat genoFile snpFile indFile = do
    indEntries <- readEigenstratInd indFile
    let snpProd = readEigenstratSnpFile snpFile
        genoProd = readEigenstratGenoFile genoFile >-> validateEigenstratEntries (length indEntries)
    return (indEntries, P.zip snpProd genoProd)

validateEigenstratEntries :: (MonadThrow m) => Int -> Pipe GenoLine GenoLine m ()
validateEigenstratEntries nr = for cat $ \line ->
    if length line /= nr
    then do
        let msg = "inconsistent nr of genotypes (" <> show (length line) <> ", but should be " <> show nr <> ") in \
                \genotype line " <> show line
        throw (SeqFormatException msg)
    else
        yield line

-- |Function to write an Eigenstrat Ind file.
writeEigenstratIndFile :: (MonadIO m) => FilePath -> [EigenstratIndEntry] -> m ()
writeEigenstratIndFile f indEntries =
    liftIO . withFile f WriteMode $ \h ->
        forM_ indEntries $ \(EigenstratIndEntry name sex popName) ->
            B.hPutStrLn h $ name <> "\t" <> sexToStr sex <> "\t" <> popName
  where
    sexToStr sex = case sex of
        Male    -> "M"
        Female  -> "F"
        Unknown -> "U"

-- |Function to write an Eigenstrat Snp File. Returns a consumer expecting EigenstratSnpEntries.
writeEigenstratSnp :: (MonadSafe m) => FilePath -- ^The Eigenstrat Snp File.
    -> Consumer EigenstratSnpEntry m () -- ^A consumer to read EigenstratSnpEntries
writeEigenstratSnp snpFile = do
    (_, snpFileH) <- lift $ PS.openFile snpFile WriteMode
    snpOutTextConsumer <- if ".gz" `isSuffixOf` snpFile then do
            def <- liftIO $ Z.initDeflate 6 (Z.WindowBits 31)
            _ <- register (deflateFinaliser def snpFileH)
            return $ gzipConsumer def snpFileH
        else
            return $ PB.toHandle snpFileH
    let toTextPipe = P.map (\(EigenstratSnpEntry chrom pos gpos gid ref alt) ->
            let snpLine = B.intercalate "\t" [gid, unChrom chrom, B.pack (show gpos),
                    B.pack (show pos), B.singleton ref, B.singleton alt]
            in  snpLine <> "\n")
    toTextPipe >-> snpOutTextConsumer

-- |Function to write an Eigentrat Geno File. Returns a consumer expecting Eigenstrat Genolines.
writeEigenstratGeno :: (MonadSafe m) => FilePath -- ^The Genotype file
    -> Consumer GenoLine m () -- ^A consumer to read Genotype entries.
writeEigenstratGeno genoFile = do
    (_, genoFileH) <- lift $ PS.openFile genoFile WriteMode
    genoOutTextConsumer <- if ".gz" `isSuffixOf` genoFile then do
            def <- liftIO $ Z.initDeflate 6 (Z.WindowBits 31)
            _ <- register (deflateFinaliser def genoFileH)
            return $ gzipConsumer def genoFileH
        else
            return $ PB.toHandle genoFileH
    let toTextPipe = P.map (\genoLine ->
            let genoLineStr = B.concat . map (B.pack . show . toEigenStratNum) . toList $ genoLine
            in  genoLineStr <> "\n")
    toTextPipe >-> genoOutTextConsumer
  where
    toEigenStratNum c = case c of
        HomRef  -> 2 :: Int
        Het     -> 1
        HomAlt  -> 0
        Missing -> 9

-- |Function to write an Eigenstrat Database. Returns a consumer expecting joint Snp- and Genotype lines.
writeEigenstrat :: (MonadSafe m) => FilePath -- ^The Genotype file
                -> FilePath -- ^The Snp File
                -> FilePath -- ^The Ind file
                -> [EigenstratIndEntry] -- ^The list of individual entries
                -> Consumer (EigenstratSnpEntry, GenoLine) m () -- ^A consumer to read joint Snp/Genotype entries.
writeEigenstrat genoFile snpFile indFile indEntries = do
    liftIO $ writeEigenstratIndFile indFile indEntries
    let snpOutConsumer  = writeEigenstratSnp  snpFile
        genoOutConsumer = writeEigenstratGeno genoFile
    P.tee (P.map fst >-> snpOutConsumer) >-> P.map snd >-> genoOutConsumer

