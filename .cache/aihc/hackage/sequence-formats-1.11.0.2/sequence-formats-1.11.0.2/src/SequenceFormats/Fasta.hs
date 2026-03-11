{-# LANGUAGE OverloadedStrings #-}

{-| Module to read and parse through a Fasta file. The Fasta format is defined here:
<https://en.wikipedia.org/wiki/FASTA_format>
-}
module SequenceFormats.Fasta (readNextFastaEntry, loadFastaChrom) where

import           SequenceFormats.Utils            (Chrom (..))

import           Control.Exception.Base           (AssertionFailed (..),
                                                   throwIO)
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Data.Char                        (isSpace)
import           Lens.Family2                     (view)
import           Pipes                            (Producer, next, runEffect,
                                                   (>->))
import           Pipes.Attoparsec                 (parse)
import qualified Pipes.ByteString                 as P
import           Pipes.Prelude                    (drain)
import           System.IO                        (Handle, hPutStr, stderr)

-- |A function to select out a specific chromosome from a Fasta File. Expects a file handle to the
-- file and a chromosome. Note that by Chromosome I simply denote a fasta header line, as is the
-- case for example for the human reference genome. Returns a Bytestring-Producer over the single sequence followed the specified header (the chromosome).
loadFastaChrom :: Handle -> Chrom -> IO (Producer B.ByteString IO ())
loadFastaChrom refFileHandle chrom = do
    let prod = P.fromHandle refFileHandle
    go prod
  where
    go prod = do
        (chrom_, prod') <- readNextFastaEntry prod
        hPutStr stderr ("found chromosome " <> show chrom_)
        if chrom_ == chrom
        then return (void prod')
        else do
            newProd <- runEffect $ prod' >-> drain
            go newProd

-- |This function takes a Bytestring-Producer over a Fasta-file, reads in the first header and then returns a produer over its sequence. The return of that producer is the Bytestring-Producer of the rest of the fasta file.
readNextFastaEntry :: (MonadIO m) => Producer B.ByteString m () ->
                      m (Chrom, Producer B.ByteString m (Producer B.ByteString m ()))
readNextFastaEntry prod = do
    (res, rest) <- runStateT (parse fastaHeaderLineParser) prod
    header <- case res of
        Nothing -> liftIO . throwIO $ AssertionFailed "Could not find chromosome. Fasta file exhausted."
        Just (Left e_) -> do
            x <- next rest
            case x of
                (Right (chunk, _)) -> do
                    let msg = show e_ ++ B.unpack chunk
                    liftIO . throwIO $ AssertionFailed ("Fasta header parsing error: " ++ msg)
                _ -> error "should not happen"
        Just (Right h) -> return h
    return (header, view (P.break (==62)) rest >-> P.filter (\c -> c /= 10 && c /= 13))
-- '>' == 62, '\n' == 10, \r == 13

fastaHeaderLineParser :: A.Parser Chrom
fastaHeaderLineParser = do
    _ <- A.char '>'
    chrom <- A.takeWhile $ not . isSpace
    A.skipWhile (\c -> c /= '\n' && c /= '\r')
    A.endOfLine
    return . Chrom $ chrom
