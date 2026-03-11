module SequenceFormats.Bed where

import           SequenceFormats.Utils            (Chrom (..), consumeProducer,
                                                   readFileProd)

import           Control.Monad.Trans.Class        (lift)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Char                        (isSpace)
import           Pipes                            (Producer, next, yield)
import           Pipes.Safe                       (MonadSafe)

data BedEntry = BedEntry Chrom Int Int
    deriving (Show, Eq)

bedFileParser :: A.Parser BedEntry
bedFileParser = BedEntry <$> chrom <* A.skipSpace <*> A.decimal <* A.skipSpace <*> A.decimal <* A.endOfLine
  where
    chrom = Chrom <$> A.takeTill isSpace

readBedFile :: (MonadSafe m) => FilePath -> Producer BedEntry m ()
readBedFile bedFile = consumeProducer bedFileParser (readFileProd bedFile)

data IntervalStatus = BedBehind
    | BedOn
    | BedAhead

filterThroughBed :: (Monad m) => Producer BedEntry m () -> (a -> (Chrom, Int)) -> Producer a m () -> Producer a m ()
filterThroughBed bedProd posFunc gProd = do
    b <- lift $ next bedProd
    let (bedCurrent, bedRest) = case b of
            Left _  -> error "Bed file empty or not readable"
            Right r -> r
    f' <- lift $ next gProd
    let (gCurrent, gRest) = case f' of
            Left _  -> error "Genomic stream empty or not readable"
            Right r -> r
    go bedCurrent gCurrent bedRest gRest
  where
    go bedCurrent gCurrent bedRest gRest = do
        let recurseNextBed = do
                b <- lift $ next bedRest
                case b of
                    Left () -> return ()
                    Right (nextBed, bedRest') -> go nextBed gCurrent bedRest' gRest
            recurseNextG = do
                f' <- lift $ next gRest
                case f' of
                    Left ()               -> return ()
                    Right (nextG, gRest') -> go bedCurrent nextG bedRest gRest'
        case bedCurrent `checkIntervalStatus` gCurrent of
            BedBehind -> recurseNextBed
            BedAhead -> recurseNextG
            BedOn -> do
                yield gCurrent
                recurseNextG
    -- checkIntervalStatus :: BedEntry -> a -> IntervalStatus
    checkIntervalStatus (BedEntry bedChrom bedStart bedEnd) g =
        case bedChrom `compare` (fst . posFunc) g of
            LT -> BedBehind
            GT -> BedAhead
            EQ -> if bedStart + 1 > (snd . posFunc) g then
                      BedAhead
                  else
                      if bedEnd < (snd . posFunc) g then BedBehind else BedOn
