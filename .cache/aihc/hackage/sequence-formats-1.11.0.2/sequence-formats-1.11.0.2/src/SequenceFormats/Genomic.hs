module SequenceFormats.Genomic where

import           SequenceFormats.Bed        (BedEntry (..), filterThroughBed)
import           SequenceFormats.Eigenstrat (EigenstratSnpEntry (..))
import           SequenceFormats.FreqSum    (FreqSumEntry (..))
import           SequenceFormats.Pileup     (PileupRow (..))
import           SequenceFormats.Utils      (Chrom)
import           SequenceFormats.VCF        (VCFentry (..))

import           Pipes                      (Producer)

class Genomic a where
    genomicPosition :: a -> (Chrom, Int)

    genomicChrom :: a -> Chrom
    genomicChrom = fst . genomicPosition

    genomicBase :: a -> Int
    genomicBase = snd . genomicPosition

instance Genomic EigenstratSnpEntry where
    genomicPosition (EigenstratSnpEntry c p _ _ _ _) = (c, p)

instance Genomic FreqSumEntry where
    genomicPosition (FreqSumEntry c p _ _ _ _ _) = (c, p)

instance Genomic PileupRow where
    genomicPosition (PileupRow c p _ _ _) = (c, p)

instance Genomic VCFentry where
    genomicPosition (VCFentry c p _ _ _ _ _ _ _) = (c, p)

chromFilter :: (Genomic e) => [Chrom] -> e -> Bool
chromFilter exclusionList = (`notElem` exclusionList) . genomicChrom

genomicFilterThroughBed :: (Monad m, Genomic e) => Producer BedEntry m () -> Producer e m () -> Producer e m ()
genomicFilterThroughBed bedProd = filterThroughBed bedProd genomicPosition
