{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
--
-- Module      :  Analyze.Analyze
-- Description :  Parse sequence file formats and analyze them
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 08:41:05 2018.
module SLynx.Examine.Examine
  ( examineCmd,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified ELynx.Alphabet.Alphabet as A
import qualified ELynx.Alphabet.Character as C
import qualified ELynx.Sequence.Alignment as M
import qualified ELynx.Sequence.Distance as D
import ELynx.Sequence.Divergence
import qualified ELynx.Sequence.Sequence as Seq
import ELynx.Tools.ByteString
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import qualified Numeric.LinearAlgebra as L
import SLynx.Examine.Options
import SLynx.Tools
import qualified Statistics.Sample as Sm
import System.IO
import Text.Printf

pRow :: String -> String -> BL.ByteString
pRow name val = alignLeft 50 n <> alignRight 10 v
  where
    n = BL.pack name
    v = BL.pack val

examineAlignment :: Bool -> M.Alignment -> BL.ByteString
examineAlignment perSiteFlag a =
  BL.unlines
    [ "Sequences have equal length (multi sequence alignment, or single sequence).",
      "Number of columns in alignment:",
      pRow "  Total:" $ show aL,
      pRow "  Constant:" $ show nConstant,
      pRow "  Constant (including gaps or unknowns):" $ show nConstantSoft,
      pRow "  Without gaps:" $ show (M.length aNoGaps),
      pRow "  With standard characters only:" $
        show (M.length aOnlyStd),
      "",
      pRow "Total number of characters:" $ show nTot,
      pRow "Standard (i.e., not extended IUPAC) characters:" $
        show (nTot - nIUPAC - nGaps - nUnknowns),
      pRow "Extended IUPAC characters:" $ show nIUPAC,
      pRow "Gaps:" $ show nGaps,
      pRow "Unknowns:" $ show nUnknowns,
      "",
      pRow "Percentage of standard characters:" $
        printf "%2.2f" (100.0 - percentIUPAC - percentGaps - percentUnknowns),
      pRow "Percentage of extended IUPAC characters:" $
        printf "%2.2f" percentIUPAC,
      pRow "Percentage of gaps:" $ printf "%2.2f" percentGaps,
      pRow "Percentage of unknowns:" $ printf "%2.2f" percentUnknowns,
      "",
      BL.pack "Distribution of characters:",
      -- Holy crap.
      BL.pack $
        concatMap ((: "     ") . C.toChar) $
          S.toList $
            A.std $
              A.alphabetSpec $
                M.alphabet a,
      BL.pack $ unwords $ map (printf "%.3f") charFreqs,
      "",
      BL.pack "Pairwise hamming distances (per site):",
      pRow "  Mean:" $ printf "%.3f" hMean,
      pRow "  Standard deviation:" $ printf "%.3f" $ sqrt hVar,
      pRow "  Minimum:" $ printf "%.3f" hMin,
      pRow "  Maximum:" $ printf "%.3f" hMax,
      "",
      "Mean effective number of states (measured using entropy):",
      pRow "Across whole alignment:" $ printf "%.3f" kEffMean,
      pRow "Across columns without gaps:" $ printf "%.3f" kEffMeanNoGaps,
      pRow "Across columns without extended IUPAC characters:" $
        printf "%.3f" kEffMeanOnlyStd,
      "",
      BL.pack "Mean effective number of states (measured using homoplasy):",
      pRow "Across whole alignment:" $ printf "%.3f" kEffMeanHomo,
      pRow "Across columns without gaps:" $ printf "%.3f" kEffMeanNoGapsHomo,
      pRow "Across columns without extended IUPAC characters:" $
        printf "%.3f" kEffMeanOnlyStdHomo,
      "",
      "Divergence matrix:"
    ]
    <> perSiteBS
  where
    aL = M.length a
    nConstant = M.length $ M.filterColsConstant a
    nConstantSoft = M.length $ M.filterColsConstantSoft a
    nTot = M.length a * M.nSequences a
    nIUPAC = M.countIUPACChars a
    nGaps = M.countGaps a
    nUnknowns = M.countUnknowns a
    percentIUPAC = 100 * fromIntegral nIUPAC / fromIntegral nTot :: Double
    percentGaps = 100 * fromIntegral nGaps / fromIntegral nTot :: Double
    percentUnknowns = 100 * fromIntegral nUnknowns / fromIntegral nTot :: Double
    aNoGaps = M.filterColsNoGaps a
    aNoGapsFreq = M.toFrequencyData aNoGaps
    aOnlyStd = M.filterColsOnlyStd aNoGaps
    aOnlyStdFreq = M.toFrequencyData aOnlyStd
    charFreqsPerSite = M.toFrequencyData a
    charFreqs = M.distribution charFreqsPerSite
    seqs = M.toSequences a
    normlz x = fromIntegral x / fromIntegral aL
    pairwiseHamming =
      V.fromList
        [ either error normlz $ D.hamming x y
          | x <- seqs,
            y <- seqs,
            x /= y
        ]
    (hMean, hVar) = Sm.meanVariance pairwiseHamming
    hMin = V.minimum pairwiseHamming
    hMax = V.maximum pairwiseHamming
    kEffs = M.kEffEntropy charFreqsPerSite
    kEffsNoGaps = M.kEffEntropy aNoGapsFreq
    kEffsOnlyStd = M.kEffEntropy aOnlyStdFreq
    kEffMean = sum kEffs / fromIntegral (length kEffs)
    kEffMeanNoGaps = sum kEffsNoGaps / fromIntegral (length kEffsNoGaps)
    kEffMeanOnlyStd = sum kEffsOnlyStd / fromIntegral (length kEffsOnlyStd)
    kEffsHomo = M.kEffHomoplasy charFreqsPerSite
    kEffsNoGapsHomo = M.kEffHomoplasy aNoGapsFreq
    kEffsOnlyStdHomo = M.kEffHomoplasy aOnlyStdFreq
    kEffMeanHomo = sum kEffsHomo / fromIntegral (length kEffsHomo)
    kEffMeanNoGapsHomo =
      sum kEffsNoGapsHomo / fromIntegral (length kEffsNoGapsHomo)
    kEffMeanOnlyStdHomo =
      sum kEffsOnlyStdHomo / fromIntegral (length kEffsOnlyStdHomo)
    perSiteBS =
      if perSiteFlag
        then
          BL.unlines
            [ BL.pack "Effective number of used states per site (measured using entropy):",
              BL.pack . show $ kEffs
            ]
        else BL.empty

examine :: Bool -> [Seq.Sequence] -> BL.ByteString
examine perSiteFlag ss =
  Seq.summarizeSequences ss <> case M.fromSequences ss of
    Left _ -> BL.empty
    Right a -> BL.pack "\n" <> examineAlignment perSiteFlag a

-- From https://stackoverflow.com/a/52602906/3536806.
tuples :: [a] -> [(a, a)]
tuples [] = []
tuples (x : xs) = map (x,) xs ++ tuples xs

-- This is all ugly, but who cares.
writeDivergenceMatrix :: Handle -> (Seq.Sequence, Seq.Sequence, MU.Matrix Int) -> IO ()
writeDivergenceMatrix h (x, y, xs) = do
  BL.hPutStrLn h $ "> " <> Seq.name x <> ", " <> Seq.name y
  hPutStr h $ L.dispf 0 m
  where
    n = MU.rows xs
    m = L.matrix n $ map fromIntegral $ MU.toList xs

computeDivergenceMatrices :: [Seq.Sequence] -> ELynx ExamineArguments ()
computeDivergenceMatrices ss = do
  logInfoS "Compute divergence matrices."
  let xys = tuples ss
      ds = map (\(x, y) -> (x, y, either error id $ divergence x y)) xys
  h <- outHandle "divergence matrices" ".div"
  mapM_ (liftIO . writeDivergenceMatrix h) ds
  liftIO $ hClose h

-- | Examine sequences.
examineCmd :: ELynx ExamineArguments ()
examineCmd = do
  (ExamineArguments al inFile perSiteFlag divergenceFlag) <- localArguments <$> ask
  ss <- readSeqs al inFile
  let result = examine perSiteFlag ss
  out "result of examination" result ".out"
  when divergenceFlag (computeDivergenceMatrices ss)
