{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  SLynx.Simulate.Simulate
-- Description :  Simulate multiple sequence alignments
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Jan 28 14:12:52 2019.
module SLynx.Simulate.Simulate
  ( simulateCmd,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import ELynx.Alphabet.Alphabet as A
import ELynx.Import.MarkovProcess.EDMModelPhylobayes
import ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
import qualified ELynx.MarkovProcess.AminoAcid as MA
import ELynx.MarkovProcess.GammaRateHeterogeneity
import qualified ELynx.MarkovProcess.MixtureModel as MM
import qualified ELynx.MarkovProcess.PhyloModel as MP
import qualified ELynx.MarkovProcess.RateMatrix as MR
import qualified ELynx.MarkovProcess.SubstitutionModel as MS
import ELynx.Sequence.Export.Fasta
import qualified ELynx.Sequence.Sequence as Seq hiding
  ( name,
  )
import ELynx.Simulate.MarkovProcessAlongTree
import ELynx.Tools.ByteString
import ELynx.Tools.Definitions
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.InputOutput
import ELynx.Tools.Logger
import ELynx.Tools.Options
import ELynx.Tools.Reproduction
import ELynx.Tree
import qualified Numeric.LinearAlgebra as L
import SLynx.Simulate.Options
import SLynx.Simulate.PhyloModel
import System.Random.Stateful
import Text.Printf

-- Display a vector with given precision.
dispv :: Int -> VS.Vector L.R -> String
dispv p v = head $ tail $ lines $ L.dispf p (L.asRow v)

-- Display a matrix with given precision and indent.
dispmi :: Int -> Int -> L.Matrix L.R -> String
dispmi p i m =
  intercalate "\n" $ map (replicate i ' ' ++) $ tail $ lines $ L.dispf p m

getDistLine :: Int -> MR.StationaryDistribution -> BB.Builder
getDistLine i d =
  BB.intDec i
    <> BB.char8 ' '
    <> s
  where
    s = mconcat $ intersperse (BB.char8 ' ') $ map BB.doubleDec $ VS.toList d

writeSiteDists :: [Int] -> V.Vector MR.StationaryDistribution -> ELynx SimulateArguments ()
-- writeSiteDists is ds = out "site distributions of distribution mixture model" output ".sitedists"
writeSiteDists componentIs ds = do
  mbn <- outFileBaseName . globalArguments <$> ask
  case mbn of
    Nothing -> return ()
    Just bn -> liftIO $ BL.writeFile (bn <> ".sitedists") output
  where
    dsPaml = V.map MA.alphaToPamlVec ds
    lns = [getDistLine i d | (i, c) <- zip [1 ..] componentIs, let d = dsPaml V.! c]
    output = BB.toLazyByteString $ mconcat $ intersperse (BB.char8 '\n') lns

-- Simulate a 'Alignment' for a given phylogenetic model,
-- phylogenetic tree, and alignment length.
simulateAlignment ::
  (RandomGen g, HasLength e, HasName a) =>
  MP.PhyloModel ->
  Tree e a ->
  Int ->
  IOGenM g ->
  ELynx SimulateArguments ()
simulateAlignment pm t' n g = do
  let t = fromLength . getLength <$> toTreeBranchLabels t'
  leafStates <- case pm of
    MP.SubstitutionModel sm -> liftIO $ simulateAndFlattenPar n d e t g
      where
        d = MS.stationaryDistribution sm
        e = MS.exchangeabilityMatrix sm
    MP.MixtureModel mm -> do
      (cs, ss) <- liftIO $ simulateAndFlattenMixtureModelPar n ws ds es t g
      -- TODO: Writing site distributions only makes sense for EDM models.
      -- Remove this if not needed or improve to be helpful in general.
      writeSiteDists cs ds
      return ss
      where
        ws = MM.getWeights mm
        ds = V.map MS.stationaryDistribution $ MM.getSubstitutionModels mm
        es = V.map MS.exchangeabilityMatrix $ MM.getSubstitutionModels mm
  let leafNames = map getName $ leaves t'
      code = MP.getAlphabet pm
      -- XXX: Probably use type safe stuff here?
      alph = A.all $ alphabetSpec code
      sequences =
        [ Seq.Sequence (fromName sName) "" code (U.fromList $ map (`Set.elemAt` alph) ss)
          | (sName, ss) <- zip leafNames leafStates
        ]
      output = sequencesToFasta sequences
  logInfoS ""
  out "simulated multi sequence alignment" output ".fasta"

-- Summarize EDM components; line to be printed to screen or log.
summarizeEDMComponents :: [EDMComponent] -> BL.ByteString
summarizeEDMComponents cs =
  BL.pack $
    "Empiricial distribution mixture model with "
      ++ show (length cs)
      ++ " components."

reportModel :: MP.PhyloModel -> ELynx SimulateArguments ()
reportModel m = do
  as <- globalArguments <$> ask
  if writeElynxFile as
    then
      ( do
          let bn = outFileBaseName as
          case bn of
            Nothing ->
              logInfoS
                "No output file provided; omit writing machine-readable phylogenetic model."
            Just _ ->
              out "model definition (machine readable)" (BL.pack (show m) <> "\n") ".model.gz"
      )
    else logInfoS "No elynx file required; omit writing machine-readable phylogenetic model."

pretty :: Length -> String
pretty = printf "%.5f" . fromLength

prettyRow :: String -> String -> BL.ByteString
prettyRow name val = alignLeft 33 n <> alignRight 8 v
  where
    n = BL.pack name
    v = BL.pack val

-- | Examine branches of a tree.
summarizeLengths :: (HasLength e) => Tree e a -> BL.ByteString
summarizeLengths t =
  BL.intercalate
    "\n"
    [ prettyRow "Origin height: " $ pretty h,
      prettyRow "Average distance origin to leaves: " $ pretty h',
      prettyRow "Total branch length: " $ pretty b
    ]
  where
    n = length $ leaves t
    h = height t
    h' = sum (distancesOriginLeaves t) / fromIntegral n
    b = totalBranchLength t

-- Round double to a given precision.
roundN :: Int -> Double -> Double
roundN n v = fromInteger (round $ v * (10 ^ n)) / (10.0 ^^ n)

-- Summarize a substitution model; lines to be printed to screen or log.
summarizeSM :: MS.SubstitutionModel -> [BL.ByteString]
summarizeSM sm =
  map BL.pack $
    (show (MS.alphabet sm) ++ " substitution model: " ++ MS.name sm ++ ".")
      : ["Parameters: " ++ show (MS.params sm) ++ "." | not (null (MS.params sm))]
      ++ case MS.alphabet sm of
        DNA ->
          [ "Stationary distribution: "
              ++ dispv precision (MS.stationaryDistribution sm)
              ++ ".",
            "Exchangeability matrix:\n"
              ++ dispmi 2 precision (MS.exchangeabilityMatrix sm)
              ++ ".",
            "Scale: " ++ show (roundN precision $ MS.totalRate sm) ++ "."
          ]
        Protein ->
          [ "Stationary distribution: "
              ++ dispv precision (MS.stationaryDistribution sm)
              ++ ".",
            "Scale: " ++ show (roundN precision $ MS.totalRate sm) ++ "."
          ]
        _ ->
          error
            "Extended character sets are not supported with substitution models."

-- Summarize a mixture model component; lines to be printed to screen or log.
summarizeMMComponent :: MM.Component -> [BL.ByteString]
summarizeMMComponent c =
  BL.pack "Weight: "
    <> (BB.toLazyByteString . BB.doubleDec $ MM.weight c)
    : summarizeSM (MM.substModel c)

-- Summarize a mixture model; lines to be printed to screen or log.
summarizeMM :: MixtureModelGlobalNormalization -> MM.MixtureModel -> [BL.ByteString]
summarizeMM nz m =
  [ BL.pack $ "Mixture model: " ++ MM.name m ++ ".",
    BL.pack $ "Mixture model normalization: " ++ show nz ++ ".",
    BL.pack $ "Number of components: " ++ show n ++ "."
  ]
    ++ detail
  where
    n = length $ MM.components m
    detail =
      if n <= 100
        then
          concat
            [ BL.pack ("Component " ++ show i ++ ":") : summarizeMMComponent c
              | (i, c) <- zip [1 :: Int ..] (V.toList $ MM.components m)
            ]
        else []

-- Summarize a phylogenetic model; lines to be printed to screen or log.
summarizePM :: MixtureModelGlobalNormalization -> MP.PhyloModel -> [BL.ByteString]
summarizePM nz (MP.MixtureModel mm) = summarizeMM nz mm
summarizePM _ (MP.SubstitutionModel sm) = summarizeSM sm

-- | Simulate sequences.
simulateCmd :: ELynx SimulateArguments ()
simulateCmd = do
  l <- localArguments <$> ask
  let treeFile = argsTreeFile l
  logInfoS ""
  logInfoS $ "Read tree from file '" ++ treeFile ++ "'."
  tree <- liftIO $ parseFileWith (newick Standard) treeFile
  let t' = either error id $ toLengthTree tree
  logInfoS $ "Number of leaves: " ++ show (length $ leaves t')
  logInfoB $ summarizeLengths t'
  let edmFile = argsEDMFile l
  let sProfileFiles = argsSiteprofilesFiles l
  logInfoS ""
  logDebugS "Read EDM file or siteprofile files."
  when (isJust edmFile && isJust sProfileFiles) $
    error "Got both: --edm-file and --siteprofile-files."
  edmCs <- case edmFile of
    Nothing -> return Nothing
    Just edmF -> do
      logInfoS "Read EDM file."
      liftIO $ Just <$> parseFileWith phylobayes edmF
  maybe
    (return ())
    (logInfoB . summarizeEDMComponents)
    edmCs
  sProfiles <- case sProfileFiles of
    Nothing -> return Nothing
    Just fns -> do
      logInfoS $
        "Read siteprofiles from "
          ++ show (length fns)
          ++ " file(s)."
      logDebugS $ "The file names are:" ++ show fns
      xs <- liftIO $ mapM (parseFileWith siteprofiles) fns
      return $ Just $ concat xs
  maybe
    (return ())
    (logInfoB . summarizeEDMComponents)
    sProfiles
  let edmCsOrSiteprofiles = edmCs <|> sProfiles
  logInfoS "Read model string."
  let ms = argsSubstitutionModelString l
      mm = argsMixtureModelString l
      mws = argsMixtureWeights l
      nz = argsMixtureModelGlobalNormalization l
      eitherPhyloModel' = getPhyloModel ms mm nz mws edmCsOrSiteprofiles
  phyloModel' <- case eitherPhyloModel' of
    Left err -> lift $ error err
    Right pm -> return pm
  let maybeGammaParams = argsGammaParams l
  phyloModel <- case maybeGammaParams of
    Nothing -> do
      logInfoB $ BL.unlines $ summarizePM nz phyloModel'
      return phyloModel'
    Just (n, alpha) -> do
      logInfoB $ BL.intercalate "\n" $ summarizePM nz phyloModel'
      logInfoS ""
      logInfoB $ BL.intercalate "\n" $ summarizeGammaRateHeterogeneity n alpha
      return $ expand n alpha phyloModel'
  reportModel phyloModel
  logInfoS "Simulate alignment."
  let alignmentLength = argsLength l
  logInfoS $ "Length: " <> show alignmentLength <> "."
  gen <- newIOGenM $ mkStdGen $ case argsSeed l of
    RandomUnset -> error "simulateCmd: seed not available; please contact maintainer."
    RandomSet s -> s
    Fixed s -> s
  simulateAlignment phyloModel t' alignmentLength gen
