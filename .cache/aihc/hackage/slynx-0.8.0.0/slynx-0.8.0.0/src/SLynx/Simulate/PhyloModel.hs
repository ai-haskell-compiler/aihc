{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  SLynx.Simulate.PhyloModel
-- Description :  Parse and interpret the model string
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Feb  1 13:32:16 2019.
module SLynx.Simulate.PhyloModel
  ( getPhyloModel,
  )
where

import Control.Applicative
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Either (rights)
import Data.Maybe
import qualified Data.Vector as V
import ELynx.Import.MarkovProcess.EDMModelPhylobayes
  ( EDMComponent,
  )
import ELynx.MarkovProcess.AminoAcid
import ELynx.MarkovProcess.CXXModels
import qualified ELynx.MarkovProcess.MixtureModel as M
import ELynx.MarkovProcess.Nucleotide
import qualified ELynx.MarkovProcess.PhyloModel as P
import ELynx.MarkovProcess.RateMatrix
import qualified ELynx.MarkovProcess.SubstitutionModel as S
import ELynx.Tools.Equality
import ELynx.Tools.InputOutput
import Numeric.LinearAlgebra (norm_1, size, vector)
import SLynx.Simulate.Options (MixtureModelGlobalNormalization (..))

nNuc :: Int
-- nNuc = length (alphabet :: [Nucleotide])
nNuc = 4

nAA :: Int
-- nAA = length (alphabet :: [AminoAcid])
nAA = 20

-- Model parameters between square brackets.
paramsStart :: Char
paramsStart = '['

paramsEnd :: Char
paramsEnd = ']'

-- Stationary distribution between curly brackets.
sdStart :: Char
sdStart = '{'

sdEnd :: Char
sdEnd = '}'

-- Mixture model components between round brackets.
mmStart :: Char
mmStart = '('

mmEnd :: Char
mmEnd = ')'

separator :: Char
separator = ','

name :: Parser String
name =
  BS.unpack
    <$> takeWhile1 (notInClass [paramsStart, paramsEnd, sdStart, sdEnd, mmStart, mmEnd, separator])

params :: Parser [Double]
params = char paramsStart *> double `sepBy1` char separator <* char paramsEnd

stationaryDistribution :: Parser StationaryDistribution
stationaryDistribution = do
  _ <- char sdStart
  f <- vector <$> double `sepBy1` char separator
  _ <- char sdEnd
  if nearlyEq (norm_1 f) 1.0
    then return f
    else
      error $
        "Sum of stationary distribution is "
          ++ show (norm_1 f)
          ++ " but should be 1.0."

assertLength :: StationaryDistribution -> Int -> a -> a
assertLength d n r =
  if size d /= n
    then
      error $
        "Length of stationary distribution is "
          ++ show (size d)
          ++ " but should be "
          ++ show n
          ++ "."
    else r

-- This is the main function that connects the model string, the parameters and
-- the stationary distribution. It should check that the model is valid.
assembleSubstitutionModel ::
  S.Normalize ->
  String ->
  Maybe S.Params ->
  Maybe StationaryDistribution ->
  Either String S.SubstitutionModel
-- DNA models.
assembleSubstitutionModel nz "JC" Nothing Nothing = Right $ jc nz
assembleSubstitutionModel nz "F81" Nothing (Just d) =
  Right $ assertLength d nNuc $ f81 nz d
assembleSubstitutionModel nz "HKY" (Just [k]) (Just d) =
  Right $ assertLength d nNuc $ hky nz k d
assembleSubstitutionModel nz "GTR4" (Just es) (Just d) =
  Right $ assertLength d nNuc $ gtr4 nz es d
-- Protein models.
assembleSubstitutionModel nz "Poisson" Nothing Nothing = Right $ poisson nz
assembleSubstitutionModel nz "Poisson-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ poissonCustom Nothing nz d
assembleSubstitutionModel nz "LG" Nothing Nothing = Right $ lg nz
assembleSubstitutionModel nz "LG-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ lgCustom Nothing nz d
assembleSubstitutionModel nz "WAG" Nothing Nothing = Right $ wag nz
assembleSubstitutionModel nz "WAG-Custom" Nothing (Just d) =
  Right $ assertLength d nAA $ wagCustom Nothing nz d
assembleSubstitutionModel nz "GTR20" (Just es) (Just d) =
  Right $ assertLength d nAA $ gtr20 nz es d
-- Ohterwisse, we cannot assemble the model.
assembleSubstitutionModel nz n mps mf =
  Left $
    unlines
      [ "Cannot assemble substitution model.",
        "Normalize: " ++ show nz,
        "Name: " ++ show n,
        "Parameters: " ++ show mps,
        "Stationary distribution: " ++ show mf
      ]

parseSubstitutionModel :: S.Normalize -> Parser S.SubstitutionModel
parseSubstitutionModel nz = do
  n <- name
  mps <- optional params
  mf <- optional stationaryDistribution
  let esm = assembleSubstitutionModel nz n mps mf
  case esm of
    Left err -> fail err
    Right sm -> return sm

edmModel :: MixtureModelGlobalNormalization -> [EDMComponent] -> Maybe [M.Weight] -> Parser M.MixtureModel
edmModel nz cs mws = do
  _ <- string "EDM"
  _ <- char mmStart
  n <- name
  mps <- optional params
  _ <- char mmEnd
  when (null cs) $ error "edmModel: no EDM components given."
  let sms = map (\c -> assembleSubstitutionModel subNz n mps (Just $ snd c)) cs
      edmName = "EDM" ++ show (length cs)
      ws = fromMaybe (map fst cs) mws
      errs = [e | (Left e) <- sms]
  when (length sms /= length ws) $
    error "edmModel: number of substitution models and weights differs."
  if not $ null errs
    then fail $ head errs
    else
      return $
        M.fromSubstitutionModels edmName mmNz (V.fromList ws) (V.fromList $ rights sms)
  where
    (subNz, mmNz) = case nz of
      GlobalNormalization -> (S.DoNotNormalize, S.DoNormalize)
      LocalNormalization -> (S.DoNormalize, S.DoNotNormalize)

cxxModel :: MixtureModelGlobalNormalization -> Maybe [M.Weight] -> Parser M.MixtureModel
cxxModel LocalNormalization _ = fail "Local normalization impossible with CXX models."
cxxModel _ mws = do
  _ <- char 'C'
  n <- decimal :: Parser Int
  return $ cxx n mws

standardMixtureModel :: MixtureModelGlobalNormalization -> [M.Weight] -> Parser M.MixtureModel
standardMixtureModel nz ws = do
  _ <- string "MIXTURE"
  _ <- char mmStart
  sms <- parseSubstitutionModel subNz `sepBy1` char separator
  _ <- char mmEnd
  -- XXX: The use of `Data.List.NonEmpty.fromList` leads to uninformative error messages.
  return $ M.fromSubstitutionModels "MIXTURE" mmNz (V.fromList ws) (V.fromList sms)
  where
    (subNz, mmNz) = case nz of
      GlobalNormalization -> (S.DoNotNormalize, S.DoNormalize)
      LocalNormalization -> (S.DoNormalize, S.DoNotNormalize)

mixtureModel :: MixtureModelGlobalNormalization -> Maybe [EDMComponent] -> Maybe [M.Weight] -> Parser M.MixtureModel
mixtureModel nz Nothing Nothing = try (cxxModel nz Nothing) <|> fail "No weights provided."
mixtureModel nz Nothing mws@(Just ws) = try (cxxModel nz mws) <|> standardMixtureModel nz ws
mixtureModel nz (Just cs) mws = edmModel nz cs mws

-- | Parse the phylogenetic model string. The argument list is somewhat long,
-- but models can have many parameters and we have to check for redundant
-- parameters.
--
-- @
-- getPhyloModel maybeSubstitutionModelString maybeMixtureModelString maybeEDMComponents
-- @
getPhyloModel ::
  Maybe String ->
  Maybe String ->
  MixtureModelGlobalNormalization ->
  Maybe [M.Weight] ->
  Maybe [EDMComponent] ->
  Either String P.PhyloModel
getPhyloModel Nothing Nothing _ _ _ = Left "No model was given. See help."
getPhyloModel (Just _) (Just _) _ _ _ =
  Left "Both, substitution and mixture model string given; use only one."
getPhyloModel (Just s) Nothing nz Nothing Nothing
  | nz == GlobalNormalization = Left "Global normalization not possible for substitution models."
  | otherwise = Right $ P.SubstitutionModel $ parseStringWith (parseSubstitutionModel S.DoNormalize) s
getPhyloModel (Just _) Nothing _ (Just _) _ =
  Left "Weights given; but cannot be used with substitution model."
getPhyloModel (Just _) Nothing _ _ (Just _) =
  let msg1 = "Empirical distribution mixture model components given;"
      msg2 = " but cannot be used with substitution model."
   in Left $ msg1 <> msg2
getPhyloModel Nothing (Just m) nz mws mcs =
  Right $ P.MixtureModel $ parseStringWith (mixtureModel nz mcs mws) m
