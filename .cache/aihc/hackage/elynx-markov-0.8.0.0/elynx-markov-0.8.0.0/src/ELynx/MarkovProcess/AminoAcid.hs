-- |
-- Module      :  ELynx.MarkovProcess.AminoAcid
-- Description :  Amino acid rate matrices such as LG
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 09:29:19 2019.
--
-- The order of amino acids is alphabetic.
module ELynx.MarkovProcess.AminoAcid
  ( -- * Amino acid substitution models
    lg,
    lgCustom,
    wag,
    wagCustom,
    poisson,
    poissonCustom,
    gtr20,

    -- * Convenience functions
    alphaToPamlVec,
    pamlToAlphaVec,
  )
where

import Data.ByteString.Internal (c2w)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import ELynx.Alphabet.Alphabet
import ELynx.MarkovProcess.RateMatrix
import ELynx.MarkovProcess.SubstitutionModel
import Numeric.LinearAlgebra

n :: Int
n = 20

-- Some matrices have to be converted from PAML order to alphabetical order. See
-- 'pamlToAlphaVec' and 'pamlToAlphaMat'.

-- Amno acids in alphabetical order.
aaAlphaOrder :: V.Vector Word8
aaAlphaOrder =
  V.map
    c2w
    $ V.fromList
      [ 'A',
        'C',
        'D',
        'E',
        'F',
        'G',
        'H',
        'I',
        'K',
        'L',
        'M',
        'N',
        'P',
        'Q',
        'R',
        'S',
        'T',
        'V',
        'W',
        'Y'
      ]

-- Amino acids in PAML oder.
aaPamlOrder :: V.Vector Word8
aaPamlOrder =
  V.map
    c2w
    $ V.fromList
      [ 'A',
        'R',
        'N',
        'D',
        'C',
        'Q',
        'E',
        'G',
        'H',
        'I',
        'L',
        'K',
        'M',
        'F',
        'P',
        'S',
        'T',
        'W',
        'Y',
        'V'
      ]

alphaIndexToPamlIndex :: Int -> Int
alphaIndexToPamlIndex i =
  fromMaybe
    (error $ "Could not convert index " ++ show i ++ ".")
    (V.elemIndex aa aaPamlOrder)
  where
    aa = aaAlphaOrder V.! i

pamlIndexToAlphaIndex :: Int -> Int
pamlIndexToAlphaIndex i =
  fromMaybe
    (error $ "Could not convert index " ++ show i ++ ".")
    (V.elemIndex aa aaAlphaOrder)
  where
    aa = aaPamlOrder V.! i

-- | Convert an amino acid vector in PAML order to a vector in alphabetical order.
pamlToAlphaVec :: Vector R -> Vector R
pamlToAlphaVec v = build n (\i -> v ! alphaIndexToPamlIndex (round i))

-- | Convert an amino acid vector in alphabetical order to a vector in PAML order.
alphaToPamlVec :: Vector R -> Vector R
alphaToPamlVec v = build n (\i -> v ! pamlIndexToAlphaIndex (round i))

-- Convert an amino acid matrix in PAML order to a matrix in alphabetical order.
pamlToAlphaMat :: Matrix R -> Matrix R
pamlToAlphaMat m =
  build
    (n, n)
    ( \i j -> m ! alphaIndexToPamlIndex (round i) ! alphaIndexToPamlIndex (round j)
    )

-- Lower triangular matrix of LG exchangeabilities in PAML order and in form of
-- a list.
lgExchRawPaml :: [Double]
lgExchRawPaml =
  [ 0.425093,
    0.276818,
    0.751878,
    0.395144,
    0.123954,
    5.076149,
    2.489084,
    0.534551,
    0.528768,
    0.062556,
    0.969894,
    2.807908,
    1.695752,
    0.523386,
    0.084808,
    1.038545,
    0.363970,
    0.541712,
    5.243870,
    0.003499,
    4.128591,
    2.066040,
    0.390192,
    1.437645,
    0.844926,
    0.569265,
    0.267959,
    0.348847,
    0.358858,
    2.426601,
    4.509238,
    0.927114,
    0.640543,
    4.813505,
    0.423881,
    0.311484,
    0.149830,
    0.126991,
    0.191503,
    0.010690,
    0.320627,
    0.072854,
    0.044265,
    0.008705,
    0.108882,
    0.395337,
    0.301848,
    0.068427,
    0.015076,
    0.594007,
    0.582457,
    0.069673,
    0.044261,
    0.366317,
    4.145067,
    0.536518,
    6.326067,
    2.145078,
    0.282959,
    0.013266,
    3.234294,
    1.807177,
    0.296636,
    0.697264,
    0.159069,
    0.137500,
    1.124035,
    0.484133,
    0.371004,
    0.025548,
    0.893680,
    1.672569,
    0.173735,
    0.139538,
    0.442472,
    4.273607,
    6.312358,
    0.656604,
    0.253701,
    0.052722,
    0.089525,
    0.017416,
    1.105251,
    0.035855,
    0.018811,
    0.089586,
    0.682139,
    1.112727,
    2.592692,
    0.023918,
    1.798853,
    1.177651,
    0.332533,
    0.161787,
    0.394456,
    0.075382,
    0.624294,
    0.419409,
    0.196961,
    0.508851,
    0.078281,
    0.249060,
    0.390322,
    0.099849,
    0.094464,
    4.727182,
    0.858151,
    4.008358,
    1.240275,
    2.784478,
    1.223828,
    0.611973,
    1.739990,
    0.990012,
    0.064105,
    0.182287,
    0.748683,
    0.346960,
    0.361819,
    1.338132,
    2.139501,
    0.578987,
    2.000679,
    0.425860,
    1.143480,
    1.080136,
    0.604545,
    0.129836,
    0.584262,
    1.033739,
    0.302936,
    1.136863,
    2.020366,
    0.165001,
    0.571468,
    6.472279,
    0.180717,
    0.593607,
    0.045376,
    0.029890,
    0.670128,
    0.236199,
    0.077852,
    0.268491,
    0.597054,
    0.111660,
    0.619632,
    0.049906,
    0.696175,
    2.457121,
    0.095131,
    0.248862,
    0.140825,
    0.218959,
    0.314440,
    0.612025,
    0.135107,
    1.165532,
    0.257336,
    0.120037,
    0.054679,
    5.306834,
    0.232523,
    0.299648,
    0.131932,
    0.481306,
    7.803902,
    0.089613,
    0.400547,
    0.245841,
    3.151815,
    2.547870,
    0.170887,
    0.083688,
    0.037967,
    1.959291,
    0.210332,
    0.245034,
    0.076701,
    0.119013,
    10.649107,
    1.702745,
    0.185202,
    1.898718,
    0.654683,
    0.296501,
    0.098369,
    2.188158,
    0.189510,
    0.249313
  ]

-- Exchangeabilities of LG model in alphabetical order.
lgExch :: ExchangeabilityMatrix
lgExch = pamlToAlphaMat $ exchFromListLower n lgExchRawPaml

normalizeSumVec :: Vector Double -> Vector Double
normalizeSumVec v = V.map (/ s) v
  where
    s = V.sum v
{-# INLINE normalizeSumVec #-}

-- Stationary distribution in PAML order.
lgStatDistPaml :: StationaryDistribution
lgStatDistPaml =
  normalizeSumVec $
    fromList
      [ 0.079066,
        0.055941,
        0.041977,
        0.053052,
        0.012937,
        0.040767,
        0.071586,
        0.057337,
        0.022355,
        0.062157,
        0.099081,
        0.064600,
        0.022951,
        0.042302,
        0.044040,
        0.061197,
        0.053287,
        0.012066,
        0.034155,
        0.069147
      ]

-- Stationary distribution of LG model in alphabetical order.
lgStatDist :: StationaryDistribution
lgStatDist = pamlToAlphaVec lgStatDistPaml

-- | LG substitution model.
lg :: Normalize -> SubstitutionModel
lg nz = substitutionModel Protein "LG" [] nz lgStatDist lgExch

-- | LG substitution model with maybe a name and a custom stationary distribution.
lgCustom :: Maybe String -> Normalize -> StationaryDistribution -> SubstitutionModel
lgCustom mnm nz d = substitutionModel Protein nm [] nz d lgExch
  where
    nm = fromMaybe "LG-Custom" mnm

-- WAG exchangeability list in PAML order.
wagExchRawPaml :: [Double]
wagExchRawPaml =
  [ 55.15710,
    50.98480,
    63.53460,
    73.89980,
    14.73040,
    542.94200,
    102.70400,
    52.81910,
    26.52560,
    3.02949,
    90.85980,
    303.55000,
    154.36400,
    61.67830,
    9.88179,
    158.28500,
    43.91570,
    94.71980,
    617.41600,
    2.13520,
    546.94700,
    141.67200,
    58.46650,
    112.55600,
    86.55840,
    30.66740,
    33.00520,
    56.77170,
    31.69540,
    213.71500,
    395.62900,
    93.06760,
    24.89720,
    429.41100,
    57.00250,
    24.94100,
    19.33350,
    18.69790,
    55.42360,
    3.94370,
    17.01350,
    11.39170,
    12.73950,
    3.04501,
    13.81900,
    39.79150,
    49.76710,
    13.15280,
    8.48047,
    38.42870,
    86.94890,
    15.42630,
    6.13037,
    49.94620,
    317.09700,
    90.62650,
    535.14200,
    301.20100,
    47.98550,
    7.40339,
    389.49000,
    258.44300,
    37.35580,
    89.04320,
    32.38320,
    25.75550,
    89.34960,
    68.31620,
    19.82210,
    10.37540,
    39.04820,
    154.52600,
    31.51240,
    17.41000,
    40.41410,
    425.74600,
    485.40200,
    93.42760,
    21.04940,
    10.27110,
    9.61621,
    4.67304,
    39.80200,
    9.99208,
    8.11339,
    4.99310,
    67.93710,
    105.94700,
    211.51700,
    8.88360,
    119.06300,
    143.85500,
    67.94890,
    19.50810,
    42.39840,
    10.94040,
    93.33720,
    68.23550,
    24.35700,
    69.61980,
    9.99288,
    41.58440,
    55.68960,
    17.13290,
    16.14440,
    337.07900,
    122.41900,
    397.42300,
    107.17600,
    140.76600,
    102.88700,
    70.49390,
    134.18200,
    74.01690,
    31.94400,
    34.47390,
    96.71300,
    49.39050,
    54.59310,
    161.32800,
    212.11100,
    55.44130,
    203.00600,
    37.48660,
    51.29840,
    85.79280,
    82.27650,
    22.58330,
    47.33070,
    145.81600,
    32.66220,
    138.69800,
    151.61200,
    17.19030,
    79.53840,
    437.80200,
    11.31330,
    116.39200,
    7.19167,
    12.97670,
    71.70700,
    21.57370,
    15.65570,
    33.69830,
    26.25690,
    21.24830,
    66.53090,
    13.75050,
    51.57060,
    152.96400,
    13.94050,
    52.37420,
    11.08640,
    24.07350,
    38.15330,
    108.60000,
    32.57110,
    54.38330,
    22.77100,
    19.63030,
    10.36040,
    387.34400,
    42.01700,
    39.86180,
    13.32640,
    42.84370,
    645.42800,
    21.60460,
    78.69930,
    29.11480,
    248.53900,
    200.60100,
    25.18490,
    19.62460,
    15.23350,
    100.21400,
    30.12810,
    58.87310,
    18.72470,
    11.83580,
    782.13000,
    180.03400,
    30.54340,
    205.84500,
    64.98920,
    31.48870,
    23.27390,
    138.82300,
    36.53690,
    31.47300
  ]

-- WAG exchangeability matrix n alphabetical order.
wagExch :: ExchangeabilityMatrix
wagExch = pamlToAlphaMat $ exchFromListLower n wagExchRawPaml

-- WAG stationary distribution in PAML order.
wagStatDistPaml :: StationaryDistribution
wagStatDistPaml =
  normalizeSumVec $
    fromList
      [ 0.0866279,
        0.043972,
        0.0390894,
        0.0570451,
        0.0193078,
        0.0367281,
        0.0580589,
        0.0832518,
        0.0244313,
        0.048466,
        0.086209,
        0.0620286,
        0.0195027,
        0.0384319,
        0.0457631,
        0.0695179,
        0.0610127,
        0.0143859,
        0.0352742,
        0.0708957
      ]

-- WAG stationary distribution in alphabetical order.
wagStatDist :: StationaryDistribution
wagStatDist = pamlToAlphaVec wagStatDistPaml

-- | LG substitution model.
wag :: Normalize -> SubstitutionModel
wag nz = substitutionModel Protein "WAG" [] nz wagStatDist wagExch

-- | LG substitution model with maybe a name and a custom stationary distribution.
wagCustom :: Maybe String -> Normalize -> StationaryDistribution -> SubstitutionModel
wagCustom mnm nz d = substitutionModel Protein nm [] nz d wagExch
  where
    nm = fromMaybe "WAG-Custom" mnm

matrixSetDiagToZero :: Matrix R -> Matrix R
matrixSetDiagToZero m = m - diag (takeDiag m)
{-# INLINE matrixSetDiagToZero #-}

uniformExch :: ExchangeabilityMatrix
uniformExch = matrixSetDiagToZero $ matrix n $ replicate (n * n) 1.0

poissonExch :: ExchangeabilityMatrix
poissonExch = uniformExch

uniformVec :: Vector Double
uniformVec = V.replicate n (1 / fromIntegral n)

-- | Poisson substitution model.
poisson :: Normalize -> SubstitutionModel
poisson nz = substitutionModel Protein "Poisson" [] nz uniformVec poissonExch

-- | Poisson substitution model with maybe a name and a custom stationary distribution.
poissonCustom :: Maybe String -> Normalize -> StationaryDistribution -> SubstitutionModel
poissonCustom mnm nz d = substitutionModel Protein nm [] nz d poissonExch
  where
    nm = fromMaybe "Poisson-Custom" mnm

-- | General time reversible (GTR) substitution model for amino acids.
gtr20 :: Normalize -> [Double] -> StationaryDistribution -> SubstitutionModel
gtr20 nz es d = substitutionModel Protein "GTR" es nz d e
  where
    e = exchFromListUpper n es
