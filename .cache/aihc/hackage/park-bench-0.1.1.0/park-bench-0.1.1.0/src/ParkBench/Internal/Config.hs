module ParkBench.Internal.Config
  ( Config (..),
    getFromEnv,
  )
where

import ParkBench.Internal.Prelude
import System.Environment
import Text.Read (readMaybe)

data Config = Config
  { runlen :: Rational
  }

getFromEnv :: IO Config
getFromEnv = do
  maybeRunlen <- lookupDoubleEnv "PARK_BENCH_RUNLEN"
  pure
    Config
      { runlen =
          case maybeRunlen of
            Nothing -> 100_000_000
            Just runlen -> doubleToRational runlen * 1_000_000_000
      }

lookupDoubleEnv :: String -> IO (Maybe Double)
lookupDoubleEnv name = do
  val <- lookupEnv name
  pure (val >>= readMaybe)
