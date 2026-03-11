{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  ELynx.Tools.Reproduction
-- Description :  Functions to ease reproduction of analyses
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Nov 19 15:07:09 2019.
--
-- Use of standard input is not supported.
module ELynx.Tools.Reproduction
  ( -- * Reproduction
    SeedOpt (..),
    fromSeedOpt,
    Reproducible (..),
    getReproductionHash,
    Reproduction (..),
    writeReproduction,
    hashFile,
  )
where

import Control.Monad
import Crypto.Hash.SHA256
import Data.Aeson hiding (encode)
import Data.ByteString.Base16
import qualified Data.ByteString.Char8 as BS
import Data.Version
import GHC.Generics
import Options.Applicative
import Paths_elynx_tools
import System.Environment

-- TODO: This should be an 'Int' (new interface).

-- | Random or fixed seed.
data SeedOpt = RandomUnset | RandomSet Int | Fixed Int
  deriving (Eq, Generic, Show)

instance FromJSON SeedOpt

instance ToJSON SeedOpt

-- | Get the seed, if set.
fromSeedOpt :: SeedOpt -> Maybe Int
fromSeedOpt RandomUnset = Nothing
fromSeedOpt (RandomSet v) = Just v
fromSeedOpt (Fixed v) = Just v

-- | Reproducible commands have
--   - a set of input files to be checked for consistency,
--   - a set of output suffixes which define output files to be checked for consistency,
--   - a function to get the seed, if available,
--   - a function to set the seed, if applicable,
--   - a parser to read the command line,
--   - a nice program name, description, and footer.
class Reproducible a where
  inFiles :: a -> [FilePath]
  outSuffixes :: a -> [String]
  getSeed :: a -> Maybe SeedOpt
  setSeed :: a -> SeedOpt -> a
  parser :: Parser a
  cmdName :: String
  cmdDsc :: [String]
  cmdFtr :: [String]
  cmdFtr = []

-- | A unique hash of the reproduction data type.
getReproductionHash :: forall a. (Reproducible a) => Reproduction a -> String
getReproductionHash r =
  BS.unpack $
    encode $
      hash $
        BS.pack $
          unlines $
            -- Reproduction.
            progName r
              : argsStr r
                <> [showVersion (rVersion r)]
                <> files r
                <> checkSums r
                -- Reproducible.
                <> inFiles ri
                <> outSuffixes ri
                <> [cmdName @a]
                <> cmdDsc @a
                <> cmdFtr @a
  where
    ri = reproducible r

setHash :: (Reproducible a) => Reproduction a -> Reproduction a
setHash r = r {rHash = Just h} where h = getReproductionHash r

-- | Necessary information for a reproducible run. Notably, the input files are
-- checked for consistency!
data Reproduction a = Reproduction
  { -- | Program name.
    progName :: String,
    -- | Command line arguments without program name.
    argsStr :: [String],
    rVersion :: Version,
    -- | Unique hash; see 'getReproductionHash'.
    rHash :: Maybe String,
    -- | File paths of used files.
    files :: [FilePath],
    -- | SHA256 sums of used files.
    checkSums :: [String],
    -- | Command argument.
    reproducible :: a
  }
  deriving (Generic)

instance (FromJSON a) => FromJSON (Reproduction a)

instance (ToJSON a) => ToJSON (Reproduction a)

-- | Helper function.
hashFile :: FilePath -> IO BS.ByteString
hashFile f = encode . hash <$> BS.readFile f

-- | Write an ELynx reproduction file.
writeReproduction ::
  forall a.
  (Eq a, Show a, Reproducible a, ToJSON a) =>
  String ->
  a ->
  IO ()
writeReproduction bn r = do
  pn <- getProgName
  as <- getArgs
  let outFs = map (bn ++) (outSuffixes r)
  let fs = inFiles r ++ outFs
  cs <- mapM hashFile fs
  let cs' = map BS.unpack cs
      s = Reproduction pn as version Nothing fs cs' r
  void $ encodeFile (bn <> ".elynx") (setHash s)
