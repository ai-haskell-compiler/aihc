{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Import.MarkovProcess.EDMModelPhylobayes
-- Description :  Import stationary distributions from Phylobayes format
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 12:12:55 2019.
module ELynx.Import.MarkovProcess.EDMModelPhylobayes
  ( EDMComponent,
    phylobayes,
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector.Storable as V
import ELynx.MarkovProcess.MixtureModel

-- | An empirical mixture model component has a weight and a stationary
-- distribution.
type EDMComponent = (Weight, V.Vector Double)

-- | Parse stationary distributions from Phylobayes format.
phylobayes :: AS.Parser [EDMComponent]
phylobayes = (AS.<?> "phylobayes") $ do
  n <- headerLine
  k <- kComponentsLine
  cs <- AS.count k $ dataLine n
  _ <- AC.skipWhile AC.isSpace
  _ <- AS.endOfInput
  return cs

headerLine :: AS.Parser Int
headerLine = do
  n <- AC.decimal
  _ <- AS.skipWhile AC.isHorizontalSpace
  -- XXX: This should be more general, but then we also want to ensure that the
  -- order of states is correct.
  _ <-
    AS.string (BL.toStrict "A C D E F G H I K L M N P Q R S T V W Y")
      <|> AS.string (BL.toStrict "A C G T")
  _ <- AC.skipWhile AC.isSpace AS.<?> "headerLine"
  return n

kComponentsLine :: AS.Parser Int
kComponentsLine = AC.decimal <* AC.skipWhile AC.isSpace AS.<?> "kComponentsLine"

dataLine :: Int -> AS.Parser EDMComponent
dataLine n = (AS.<?> "dataLine") $ do
  wght <- AC.double
  _ <- AS.skipWhile AC.isHorizontalSpace
  vals <- AC.double `AC.sepBy1` AS.skipWhile AC.isHorizontalSpace
  _ <- AC.skipWhile AC.isSpace
  when (length vals /= n) (error "Did not find correct number of entries.")
  return (wght, V.fromList vals)
