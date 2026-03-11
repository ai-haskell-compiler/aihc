-- |
-- Module      :  ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
-- Description :  Import site profiles in Phylobayes format
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 12:12:55 2019.
--
-- For now I just try to go with a huge empirical distribution mixture model. Let's
-- see if performance is good enough.
--
-- There are subtle differences between
-- `ELynx.Import.MarkovProcess.EDMModelPhylobayes` and this module, which collects
-- one stationary distribution for each site.
module ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
  ( siteprofiles,
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.Containers.ListUtils (nubInt)
import qualified Data.Vector.Storable as V
import ELynx.Import.MarkovProcess.EDMModelPhylobayes

-- | Parse stationary distributions from Phylobayes format.
siteprofiles :: AS.Parser [EDMComponent]
siteprofiles = (AS.<?> "phylobayes siteprofiles") $ do
  _ <- headerLines
  cs <- many dataLine
  _ <- AC.skipWhile AC.isSpace
  _ <- AS.endOfInput
  let ls = map length cs
      nLs = length $ nubInt ls
  when
    (nLs /= 1)
    (error "The site profiles have a different number of entries.")
  return cs

line :: AS.Parser ()
line = AS.skipWhile (not . AC.isEndOfLine)

-- For now, just ignore the header.
headerLines :: AS.Parser ()
headerLines = line *> AC.skipWhile AC.isSpace AS.<?> "headerLine"

dataLine :: AS.Parser EDMComponent
dataLine = (AS.<?> "dataLine") $ do
  -- Ignore site number.
  _ <- AC.decimal :: AS.Parser Int
  _ <- AS.skipWhile AC.isHorizontalSpace
  -- Also ignore additional white space on line.
  vals <- AC.double `AS.sepBy1` AS.skipWhile AC.isHorizontalSpace
  _ <- AC.skipWhile AC.isSpace
  -- Set the weight to 1.0 for all sites.
  return (1.0, V.fromList vals)
