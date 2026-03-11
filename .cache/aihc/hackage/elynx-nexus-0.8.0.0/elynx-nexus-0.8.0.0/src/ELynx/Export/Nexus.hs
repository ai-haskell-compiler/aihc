{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Export.Nexus
-- Description :  Nexus types and classes
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 17:10:05 2020.
module ELynx.Export.Nexus
  ( toNexus,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL

-- | Create nexus file with block name and block body.
--
-- At the moment writing one block only is supported.
toNexus :: BL.ByteString -> [BL.ByteString] -> BL.ByteString
toNexus n b = BL.unlines $ [start, begin n] <> b <> [end]

start :: BL.ByteString
start = "#NEXUS"

begin :: BL.ByteString -> BL.ByteString
begin n = "BEGIN " <> n <> ";"

end :: BL.ByteString
end = "END;"
