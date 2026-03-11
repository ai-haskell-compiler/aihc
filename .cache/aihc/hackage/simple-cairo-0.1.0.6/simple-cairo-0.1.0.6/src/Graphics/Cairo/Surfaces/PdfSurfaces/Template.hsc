{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.PdfSurfaces.Template where

import Language.Haskell.TH
import Data.Word
import Graphics.Cairo.Template

#include <cairo-pdf.h>

newtype CairoPdfOutlineFlagsT = CairoPdfOutlineFlagsT #{type cairo_pdf_outline_flags_t} deriving Show

mkOFlag :: String -> Integer -> DecsQ
mkOFlag = mkMemberGen ''CairoPdfOutlineFlagsT 'CairoPdfOutlineFlagsT

newtype CairoPdfMetadataT = CairoPdfMetadataT #{type cairo_pdf_metadata_t} deriving Show

mkMeta :: String -> Integer -> DecsQ
mkMeta = mkMemberGen ''CairoPdfMetadataT 'CairoPdfMetadataT

newtype CairoPdfVersionT = CairoPdfVersionT #{type cairo_pdf_version_t} deriving Show

mkVersion :: String -> Integer -> DecsQ
mkVersion = mkMemberGen ''CairoPdfVersionT 'CairoPdfVersionT
