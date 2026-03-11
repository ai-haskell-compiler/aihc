{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.SvgSurfaces.Template where

import Language.Haskell.TH
import Data.Word
import Graphics.Cairo.Template

#include <cairo-svg.h>

newtype CairoSvgUnitT = CairoSvgUnitT #{type cairo_svg_unit_t} deriving Show

mkUnitMember :: String -> Integer -> DecsQ
mkUnitMember = mkMemberGen ''CairoSvgUnitT 'CairoSvgUnitT

newtype CairoSvgVersionT = CairoSvgVersionT #{type cairo_svg_version_t}
	deriving Show

mkVersionMember :: String -> Integer -> DecsQ
mkVersionMember = mkMemberGen ''CairoSvgVersionT 'CairoSvgVersionT
