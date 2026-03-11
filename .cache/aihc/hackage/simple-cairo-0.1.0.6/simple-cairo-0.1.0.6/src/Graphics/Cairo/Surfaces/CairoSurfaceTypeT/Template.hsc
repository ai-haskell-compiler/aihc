{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceTypeT.Template where

import Language.Haskell.TH
import Data.Word

import Graphics.Cairo.Template

#include <cairo.h>

newtype CairoSurfaceTypeT = CairoSurfaceTypeT #{type cairo_surface_type_t} deriving Show

mkMember :: String -> Integer -> DecsQ
mkMember = mkMemberGen ''CairoSurfaceTypeT 'CairoSurfaceTypeT
