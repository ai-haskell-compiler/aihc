{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Setting where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive
import Data.Word
import Graphics.Cairo.Drawing.CairoPatternT.Basic
import Graphics.Cairo.Utilities.CairoMatrixT.Internal

#include <cairo.h>

class CairoPatternSetting s where
	cairoPatternSet :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> s -> m ()
	cairoPatternGet :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> m s

newtype CairoExtendT = CairoExtendT #{type cairo_extend_t} deriving Show

pattern CairoExtendNone :: CairoExtendT
pattern CairoExtendNone <- CairoExtendT #{const CAIRO_EXTEND_NONE} where
	CairoExtendNone = CairoExtendT #{const CAIRO_EXTEND_NONE}

pattern CairoExtendRepeat :: CairoExtendT
pattern CairoExtendRepeat <- CairoExtendT #{const CAIRO_EXTEND_REPEAT} where
	CairoExtendRepeat = CairoExtendT #{const CAIRO_EXTEND_REPEAT}

pattern CairoExtendReflect :: CairoExtendT
pattern CairoExtendReflect <- CairoExtendT #{const CAIRO_EXTEND_REFLECT} where
	CairoExtendReflect = CairoExtendT #{const CAIRO_EXTEND_REFLECT}

pattern CairoExtendPad :: CairoExtendT
pattern CairoExtendPad <- CairoExtendT #{const CAIRO_EXTEND_PAD} where
	CairoExtendPad = CairoExtendT #{const CAIRO_EXTEND_PAD}

instance CairoPatternSetting CairoExtendT where
	cairoPatternSet = cairoPatternSetExtend
	cairoPatternGet = cairoPatternGetExtend

cairoPatternSetExtend :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> CairoExtendT -> m ()
cairoPatternSetExtend (toCairoPatternT -> CairoPatternT fpt) (CairoExtendT ex) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> c_cairo_pattern_set_extend ppt ex

foreign import ccall "cairo_pattern_set_extend" c_cairo_pattern_set_extend ::
	Ptr (CairoPatternT s) -> #{type cairo_extend_t} -> IO ()

cairoPatternGetExtend :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> m CairoExtendT
cairoPatternGetExtend (toCairoPatternT -> CairoPatternT fpt) =
	unsafeIOToPrim $ CairoExtendT <$> withForeignPtr fpt c_cairo_pattern_get_extend

foreign import ccall "cairo_pattern_get_extend" c_cairo_pattern_get_extend ::
	Ptr (CairoPatternT s) -> IO #{type cairo_extend_t}

newtype CairoFilterT = CairoFilterT #{type cairo_filter_t} deriving Show

pattern CairoFilterFast :: CairoFilterT
pattern CairoFilterFast <- CairoFilterT #{const CAIRO_FILTER_FAST} where
	CairoFilterFast = CairoFilterT #{const CAIRO_FILTER_FAST}

pattern CairoFilterGood :: CairoFilterT
pattern CairoFilterGood <- CairoFilterT #{const CAIRO_FILTER_GOOD} where
	CairoFilterGood = CairoFilterT #{const CAIRO_FILTER_GOOD}

pattern CairoFilterBest :: CairoFilterT
pattern CairoFilterBest <- CairoFilterT #{const CAIRO_FILTER_BEST} where
	CairoFilterBest = CairoFilterT #{const CAIRO_FILTER_BEST}

pattern CairoFilterNearest :: CairoFilterT
pattern CairoFilterNearest <- CairoFilterT #{const CAIRO_FILTER_NEAREST} where
	CairoFilterNearest = CairoFilterT #{const CAIRO_FILTER_NEAREST}

pattern CairoFilterBilinear :: CairoFilterT
pattern CairoFilterBilinear <- CairoFilterT #{const CAIRO_FILTER_BILINEAR} where
	CairoFilterBilinear = CairoFilterT #{const CAIRO_FILTER_BILINEAR}

pattern CairoFilterGaussian :: CairoFilterT
pattern CairoFilterGaussian <- CairoFilterT #{const CAIRO_FILTER_GAUSSIAN} where
	CairoFilterGaussian = CairoFilterT #{const CAIRO_FILTER_GAUSSIAN}

instance CairoPatternSetting CairoFilterT where
	cairoPatternSet = cairoPatternSetFilter
	cairoPatternGet = cairoPatternGetFilter

cairoPatternSetFilter :: (PrimMonad m, IsCairoPatternT pt) =>
	pt (PrimState m) -> CairoFilterT -> m ()
cairoPatternSetFilter (toCairoPatternT -> CairoPatternT fpt) (CairoFilterT flt) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> c_cairo_pattern_set_filter ppt flt

foreign import ccall "cairo_pattern_set_filter" c_cairo_pattern_set_filter ::
	Ptr (CairoPatternT s) -> #{type cairo_filter_t} -> IO ()

cairoPatternGetFilter :: (PrimMonad m, IsCairoPatternT pt) =>
	pt (PrimState m) -> m CairoFilterT
cairoPatternGetFilter (toCairoPatternT -> CairoPatternT fpt) =
	unsafeIOToPrim $ CairoFilterT <$> withForeignPtr fpt c_cairo_pattern_get_filter

foreign import ccall "cairo_pattern_get_filter" c_cairo_pattern_get_filter ::
	Ptr (CairoPatternT s) -> IO #{type cairo_filter_t}

cairoPatternSetMatrix :: (PrimMonad m, IsCairoPatternT pt, IsCairoMatrixT mtx) =>
	pt (PrimState m) -> mtx (PrimState m) -> m ()
cairoPatternSetMatrix (toCairoPatternT -> CairoPatternT fpt) (toCairoMatrixT -> CairoMatrixT fmtx) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> withForeignPtr fmtx \pmtx ->
		c_cairo_pattern_set_matrix ppt pmtx

foreign import ccall "cairo_pattern_set_matrix" c_cairo_pattern_set_matrix ::
	Ptr (CairoPatternT s) -> Ptr (CairoMatrixT s) -> IO ()

cairoPatternGetMatrix :: (PrimMonad m, IsCairoPatternT pt) =>
	pt (PrimState m) -> m (CairoMatrixT (PrimState m))
cairoPatternGetMatrix (toCairoPatternT -> CairoPatternT fpt) = (CairoMatrixT <$>)
	$ cairoMatrixAlloc \pmtx -> withForeignPtr fpt \ppt ->
		c_cairo_pattern_get_matrix ppt pmtx

foreign import ccall "cairo_pattern_get_matrix" c_cairo_pattern_get_matrix ::
	Ptr (CairoPatternT s) -> Ptr (CairoMatrixT s) -> IO ()
