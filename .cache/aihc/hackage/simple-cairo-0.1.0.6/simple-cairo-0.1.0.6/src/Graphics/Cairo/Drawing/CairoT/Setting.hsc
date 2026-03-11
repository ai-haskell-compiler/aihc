{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Setting where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Word
import Data.CairoContext
import Graphics.Cairo.Exception

import Graphics.Cairo.Drawing.CairoT.CairoOperatorT

#include <cairo.h>

class CairoSetting s where
	cairoSet :: PrimMonad m => CairoT r (PrimState m) -> s -> m ()
	cairoGet :: PrimMonad m => CairoT r (PrimState m) -> m s

newtype LineWidth = LineWidth { getLineWidth :: CDouble } deriving Show

instance CairoSetting LineWidth where
	cairoSet cr = cairoSetLineWidth cr . getLineWidth
	cairoGet cr = LineWidth <$> withCairoT cr c_cairo_get_line_width

cairoSetLineWidth :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> m ()
cairoSetLineWidth cr w = withCairoT cr \pcr -> c_cairo_set_line_width pcr w

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr (CairoT r s) -> CDouble -> IO ()

foreign import ccall "cairo_get_line_width" c_cairo_get_line_width ::
	Ptr (CairoT r s) -> IO CDouble

data Dash = Dash [CDouble] CDouble deriving Show

instance CairoSetting Dash where
	cairoSet cr (Dash ds ofs) = cairoSetDash cr ds ofs
	cairoGet cr = uncurry Dash <$> cairoGetDash cr

cairoSetDash :: PrimMonad m => CairoT r (PrimState m) -> [CDouble] -> CDouble -> m ()
cairoSetDash cr ds ofs = withCairoT cr \pcr -> withArrayLen ds \lds pds -> do
	c_cairo_set_dash pcr pds (fromIntegral lds) ofs
	raiseIfError cr

foreign import ccall "cairo_set_dash" c_cairo_set_dash ::
	Ptr (CairoT r s) -> Ptr CDouble -> CInt -> CDouble -> IO ()

cairoGetDash :: PrimMonad m => CairoT r (PrimState m) -> m ([CDouble], CDouble)
cairoGetDash cr = withCairoT cr \pcr -> do
	ln <- fromIntegral <$> c_cairo_get_dash_count pcr
	allocaArray ln \ds -> alloca \ofs -> do
		c_cairo_get_dash pcr ds ofs
		(,) <$> peekArray ln ds <*> peek ofs

foreign import ccall "cairo_get_dash_count" c_cairo_get_dash_count :: Ptr (CairoT r s) -> IO CInt

foreign import ccall "cairo_get_dash" c_cairo_get_dash ::
	Ptr (CairoT r s) -> Ptr CDouble -> Ptr CDouble -> IO ()

newtype FillRule = FillRule #{type cairo_fill_rule_t} deriving Show

pattern FillRuleWinding :: FillRule
pattern FillRuleWinding <- FillRule #{const CAIRO_FILL_RULE_WINDING} where
	FillRuleWinding = FillRule #{const CAIRO_FILL_RULE_WINDING}

pattern FillRuleEvenOdd :: FillRule
pattern FillRuleEvenOdd <- FillRule #{const CAIRO_FILL_RULE_EVEN_ODD} where
	FillRuleEvenOdd = FillRule #{const CAIRO_FILL_RULE_EVEN_ODD}

instance CairoSetting FillRule where
	cairoSet cr (FillRule fr) = withCairoT cr \pcr -> c_cairo_set_fill_rule pcr fr
	cairoGet cr = FillRule <$> withCairoT cr c_cairo_get_fill_rule

foreign import ccall "cairo_set_fill_rule" c_cairo_set_fill_rule ::
	Ptr (CairoT r s) -> #{type cairo_fill_rule_t} -> IO ()

foreign import ccall "cairo_get_fill_rule" c_cairo_get_fill_rule ::
	Ptr (CairoT r s) -> IO #{type cairo_fill_rule_t}

newtype LineCap = LineCap {
	getLineCap :: #{type cairo_line_cap_t} } deriving Show

pattern LineCapButt :: LineCap
pattern LineCapButt <- LineCap #{const CAIRO_LINE_CAP_BUTT} where
	LineCapButt = LineCap #{const CAIRO_LINE_CAP_BUTT}

pattern LineCapRound :: LineCap
pattern LineCapRound <- LineCap #{const CAIRO_LINE_CAP_ROUND} where
	LineCapRound = LineCap #{const CAIRO_LINE_CAP_ROUND}

pattern LineCapSquare :: LineCap
pattern LineCapSquare <- LineCap #{const CAIRO_LINE_CAP_SQUARE} where
	LineCapSquare = LineCap #{const CAIRO_LINE_CAP_SQUARE}

instance CairoSetting LineCap where
	cairoSet cr (LineCap c) = withCairoT cr (`c_cairo_set_line_cap` c)
	cairoGet cr = LineCap <$> withCairoT cr c_cairo_get_line_cap

foreign import ccall "cairo_set_line_cap" c_cairo_set_line_cap ::
	Ptr (CairoT r s) -> #{type cairo_line_cap_t} -> IO ()

foreign import ccall "cairo_get_line_cap" c_cairo_get_line_cap ::
	Ptr (CairoT r s) -> IO #{type cairo_line_cap_t}

newtype LineJoin = LineJoin #{type cairo_line_join_t} deriving Show

pattern LineJoinMiter :: LineJoin
pattern LineJoinMiter <- LineJoin #{const CAIRO_LINE_JOIN_MITER} where
	LineJoinMiter = LineJoin #{const CAIRO_LINE_JOIN_MITER}

pattern LineJoinRound :: LineJoin
pattern LineJoinRound <- LineJoin #{const CAIRO_LINE_JOIN_ROUND} where
	LineJoinRound = LineJoin #{const CAIRO_LINE_JOIN_ROUND}

pattern LineJoinBevel :: LineJoin
pattern LineJoinBevel <- LineJoin #{const CAIRO_LINE_JOIN_BEVEL} where
	LineJoinBevel = LineJoin #{const CAIRO_LINE_JOIN_BEVEL}

instance CairoSetting LineJoin where
	cairoSet cr (LineJoin j) = withCairoT cr (`c_cairo_set_line_join` j)
	cairoGet cr = LineJoin <$> withCairoT cr c_cairo_get_line_join

foreign import ccall "cairo_set_line_join" c_cairo_set_line_join ::
	Ptr (CairoT r s) -> #{type cairo_line_join_t} -> IO ()

foreign import ccall "cairo_get_line_join" c_cairo_get_line_join ::
	Ptr (CairoT r s) -> IO #{type cairo_line_join_t}

newtype MiterLimit = MiterLimit CDouble deriving Show

instance CairoSetting MiterLimit where
	cairoSet cr (MiterLimit ml) = withCairoT cr (`c_cairo_set_miter_limit` ml)
	cairoGet cr = MiterLimit <$> withCairoT cr c_cairo_get_miter_limit

foreign import ccall "cairo_set_miter_limit" c_cairo_set_miter_limit ::
	Ptr (CairoT r s) -> CDouble -> IO ()

foreign import ccall "cairo_get_miter_limit" c_cairo_get_miter_limit ::
	Ptr (CairoT r s) -> IO CDouble

instance CairoSetting Operator where
	cairoSet cr (Operator o) = withCairoT cr (`c_cairo_set_operator` o)
	cairoGet cr = Operator <$> withCairoT cr c_cairo_get_operator

foreign import ccall "cairo_set_operator" c_cairo_set_operator ::
	Ptr (CairoT r s) -> #{type cairo_operator_t} -> IO ()

foreign import ccall "cairo_get_operator" c_cairo_get_operator ::
	Ptr (CairoT r s) -> IO #{type cairo_operator_t}
