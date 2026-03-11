{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Utilities.Types where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Struct
import Control.Monad.Primitive

#include <cairo.h>

struct "CairoRectangleIntT" #{size cairo_rectangle_int_t}
		#{alignment cairo_rectangle_int_t}
	[	("x", ''CInt, [| #{peek cairo_rectangle_int_t, x} |],
			[| #{poke cairo_rectangle_int_t, x} |]),
		("y", ''CInt, [| #{peek cairo_rectangle_int_t, y} |],
			[| #{poke cairo_rectangle_int_t, y} |]),
		("width", ''CInt, [| #{peek cairo_rectangle_int_t, width} |],
			[| #{poke cairo_rectangle_int_t, width} |]),
		("height", ''CInt, [| #{peek cairo_rectangle_int_t, height} |],
			[| #{poke cairo_rectangle_int_t, height} |]) ]
	[''Show]

c_cairo_rectangle_int_t_copy ::
	Ptr CairoRectangleIntT -> IO (Ptr CairoRectangleIntT)
c_cairo_rectangle_int_t_copy s = do
	d <- mallocBytes #{size cairo_rectangle_int_t}
	d <$ copyBytes d s #{size cairo_rectangle_int_t}

c_cairo_rectangle_int_t_free :: Ptr CairoRectangleIntT -> IO ()
c_cairo_rectangle_int_t_free = free

structPrim "CairoRectangleIntT"
	'c_cairo_rectangle_int_t_copy 'c_cairo_rectangle_int_t_free [''Show]

cairoRectangleIntTNew :: PrimMonad m => m (CairoRectangleIntTPrim (PrimState m))
cairoRectangleIntTNew = CairoRectangleIntTPrim <$> unsafeIOToPrim do
	p <- mallocBytes #{size cairo_rectangle_int_t}
	newForeignPtr p $ free p
