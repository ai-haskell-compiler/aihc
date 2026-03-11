{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-|

Module      : SDL.Raw.Font
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Stability   : experimental

Raw bindings to the @SDL2_ttf@ library. No error-handling is done here. For more
information about specific function behaviour, see the @SDL2_ttf@ documentation.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Font
  (
  -- * General
    init
  , wasInit
  , quit
  , getVersion

  -- * Loading fonts
  , Font
  , FontPath
  , PointSize
  , openFont
  , Free
  , openFont_RW
  , Index
  , openFontIndex
  , openFontIndex_RW
  , closeFont

  -- * Font attributes
  , getFontStyle
  , setFontStyle
  , pattern TTF_STYLE_NORMAL
  , pattern TTF_STYLE_BOLD
  , pattern TTF_STYLE_ITALIC
  , pattern TTF_STYLE_UNDERLINE
  , pattern TTF_STYLE_STRIKETHROUGH
  , getFontOutline
  , setFontOutline
  , getFontHinting
  , setFontHinting
  , pattern TTF_HINTING_NORMAL
  , pattern TTF_HINTING_LIGHT
  , pattern TTF_HINTING_MONO
  , pattern TTF_HINTING_NONE
  , getFontKerning
  , setFontKerning
  , fontHeight
  , fontAscent
  , fontDescent
  , fontLineSkip
  , fontFaces
  , fontFaceIsFixedWidth
  , fontFaceFamilyName
  , fontFaceStyleName
  , glyphIsProvided
  , glyphMetrics
  , getFontKerningSize

  -- * Getting text size
  , sizeText
  , sizeUTF8
  , sizeUNICODE

  -- * Rendering text
  , renderText_Solid
  , renderText_Shaded
  , renderText_Blended
  , renderText_Blended_Wrapped
  , renderUTF8_Solid
  , renderUTF8_Shaded
  , renderUTF8_Blended
  , renderUTF8_Blended_Wrapped
  , renderUNICODE_Solid
  , renderUNICODE_Shaded
  , renderUNICODE_Blended
  , renderUNICODE_Blended_Wrapped
  , renderGlyph_Solid
  , renderGlyph_Shaded
  , renderGlyph_Blended

  -- * Other
  , byteSwappedUNICODE
  , pattern UNICODE_BOM_NATIVE
  , pattern UNICODE_BOM_SWAPPED
  ) where

#include "SDL_ttf.h"

import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..), CLong(..), CUShort(..), CUInt(..))
import Foreign.Ptr            (Ptr)
import Prelude         hiding (init)
import SDL.Raw.Types          (Version, Surface, RWops, Color)
import SDL.Raw.Helper         (liftF)

pattern UNICODE_BOM_NATIVE      = #{const UNICODE_BOM_NATIVE}
pattern UNICODE_BOM_SWAPPED     = #{const UNICODE_BOM_SWAPPED}
pattern TTF_STYLE_NORMAL        = #{const TTF_STYLE_NORMAL}
pattern TTF_STYLE_BOLD          = #{const TTF_STYLE_BOLD}
pattern TTF_STYLE_ITALIC        = #{const TTF_STYLE_ITALIC}
pattern TTF_STYLE_UNDERLINE     = #{const TTF_STYLE_UNDERLINE}
pattern TTF_STYLE_STRIKETHROUGH = #{const TTF_STYLE_STRIKETHROUGH}
pattern TTF_HINTING_LIGHT       = #{const TTF_HINTING_LIGHT}
pattern TTF_HINTING_MONO        = #{const TTF_HINTING_MONO}
pattern TTF_HINTING_NONE        = #{const TTF_HINTING_NONE}
pattern TTF_HINTING_NORMAL      = #{const TTF_HINTING_NORMAL}

liftF "getVersion" "TTF_Linked_Version"
  [t|IO (Ptr Version)|]

liftF "init" "TTF_Init"
  [t|IO CInt|]

liftF "wasInit" "TTF_WasInit"
  [t|IO CInt|]

liftF "quit" "TTF_Quit"
  [t|IO ()|]

-- | A path to a font file.
type FontPath = CString

-- | Point size (based on 72DPI). Translates to pixel height.
type PointSize = CInt

-- | The raw, underlying @TTF_Font@ struct.
data Font

-- | Should the 'Ptr' 'RWops' be freed after an operation? 1 for yes, 0 for no.
type Free = CInt

-- | Indicates the font face we're loading. First face is always 0.
type Index = CLong

liftF "openFont" "TTF_OpenFont"
  [t|FontPath -> PointSize -> IO (Ptr Font)|]

liftF "openFont_RW" "TTF_OpenFontRW"
  [t|Ptr RWops -> Free -> PointSize -> IO (Ptr Font)|]

liftF "openFontIndex" "TTF_OpenFontIndex"
  [t|FontPath -> PointSize -> Index -> IO (Ptr Font)|]

liftF "openFontIndex_RW" "TTF_OpenFontIndexRW"
  [t|Ptr RWops -> Free -> PointSize -> Index -> IO (Ptr Font)|]

liftF "closeFont" "TTF_CloseFont"
  [t|Ptr Font -> IO ()|]

liftF "byteSwappedUNICODE" "TTF_ByteSwappedUNICODE"
  [t|CInt -> IO ()|]

liftF "getFontStyle" "TTF_GetFontStyle"
  [t|Ptr Font -> IO CInt|]

liftF "setFontStyle" "TTF_SetFontStyle"
  [t|Ptr Font -> CInt -> IO ()|]

liftF "getFontOutline" "TTF_GetFontOutline"
  [t|Ptr Font -> IO CInt|]

liftF "setFontOutline" "TTF_SetFontOutline"
  [t|Ptr Font -> CInt -> IO ()|]

liftF "getFontHinting" "TTF_GetFontHinting"
  [t|Ptr Font -> IO CInt|]

liftF "setFontHinting" "TTF_SetFontHinting"
  [t|Ptr Font -> CInt -> IO ()|]

liftF "getFontKerning" "TTF_GetFontKerning"
  [t|Ptr Font -> IO CInt|]

liftF "setFontKerning" "TTF_SetFontKerning"
  [t|Ptr Font -> CInt -> IO ()|]

liftF "fontHeight" "TTF_FontHeight"
  [t|Ptr Font -> IO CInt|]

liftF "fontAscent" "TTF_FontAscent"
  [t|Ptr Font -> IO CInt|]

liftF "fontDescent" "TTF_FontDescent"
  [t|Ptr Font -> IO CInt|]

liftF "fontLineSkip" "TTF_FontLineSkip"
  [t|Ptr Font -> IO CInt|]

liftF "fontFaces" "TTF_FontFaces"
  [t|Ptr Font -> IO CLong|]

liftF "fontFaceIsFixedWidth" "TTF_FontFaceIsFixedWidth"
  [t|Ptr Font -> IO CInt|]

liftF "fontFaceFamilyName" "TTF_FontFaceFamilyName"
  [t|Ptr Font -> IO CString|]

liftF "fontFaceStyleName"  "TTF_FontFaceStyleName"
  [t|Ptr Font -> IO CString|]

liftF "glyphIsProvided" "TTF_GlyphIsProvided"
  [t|Ptr Font -> CUShort -> IO CInt|]

liftF "glyphMetrics" "TTF_GlyphMetrics"
  [t|Ptr Font -> CUShort ->
     Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt ->
     IO CInt|]

liftF "sizeText" "TTF_SizeText"
  [t|Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> IO CInt|]

liftF "sizeUTF8" "TTF_SizeUTF8"
  [t|Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> IO CInt|]

liftF "sizeUNICODE" "TTF_SizeUNICODE"
  [t|Ptr Font -> Ptr CUShort -> Ptr CInt -> Ptr CInt -> IO CInt|]

liftF "renderText_Solid" "TTF_RenderText_Solid_p"
  [t|Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderUTF8_Solid" "TTF_RenderUTF8_Solid_p"
  [t|Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderUNICODE_Solid" "TTF_RenderUNICODE_Solid_p"
  [t|Ptr Font -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderGlyph_Solid" "TTF_RenderGlyph_Solid_p"
  [t|Ptr Font -> CUShort -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderText_Shaded" "TTF_RenderText_Shaded_p"
  [t|Ptr Font -> CString -> Ptr Color -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderUTF8_Shaded" "TTF_RenderUTF8_Shaded_p"
  [t|Ptr Font -> CString -> Ptr Color -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderUNICODE_Shaded" "TTF_RenderUNICODE_Shaded_p"
  [t|Ptr Font -> Ptr CUShort -> Ptr Color -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderGlyph_Shaded" "TTF_RenderGlyph_Shaded_p"
  [t|Ptr Font -> CUShort -> Ptr Color -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderText_Blended" "TTF_RenderText_Blended_p"
  [t|Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderUTF8_Blended" "TTF_RenderUTF8_Blended_p"
  [t|Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderUNICODE_Blended" "TTF_RenderUNICODE_Blended_p"
  [t|Ptr Font -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderGlyph_Blended" "TTF_RenderGlyph_Blended_p"
  [t|Ptr Font -> CUShort -> Ptr Color -> IO (Ptr Surface)|]

liftF "renderText_Blended_Wrapped" "TTF_RenderText_Blended_Wrapped_p"
  [t|Ptr Font -> CString -> Ptr Color -> CUInt -> IO (Ptr Surface)|]

liftF "renderUTF8_Blended_Wrapped" "TTF_RenderUTF8_Blended_Wrapped_p"
  [t|Ptr Font -> CString -> Ptr Color -> CUInt -> IO (Ptr Surface)|]

liftF "renderUNICODE_Blended_Wrapped" "TTF_RenderUNICODE_Blended_Wrapped_p"
  [t|Ptr Font -> Ptr CUShort -> Ptr Color -> CUInt -> IO (Ptr Surface)|]

liftF "getFontKerningSize" "TTF_GetFontKerningSize"
  [t|Ptr Font -> CInt -> CInt -> IO CInt|]
