{-|

Module      : SDL.Font
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Stability   : experimental

Bindings to the @SDL2_ttf@ library
<http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html>
which itself is a wrapper around the FreeType library.
The bindings should allow you to load fonts and
render 'Text' in various styles to an @SDL@ 'Surface'.

You can safely assume that any monadic function listed here is capable of
throwing an 'SDLException' in case it encounters an error.

-}

{-# LANGUAGE CPP, DeriveGeneric, LambdaCase, OverloadedStrings #-}

module SDL.Font
  (
  -- * General
    initialize
  , version
  , quit

  -- * Loading
  --
  -- | Use the following functions to load @TTF@ and @FON@ file formats.
  , Font(..)
  , PointSize
  , load
  , Index
  , loadIndex
  , decode
  , decodeIndex
  , free

  -- * Rendering
  --
  -- | Use the following functions to render text to a 'Surface'.
  --
  -- The methods available are described in more detail in the original
  -- @SDL2_ttf@ documentation
  -- <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC42 here>.
  , Color
  , solid
  , shaded
  , blended
  , size

  -- * Attributes
  , Style(..)
  , getStyle
  , setStyle
  , Outline
  , getOutline
  , setOutline
  , Hinting(..)
  , getHinting
  , setHinting
  , Kerning
  , getKerning
  , setKerning
  , isMonospace
  , familyName
  , styleName
  , height
  , ascent
  , descent
  , lineSkip
  , getKerningSize

  -- * Glyphs
  --
  -- | Functions that work with individual glyphs.
  , glyphProvided
  , glyphIndex
  , glyphMetrics
  , solidGlyph
  , shadedGlyph
  , blendedGlyph
  , blendedWrapped
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCString, unsafeUseAsCStringLen)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import SDL (SDLException (SDLCallFailed), Surface (..))
import SDL.Internal.Exception
import SDL.Raw.Filesystem (rwFromConstMem)
import SDL.Vect (V4 (..))

import qualified Foreign.C.String
import qualified SDL.Raw
import qualified SDL.Raw.Font

-- stolen from https://github.com/haskell-game/dear-imgui.hs/blob/main/src/DearImGui/Internal/Text.hs
#if MIN_VERSION_text(2,0,1)

import qualified Data.Text.Foreign

withCString :: Text -> (CString -> IO a) -> IO a
withCString = Data.Text.Foreign.withCString

#else

import qualified Data.Text
import qualified GHC.Foreign
import qualified System.IO

withCString :: Text -> (CString -> IO a) -> IO a
withCString t action = do
  GHC.Foreign.withCString System.IO.utf8 (Data.Text.unpack t) $ \textPtr ->
    action textPtr

#endif

-- | Gets the major, minor, patch versions of the linked @SDL2_ttf@ library.
--
-- You may call this without initializing the library with 'initialize'.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Font.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Initializes the library.
--
-- Unless noted otherwise, this must be called before any other part of the
-- library is used.
--
-- You may call this multiple times.
initialize :: MonadIO m => m ()
initialize = do
  init'd <- (== 1) `fmap` SDL.Raw.Font.wasInit
  unless init'd $
    throwIfNeg_ "SDL.Font.initialize" "TTF_Init" SDL.Raw.Font.init

-- | Cleans up any resources still in use by the library.
--
-- If called, you must call 'initialize' again before using any other parts of
-- the library.
quit :: MonadIO m => m ()
quit = SDL.Raw.Font.quit

-- | Represents a loaded font.
newtype Font = Font { unwrap :: Ptr SDL.Raw.Font.Font }
  deriving (Eq, Show)

-- | Point size (based on 72DPI) to load font as. Translates to pixel height.
type PointSize = Int

-- | Given a path to a font file, loads it for use as a 'Font' at a certain
-- 'PointSize'.
load :: MonadIO m => FilePath -> PointSize -> m Font
load path pts =
  fmap Font .
    throwIfNull "SDL.Font.load" "TTF_OpenFont" .
      liftIO . Foreign.C.String.withCString path $
        flip SDL.Raw.Font.openFont $ fromIntegral pts

-- | Same as 'load', but accepts a 'ByteString' containing a font instead.
decode :: MonadIO m => ByteString -> PointSize -> m Font
decode bytes pts = liftIO .
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap Font .
      throwIfNull "SDL.Font.decode" "TTF_OpenFontRW" $
        SDL.Raw.Font.openFont_RW rw 0 $ fromIntegral pts

-- | Designates a font face, the default and first one being 0.
type Index = Int

-- | Given a path to a font file, loads one of its font faces (designated by
-- the given index) for use as a 'Font' at a certain 'PointSize'.
--
-- The first face is always index 0, and is the one chosen by default when
-- using 'load'.
loadIndex :: MonadIO m => FilePath -> PointSize -> Index -> m Font
loadIndex path pts i =
  fmap Font .
    throwIfNull "SDL.Font.loadIndex" "TTF_OpenFontIndex" .
      liftIO . Foreign.C.String.withCString path $ \cpath ->
        SDL.Raw.Font.openFontIndex cpath (fromIntegral pts) (fromIntegral i)

-- | Same as 'loadIndex', but accepts a 'ByteString' containing a font instead.
decodeIndex :: MonadIO m => ByteString -> PointSize -> Index -> m Font
decodeIndex bytes pts i = liftIO .
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap Font .
      throwIfNull "SDL.Font.decodeIndex" "TTF_OpenFontIndexRW" $
        SDL.Raw.Font.openFontIndex_RW rw 0 (fromIntegral pts) (fromIntegral i)

-- | Frees a loaded 'Font'.
free :: MonadIO m => Font -> m ()
free = SDL.Raw.Font.closeFont . unwrap

-- | Color as an RGBA byte vector.
type Color = V4 Word8

-- | A helper for unmanaged 'Surface's, since it is not exposed by SDL itself.
unmanaged :: Ptr SDL.Raw.Surface -> Surface
unmanaged p = Surface p Nothing

-- | Renders 'Text' using the /quick and dirty/ method.
--
-- Is the fastest of the rendering methods, but results in text that isn't as
-- /smooth/.
solid :: MonadIO m => Font -> Color -> Text -> m SDL.Surface
solid (Font font) (V4 r g b a) text =
  fmap unmanaged .
    throwIfNull "SDL.Font.solid" "TTF_RenderUTF8_Solid" .
      liftIO . withCString text $ \ptr ->
        with (SDL.Raw.Color r g b a) $ \fg ->
          SDL.Raw.Font.renderUTF8_Solid font (castPtr ptr) fg

-- | Uses the /slow and nice, but with a solid box/ method.
--
-- Renders slower than 'solid', but in about the same time as 'blended'.
--
-- Results in a 'Surface' containing antialiased text of a foreground color
-- surrounded by a box of a background color. This 'Surface' will blit as fast
-- as the one from 'solid'.
shaded :: MonadIO m => Font -> Color -> Color -> Text -> m SDL.Surface
shaded (Font font) (V4 r g b a) (V4 r2 g2 b2 a2) text =
  fmap unmanaged .
    throwIfNull "SDL.Font.shaded" "TTF_RenderUTF8_Shaded" .
      liftIO . withCString text $ \ptr ->
        with (SDL.Raw.Color r g b a) $ \fg ->
          with (SDL.Raw.Color r2 g2 b2 a2) $ \bg ->
            SDL.Raw.Font.renderUTF8_Shaded font (castPtr ptr) fg bg

-- | The /slow slow slow, but ultra nice over another image/ method, 'blended'
-- renders text at high quality.
--
-- The text is antialiased and surrounded by a transparent box. Renders slower
-- than 'solid', but in about the same time as 'shaded'.
--
-- The resulting 'Surface' will blit slower than the ones from 'solid' or
-- 'shaded'.
blended :: MonadIO m => Font -> Color -> Text -> m SDL.Surface
blended (Font font) (V4 r g b a) text =
  fmap unmanaged .
    throwIfNull "SDL.Font.blended" "TTF_RenderUTF8_Blended" .
      liftIO . withCString text $ \ptr ->
        with (SDL.Raw.Color r g b a) $ \fg ->
          SDL.Raw.Font.renderUTF8_Blended font (castPtr ptr) fg

-- Helper function for converting a bitmask into a list of values.
fromMaskWith :: (Enum a, Bounded a) => (a -> CInt) -> CInt -> [a]
fromMaskWith convert cint = concatMap (\a -> find (a, convert a)) [minBound..]
  where
    find (a, i) = [a | i == i .&. cint]

-- Helper function for converting a list of values into a bitmask.
toMaskWith :: (a -> CInt) -> [a] -> CInt
toMaskWith convert = foldr ((.|.) . convert) 0

-- | Possible styles that can be applied to a 'Font'.
data Style
  = Bold
  | Italic
  | Underline
  | Strikethrough
  deriving (Eq, Enum, Ord, Bounded, Generic, Read, Show)

styleToCInt :: Style -> CInt
styleToCInt =
  \case
    Bold          -> SDL.Raw.Font.TTF_STYLE_BOLD
    Italic        -> SDL.Raw.Font.TTF_STYLE_ITALIC
    Underline     -> SDL.Raw.Font.TTF_STYLE_UNDERLINE
    Strikethrough -> SDL.Raw.Font.TTF_STYLE_STRIKETHROUGH

-- | Gets the rendering styles of a given 'Font'.
--
-- If none were ever set, this will be an empty list.
getStyle :: MonadIO m => Font -> m [Style]
getStyle = fmap (fromMaskWith styleToCInt) . SDL.Raw.Font.getFontStyle . unwrap

-- | Sets the rendering style of a 'Font'.
--
-- Use an empty list to reset the style.
setStyle :: MonadIO m => Font -> [Style] -> m ()
setStyle (Font font) = SDL.Raw.Font.setFontStyle font . toMaskWith styleToCInt

-- | The size of the 'Font' outline, in pixels.
--
-- Use 0 to turn off outlining.
type Outline = Int

-- | Gets the current outline size of a given 'Font'.
getOutline :: MonadIO m => Font -> m Outline
getOutline = fmap fromIntegral . SDL.Raw.Font.getFontOutline . unwrap

-- | Sets the outline size for a given 'Font'.
--
-- Use 0 to turn off outlining.
setOutline :: MonadIO m => Font -> Outline -> m ()
setOutline (Font font) = SDL.Raw.Font.setFontOutline font . fromIntegral

-- | The hinting setting of a 'Font'.
data Hinting
  = Normal
  | Light
  | Mono
  | None
  deriving (Eq, Enum, Ord, Bounded, Generic, Read, Show)

hintingToCInt :: Hinting -> CInt
hintingToCInt =
  \case
    Normal -> SDL.Raw.Font.TTF_HINTING_NORMAL
    Light  -> SDL.Raw.Font.TTF_HINTING_LIGHT
    Mono   -> SDL.Raw.Font.TTF_HINTING_MONO
    None   -> SDL.Raw.Font.TTF_HINTING_NONE

cIntToHinting :: CInt -> Hinting
cIntToHinting =
  \case
    SDL.Raw.Font.TTF_HINTING_NORMAL -> Normal
    SDL.Raw.Font.TTF_HINTING_LIGHT  -> Light
    SDL.Raw.Font.TTF_HINTING_MONO   -> Mono
    SDL.Raw.Font.TTF_HINTING_NONE   -> None
    _ -> error "SDL.Font.cIntToHinting received unknown TTF_HINTING."

-- | Gets the hinting setting of a given 'Font'.
getHinting :: MonadIO m => Font -> m Hinting
getHinting = fmap cIntToHinting . SDL.Raw.Font.getFontHinting . unwrap

-- | Sets the hinting setting of a font.
setHinting :: MonadIO m => Font -> Hinting -> m ()
setHinting (Font font) = SDL.Raw.Font.setFontHinting font . hintingToCInt

-- | Whether kerning is enabled or not.
--
-- The default for a newly-loaded 'Font' is enabled.
type Kerning = Bool

-- | Gets the current kerning setting of a given 'Font'.
getKerning :: MonadIO m => Font -> m Kerning
getKerning = fmap toBool . SDL.Raw.Font.getFontKerning . unwrap

-- | Sets the kerning setting for a given 'Font'.
--
-- Use 'False' to turn off kerning.
setKerning :: MonadIO m => Font -> Kerning -> m ()
setKerning (Font font) = SDL.Raw.Font.setFontKerning font . fromBool

-- | Gets the maximum pixel height of all glyphs of a given 'Font'.
height :: MonadIO m => Font -> m Int
height = fmap fromIntegral . SDL.Raw.Font.fontHeight . unwrap

-- | Gets the maximum pixel ascent of all glyphs of a given 'Font'.
--
-- This can be interpreted as the distance from the top of the font to the
-- baseline.
ascent :: MonadIO m => Font -> m Int
ascent = fmap fromIntegral . SDL.Raw.Font.fontAscent . unwrap

-- | Gets the maximum pixel descent of all glyphs of a given 'Font'.
--
-- Also interpreted as the distance from the baseline to the bottom of the
-- font.
descent :: MonadIO m => Font -> m Int
descent = fmap fromIntegral . SDL.Raw.Font.fontDescent . unwrap

-- | Gets the recommended pixel height of a rendered line of text of a given
-- 'Font'.
--
-- This is usually larger than what 'height' would return.
lineSkip :: MonadIO m => Font -> m Int
lineSkip = fmap fromIntegral . SDL.Raw.Font.fontLineSkip . unwrap

-- | Tests whether the current face of a 'Font' is a fixed width font or not.
isMonospace :: MonadIO m => Font -> m Bool
isMonospace = fmap toBool . SDL.Raw.Font.fontFaceIsFixedWidth . unwrap

cStringToText :: MonadIO m => CString -> m Text
cStringToText = fmap decodeUtf8 . liftIO . unsafePackCString

onlyIfM :: Monad m => Bool -> m a -> m (Maybe a)
onlyIfM = \case
  False -> return . const Nothing
  True  -> fmap Just

-- | Gets the current font face family name, if any.
familyName :: MonadIO m => Font -> m (Maybe Text)
familyName (Font font) = do
  cstr <- SDL.Raw.Font.fontFaceFamilyName font
  onlyIfM (cstr /= nullPtr) $ cStringToText cstr

-- | Gets the current font face style name, if any.
styleName :: MonadIO m => Font -> m (Maybe Text)
styleName (Font font) = do
  cstr <- SDL.Raw.Font.fontFaceStyleName font
  onlyIfM (cstr /= nullPtr) $ cStringToText cstr

-- | Does a 'Font' provide a certain unicode character?
glyphProvided :: MonadIO m => Font -> Char -> m Bool
glyphProvided font ch =
  glyphIndex font ch >>= \case
    Just  _ -> return True
    Nothing -> return False

{-# INLINE fromChar #-}
fromChar :: Integral a => Char -> a
fromChar = fromIntegral . fromEnum

-- | Same as 'glyphProvided', but returns an index of the glyph for the given
-- character instead, if one is provided.
glyphIndex :: MonadIO m => Font -> Char -> m (Maybe Int)
glyphIndex (Font font) ch =
  SDL.Raw.Font.glyphIsProvided font (fromChar ch)
    >>= \case
      0 -> return Nothing
      i -> return . Just $ fromIntegral i

-- | Get glyph metrics for a given unicode character. The values returned are:
--
--     1. minimum x offset
--     2. maximum x offset
--     3. minimum y offset
--     4. maximum y offset
--     5. advance offset
--
-- You can see more information about these values in the original @SDL2_ttf@
-- documentation
-- <http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC38 here>.
glyphMetrics :: MonadIO m => Font -> Char -> m (Maybe (Int, Int, Int, Int, Int))
glyphMetrics (Font font) ch =
  liftIO .
    alloca $ \minx ->
      alloca $ \maxx ->
        alloca $ \miny ->
          alloca $ \maxy ->
            alloca $ \advn -> do
              let chi = fromChar ch
              r <- SDL.Raw.Font.glyphMetrics font chi minx maxx miny maxy advn
              if r /= 0 then
                return Nothing
              else do
                a <- fromIntegral <$> peek minx
                b <- fromIntegral <$> peek maxx
                c <- fromIntegral <$> peek miny
                d <- fromIntegral <$> peek maxy
                e <- fromIntegral <$> peek advn
                return $ Just (a, b, c, d, e)

-- | Use this function to discover how wide and tall a 'Surface' needs to be
-- in order to accommodate a given text when it is rendered.
--
-- Note that no actual rendering takes place.
--
-- The values returned are the width and height, respectively, in pixels. The
-- height returned is the same one returned by 'height'.
size :: MonadIO m => Font -> Text -> m (Int, Int)
size (Font font) text =
    liftIO .
      withCString text $ \ptr ->
        alloca $ \w ->
          alloca $ \h ->
            SDL.Raw.Font.sizeUTF8 font (castPtr ptr) w h
              >>= \case
                0 -> do
                  w' <- fromIntegral <$> peek w
                  h' <- fromIntegral <$> peek h
                  return (w', h')
                _ -> do
                  err <- getError
                  throwIO $ SDLCallFailed "SDL.Font.size" "TTF_SizeUTF8" err

-- | Same as 'solid', but renders a single glyph instead.
solidGlyph :: MonadIO m => Font -> Color -> Char -> m SDL.Surface
solidGlyph (Font font) (V4 r g b a) ch =
  fmap unmanaged .
    throwIfNull "SDL.Font.solidGlyph" "TTF_RenderGlyph_Solid" .
      liftIO .
        with (SDL.Raw.Color r g b a) $ \fg ->
          SDL.Raw.Font.renderGlyph_Solid font (fromChar ch) fg

-- | Same as 'shaded', but renders a single glyph instead.
shadedGlyph :: MonadIO m => Font -> Color -> Color -> Char -> m SDL.Surface
shadedGlyph (Font font) (V4 r g b a) (V4 r2 g2 b2 a2) ch =
  fmap unmanaged .
    throwIfNull "SDL.Font.shadedGlyph" "TTF_RenderGlyph_Solid" .
      liftIO .
        with (SDL.Raw.Color r g b a) $ \fg ->
          with (SDL.Raw.Color r2 g2 b2 a2) $ \bg ->
            SDL.Raw.Font.renderGlyph_Shaded font (fromChar ch) fg bg

-- | Same as 'blended', but renders a single glyph instead.
blendedGlyph :: MonadIO m => Font -> Color -> Char -> m SDL.Surface
blendedGlyph (Font font) (V4 r g b a) ch =
  fmap unmanaged .
    throwIfNull "SDL.Font.blendedGlyph" "TTF_RenderGlyph_Blended" .
      liftIO .
        with (SDL.Raw.Color r g b a) $ \fg ->
          SDL.Raw.Font.renderGlyph_Blended font (fromChar ch) fg

-- | Same as 'blended', but renders across multiple lines.
--   Text is wrapped to multiple lines on line endings and on word boundaries
--   if it extends beyond wrapLength in pixels.
blendedWrapped :: MonadIO m => Font -> Color -> Int -> Text -> m SDL.Surface
blendedWrapped (Font font) (V4 r g b a) wrapLength text =
  fmap unmanaged .
    throwIfNull "SDL.Font.blended" "TTF_RenderUTF8_Blended_Wrapped" .
      liftIO . withCString text $ \ptr ->
        with (SDL.Raw.Color r g b a) $ \fg ->
          SDL.Raw.Font.renderUTF8_Blended_Wrapped font (castPtr ptr) fg $ fromIntegral wrapLength

-- | From a given 'Font' get the kerning size of two glyphs.
getKerningSize :: MonadIO m => Font -> Index -> Index -> m Int
getKerningSize (Font font) prevIndex index =
  fmap fromIntegral $ SDL.Raw.Font.getFontKerningSize font (fromIntegral prevIndex) (fromIntegral index)
