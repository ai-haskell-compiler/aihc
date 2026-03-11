module Graphics.GD.ByteString (
                    -- * Types
                    Image, Size, Point, Color,
                    -- * Creating and copying images
                    GD.newImage, GD.copyImage, 
                    GD.copyRegion, GD.copyRegionScaled,
                    -- * Memory management
                    GD.withImage,
                    -- * Loading images
                    -- ** JPEG
                    loadJpegFile, loadJpegData, loadJpegByteString,
                    -- ** PNG
                    loadPngFile, loadPngData, loadPngByteString,
                    -- ** GIF
                    loadGifFile, loadGifData, loadGifByteString,
                    -- * Saving images
                    -- ** JPEG
                    saveJpegFile, saveJpegByteString,
                    -- ** PNG
                    savePngFile, savePngByteString,
                    -- ** GIF
                    saveGifFile, saveGifByteString,
                    -- * Getting image information
                    GD.imageSize,
                    -- * Querying
                    GD.getPixel,
                    -- * Manipulating images
                    GD.resizeImage, GD.rotateImage,
                    -- * Drawing
                    GD.fillImage,
                    GD.drawFilledRectangle,
                    GD.drawFilledEllipse,
                    GD.drawLine,
                    GD.drawArc,
                    GD.antiAliased,
                    GD.setPixel,
                    -- * Text
                    GD.useFontConfig,
                    drawString,
                    measureString,
                    drawStringCircle,
                    -- * Colors
                    GD.rgb, GD.rgba, GD.toRGBA
                   ) where

import           Graphics.GD.Internal     (Point,Color,Image,GDImage,CFILE,Size)
import qualified Graphics.GD.Internal     as GD
 
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI

import           Control.Monad            (liftM,unless)
import           Foreign                  (Ptr)
import qualified Foreign                  as F
import           Foreign                  (peek)
import           Foreign.C                (CInt)
import           Foreign.C                (peekCAString,peekCAString)

--
-- * Loading images
--

-- | Load a JPEG image from a file.
loadJpegFile :: FilePath -> IO Image
loadJpegFile = loadImageFile GD.gdImageCreateFromJpeg

-- | Load a JPEG image from a buffer.
loadJpegData :: Int   -- ^ Buffer size.
             -> Ptr a -- ^ Buffer with image data.
             -> IO Image
loadJpegData = loadImageData GD.gdImageCreateFromJpegPtr

-- | Load a JPEG image from a ByteString
loadJpegByteString :: B.ByteString -> IO Image
loadJpegByteString = onByteStringData loadJpegData


-- | Load a PNG image from a file.
loadPngFile :: FilePath -> IO Image
loadPngFile = loadImageFile GD.gdImageCreateFromPng

-- | Load a PNG image from a buffer.
loadPngData :: Int   -- ^ Buffer size.
            -> Ptr a -- ^ Buffer with image data.
            -> IO Image
loadPngData = loadImageData GD.gdImageCreateFromPngPtr

-- | Load a PNG image from a ByteString
loadPngByteString :: B.ByteString -> IO Image
loadPngByteString = onByteStringData loadPngData

-- | Load a GIF image from a file.
loadGifFile :: FilePath -> IO Image
loadGifFile = loadImageFile GD.gdImageCreateFromGif

-- | Load a GIF image from a buffer.
loadGifData :: Int   -- ^ Buffer size.
            -> Ptr a -- ^ Buffer with image data.
            -> IO Image
loadGifData = loadImageData GD.gdImageCreateFromGifPtr

-- | Load a GIF image from a ByteString
loadGifByteString :: B.ByteString -> IO Image
loadGifByteString = onByteStringData loadGifData


loadImageFile :: (Ptr CFILE -> IO (Ptr GDImage)) -> FilePath -> IO Image
loadImageFile f file = do
  p <- F.throwIfNull ("Loading image from " ++ file) $ GD.withCFILE file "rb" f
  GD.mkImage p

loadImageData :: (CInt -> Ptr a -> IO (Ptr GDImage)) -> Int -> Ptr a -> IO Image
loadImageData f sz buf =
    do p <- F.throwIfNull ("Loading image") $ f (fromIntegral sz) buf
       GD.mkImage p

onByteStringData :: (Int -> Ptr a -> IO b) -> B.ByteString -> IO b
onByteStringData f bstr 
    = case BI.toForeignPtr bstr of
        (fptr, start, sz) -> F.withForeignPtr fptr $
                               \ptr -> f sz (F.plusPtr ptr start)

--
-- * Saving images
--

-- | Save an image as a JPEG file.
saveJpegFile :: Int -- ^ quality: 0-95, or negative for default quality.
             -> FilePath -> Image -> IO ()
saveJpegFile q = saveImageFile (\p h -> GD.gdImageJpeg p h (fromIntegral q))

-- | Write a JPEG format ByteString of an image.
saveJpegByteString :: Int -> Image -> IO B.ByteString
saveJpegByteString q =
  saveImageByteString (\p h -> GD.gdImageJpegPtr p h (fromIntegral q))


-- | Save an image as a PNG file.
savePngFile :: FilePath -> Image -> IO ()
savePngFile = saveImageFile GD.gdImagePng

-- | Write a PNG format ByteString of an image.
savePngByteString :: Image -> IO B.ByteString
savePngByteString = saveImageByteString GD.gdImagePngPtr


-- | Save an image as a GIF file.
saveGifFile :: FilePath -> Image -> IO ()
saveGifFile = saveImageFile GD.gdImageGif

-- | Write a GIF format ByteString of an image.
saveGifByteString :: Image -> IO B.ByteString
saveGifByteString = saveImageByteString GD.gdImageGifPtr

saveImageFile :: (Ptr GDImage -> Ptr CFILE -> IO ()) -> FilePath -> Image
                 -> IO ()
saveImageFile f file i = GD.withImagePtr i (\p -> GD.withCFILE file "wb" (f p))

saveImageByteString :: (Ptr GDImage -> Ptr CInt -> IO (Ptr a)) -> Image ->
                       IO (B.ByteString)
saveImageByteString f img = GD.withImagePtr img (\p -> dataByteString (f p))

dataByteString :: (Ptr CInt -> IO (Ptr a)) -> IO B.ByteString
dataByteString f = F.alloca $ \szPtr -> do
  datPtr <- f szPtr >>= F.newForeignPtr GD.gdFree . F.castPtr
  liftM (BI.fromForeignPtr datPtr 0 . fromIntegral) (peek szPtr)

--
-- * Text
--

-- | Draw a string using the FreeType 2.x library
drawString :: B.ByteString -- ^ Font name
           -> Double       -- ^ Font point size
           -> Double       -- ^ Angle in counterclockwise radians
           -> Point        -- ^ Origin
           -> B.ByteString -- ^ Text, including HTML entities
           -> Color -> Image
           -> IO (Point, Point, Point, Point) -- ^ Bounding box
                                              -- of the drawn
                                              -- text
drawString fontName ptSize angle (oriX, oriY) txt color img =
  GD.withImagePtr img $ drawStringImagePtr color fontName ptSize angle 
                           (oriX, oriY) txt

-- | Measure a string using the FreeType 2.x library.  This computes
-- the bounding box but does not actually draw the string to any
-- image.
measureString :: B.ByteString -- ^ Font name
              -> Double       -- ^ Font point size
              -> Double       -- ^ Angle in counterclockwise radians
              -> Point        -- ^ Origin
              -> B.ByteString -- ^ Text, including HTML entities
              -> Color
              -> IO (Point, Point, Point, Point) -- ^ Bounding
                                                 -- box of the
                                                 -- drawn text
measureString fontName ptSize angle (oriX, oriY) txt color
    = drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt F.nullPtr

drawStringImagePtr :: Color -> B.ByteString -> Double -> Double -> Point
                      -> B.ByteString -> Ptr GDImage
                      -> IO (Point, Point, Point, Point)
drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt imgPtr
    = F.allocaArray 8 $
      \bboxPtr -> B.useAsCString fontName $
      \cFontName -> B.useAsCString txt $
      \cTxt -> do res <- GD.gdImageStringFT imgPtr bboxPtr color cFontName
                           (GD.double ptSize) (GD.double angle)
                           (GD.int oriX) (GD.int oriY) cTxt
                  if res == F.nullPtr
                     then F.peekArray 8 bboxPtr >>= parseBBox
                     else peekCAString res >>= ioError . userError
    where parseBBox l =
            case map GD.int l of
              [llx, lly, lrx, lry, urx, ury, ulx, uly] ->
                return ((llx, lly), (lrx, lry), (urx, ury), (ulx, uly))
              _ -> ioError $ userError $
                     "parseBBox with /= 8 elements: " ++ show l

-- | Draw strings around the top and bottom of a torus
drawStringCircle :: Point        -- ^ Center of text path circle
                 -> Double       -- ^ Outer radius of text
                 -> Double       -- ^ Fraction of radius occupied by text
                 -> Double       -- ^ Portion of circle arc filled by text
                 -> B.ByteString -- ^ Font name
                 -> Double       -- ^ Font size hint
                 -> B.ByteString -- ^ Text to write on the top of the circle
                 -> B.ByteString -- ^ Text to write on the bottom of the circle
                 -> Color        -- ^ Text color
                 -> Image -> IO ()
drawStringCircle (ctrX, ctrY) rad textRad textFill fontName fontSize topTxt
                 bottomTxt color img
    = B.useAsCString fontName $ 
      \cFontName -> B.useAsCString topTxt $
      \cTopTxt -> B.useAsCString bottomTxt $ 
      \cBottomTxt -> GD.withImagePtr img $ 
      \imgPtr -> do
        res <- GD.gdImageStringFTCircle imgPtr (GD.int ctrX) (GD.int ctrY)
                                        (GD.double rad) (GD.double textRad)
                                        (GD.double textFill) cFontName
                                        (GD.double fontSize) cTopTxt
                                        cBottomTxt  color                    
        unless (res == F.nullPtr) (peekCAString res >>= ioError . userError)
