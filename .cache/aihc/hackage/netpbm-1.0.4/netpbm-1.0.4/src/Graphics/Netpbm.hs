{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}


-- | Parsing the netpbm image formates (PBM, PGM and PPM, both ASCII and binary) from 'ByteString's.
--
-- All netpbm image formats are implemented (P1 - P6).
--
-- To parse one of these formats, use `parsePPM`.
--
-- See also: <http://www.imagemagick.org/Usage/formats/#netpbm>
module Graphics.Netpbm (
  PPMType (..)
, PPM (..)
, PpmPixelRGB8 (..)
, PpmPixelRGB16 (..)
, PbmPixel (..)
, PgmPixel8 (..)
, PgmPixel16 (..)
, PPMHeader (..)
, PpmPixelData (..)
, pixelVectorToList
, pixelDataToIntList
, parsePPM
, PpmParseResult
-- TODO expose attoparsec functions in .Internal package
) where

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 as A8
import           Data.Attoparsec.Binary (anyWord16be)
import           Data.Bits (testBit)
import           Data.ByteString (ByteString)
import           Data.Char (chr, ord)
import           Data.List (foldl')
import           Data.Word (Word8, Word16)
import           Foreign.Storable.Record as Store
import           Foreign.Storable (Storable (..))

import qualified Data.Vector.Storable as S
import           Data.Vector.Storable ((!))
import qualified Data.Vector.Storable.Mutable as SM

import Data.Vector.Unboxed.Deriving


-- | The netpbm image type of an image.
data PPMType = P1 -- ^ ASCII bitmap
             | P2 -- ^ ASCII greymap
             | P3 -- ^ ASCII pixmap (color)
             | P4 -- ^ binary bitmap
             | P5 -- ^ binary greymap
             | P6 -- ^ binary pixmap (color)
             deriving (Eq, Show, Enum, Ord)


-- | A PPM file with type, dimensions, and image data.
data PPM = PPM {
  ppmHeader :: PPMHeader
, ppmData   :: PpmPixelData
}

-- | Meta information about the image: The exact PPM format and dimensions.
data PPMHeader = PPMHeader {
  ppmType   :: !PPMType
, ppmWidth  :: !Int
, ppmHeight :: !Int
} deriving (Eq, Show)

instance Show PPM where
  show PPM { ppmHeader = PPMHeader { ppmType, ppmWidth, ppmHeight } } = "PPM " ++ show ppmType ++ " image " ++ dim
    where
      dim = show (ppmWidth, ppmHeight)


-- | A pixel containing three 8-bit color components, RGB.
data PpmPixelRGB8 = PpmPixelRGB8 {-# UNPACK #-} !Word8 -- Red
                                 {-# UNPACK #-} !Word8 -- Green
                                 {-# UNPACK #-} !Word8 -- Blue
                                 deriving (Eq, Show)

-- | A pixel containing three 16-bit color components, RGB.
data PpmPixelRGB16 = PpmPixelRGB16 {-# UNPACK #-} !Word16 -- Red
                                   {-# UNPACK #-} !Word16 -- Green
                                   {-# UNPACK #-} !Word16 -- Blue
                                   deriving (Eq, Show)

-- | A pixel containing black or white.
newtype PbmPixel = PbmPixel Bool -- False = black, True = white
                 deriving (Eq, Show)

-- | A pixel containing an 8-bit greyscale value.
data PgmPixel8 = PgmPixel8 {-# UNPACK #-} !Word8
                           deriving (Eq, Show)

-- | A pixel containing a 16-bit greyscale value.
data PgmPixel16 = PgmPixel16 {-# UNPACK #-} !Word16
                             deriving (Eq, Show)

-- | Image data, either 8 or 16 bits.
-- TODO rename to PNM
data PpmPixelData = PpmPixelDataRGB8 (S.Vector PpmPixelRGB8)   -- ^ For 8-bit PPMs.
                  | PpmPixelDataRGB16 (S.Vector PpmPixelRGB16) -- ^ For 16-bit PPMs.
                  | PbmPixelData (S.Vector PbmPixel)           -- ^ For 1-bit PBMs.
                  | PgmPixelData8 (S.Vector PgmPixel8)         -- ^ For 8-bit PGMs.
                  | PgmPixelData16 (S.Vector PgmPixel16)       -- ^ For 16-bit PGMs.


-- | Converts a vector of pixels to a list for convenience.
pixelVectorToList :: (Storable a) => S.Vector a -> [a]
pixelVectorToList = S.toList


-- | Converts pixel data to a list of positive `Int`s.
--
-- How big they can become depends on the bit depth of the pixel data.
pixelDataToIntList :: PpmPixelData -> [Int]
pixelDataToIntList d = case d of
  PpmPixelDataRGB8 v  -> concat [ map fromIntegral [r, g, b] | PpmPixelRGB8 r g b  <- S.toList v ]
  PpmPixelDataRGB16 v -> concat [ map fromIntegral [r, g, b] | PpmPixelRGB16 r g b <- S.toList v ]
  PbmPixelData v      ->        [ if b then 1 else 0         | PbmPixel b          <- S.toList v ]
  PgmPixelData8 v     ->        [ fromIntegral x             | PgmPixel8 x         <- S.toList v ]
  PgmPixelData16 v    ->        [ fromIntegral x             | PgmPixel16 x        <- S.toList v ]


-- * Storable instance for pixels

storePpmPixel8 :: Store.Dictionary PpmPixelRGB8
storePpmPixel8 =
  Store.run $ liftA3 PpmPixelRGB8
    (Store.element (\(PpmPixelRGB8 x _ _) -> x))
    (Store.element (\(PpmPixelRGB8 _ y _) -> y))
    (Store.element (\(PpmPixelRGB8 _ _ z) -> z))

storePpmPixel16 :: Store.Dictionary PpmPixelRGB16
storePpmPixel16 =
  Store.run $ liftA3 PpmPixelRGB16
    (Store.element (\(PpmPixelRGB16 x _ _) -> x))
    (Store.element (\(PpmPixelRGB16 _ y _) -> y))
    (Store.element (\(PpmPixelRGB16 _ _ z) -> z))

storePbmPixel :: Store.Dictionary PbmPixel
storePbmPixel =
  Store.run $ liftA PbmPixel
    (Store.element (\(PbmPixel x) -> x))

storePgmPixel8 :: Store.Dictionary PgmPixel8
storePgmPixel8 =
  Store.run $ liftA PgmPixel8
    (Store.element (\(PgmPixel8 x) -> x))

storePgmPixel16 :: Store.Dictionary PgmPixel16
storePgmPixel16 =
  Store.run $ liftA PgmPixel16
    (Store.element (\(PgmPixel16 x) -> x))

instance Storable PpmPixelRGB8 where
  sizeOf = Store.sizeOf storePpmPixel8
  alignment = Store.alignment storePpmPixel8
  peek = Store.peek storePpmPixel8
  poke = Store.poke storePpmPixel8

instance Storable PpmPixelRGB16 where
  sizeOf = Store.sizeOf storePpmPixel16
  alignment = Store.alignment storePpmPixel16
  peek = Store.peek storePpmPixel16
  poke = Store.poke storePpmPixel16

instance Storable PbmPixel where
  sizeOf = Store.sizeOf storePbmPixel
  alignment = Store.alignment storePbmPixel
  peek = Store.peek storePbmPixel
  poke = Store.poke storePbmPixel

instance Storable PgmPixel8 where
  sizeOf = Store.sizeOf storePgmPixel8
  alignment = Store.alignment storePgmPixel8
  peek = Store.peek storePgmPixel8
  poke = Store.poke storePgmPixel8

instance Storable PgmPixel16 where
  sizeOf = Store.sizeOf storePgmPixel16
  alignment = Store.alignment storePgmPixel16
  peek = Store.peek storePgmPixel16
  poke = Store.poke storePgmPixel16


-- | Parses a netpbm magic number.
-- One of P1, P2, P3, P4, P5, P6.
magicNumberParser :: Parser PPMType
magicNumberParser = do
  magic <- choice ["P1", "P2", "P3", "P4", "P5", "P6"]
  case magic of
    "P1" -> return P1
    "P2" -> return P2
    "P3" -> return P3
    "P4" -> return P4
    "P5" -> return P5
    "P6" -> return P6
    _    -> fail $ "PPM: uknown PPM format " ++ show magic



-- Not writing this as @comments = skipMany comment@ because that would allow this parser
-- to consume no input, which makes it loop forever when stuck into something like `many`.
{-# INLINE comment #-}
comment :: Parser ByteString
comment = "#" *> A.takeWhile isNotNewline <* endOfLine
  where
    isNotNewline w = w /= 10 && w /= 13


{-# INLINE sep #-}
sep :: Parser ()
-- At least one space, optionally with more space or comments around
sep = do skipMany comment
         singleWhitespace
         skipMany (singleWhitespace <|> void comment)


-- | Decimal, possibly with comments interleaved,
-- but starting and ending with a digit.
-- See the notes about comments.
{-# INLINE decimalC #-}
decimalC :: Parser Int
decimalC = foldl' shiftDecimalChar 0 <$> (digit `sepBy1` skipMany comment)
  where
    shiftDecimalChar a d = a * 10 + ord d - (48 :: Int)


headerParser :: Parser PPMHeader
headerParser = do
  ppmType <- magicNumberParser
  sep
  width <- decimalC
  sep
  height <- decimalC
  skipMany comment -- Don't allow whitespace here since after the next whitespace there must not be any more comments
  return $ PPMHeader ppmType width height


{-# INLINE word8max #-}
-- Parsing words not bigger than given maxval
word8max :: Word8 -> Parser Word8
word8max m = A.satisfy (<= m) <?> "pixel data must be smaller than maxval"

{-# INLINE word16max #-}
word16max :: Word16 -> Parser Word16
word16max m = do w16 <- anyWord16be
                 when (not $ w16 <= m) $ fail "pixel data must be smaller than maxval"
                 return w16


{-# INLINE isValidMaxval #-}
isValidMaxval :: Int -> Bool
isValidMaxval v = v > 0 && v < 65536


{-# INLINE singleWhitespace #-}
singleWhitespace :: Parser ()
singleWhitespace = void $ A.satisfy isSpace_w8


-- | Parses a SINGLE PPM file.
--
-- Specification: http://netpbm.sourceforge.net/doc/ppm.html
--
-- There can be multiple images in one file, each starting with
-- a "Pn" magic number.
--
-- Comments starting with '#' can only be
-- "before the whitespace character that delimits the raster"
-- (see http://netpbm.sourceforge.net/doc/pbm.html).
-- Nevertheless, I interpret that as "comments cannot be
-- inside the magic number".
--
-- See also the notes for `imagesParser`.
ppmBodyParser :: PPMHeader -> Parser PPM
ppmBodyParser header@PPMHeader { ppmWidth = width, ppmHeight = height } = do

  sep

  maxColorVal <- decimalC
  when (not $ isValidMaxval maxColorVal) $
    fail $ "PPM: invalid color maxval " ++ show maxColorVal
  skipMany comment

  singleWhitespace -- obligatory SINGLE whitespace; starting from here, comments are not allowed any more

  raster <- case maxColorVal of -- 1 or 2 bytes per pixel
    -- Parse pixel data into vector, making sure that words don't exceed maxColorVal
    m | m < 256   -> let v = word8max (fromIntegral m)
                      in PpmPixelDataRGB8  <$> S.replicateM (height * width) (PpmPixelRGB8  <$> v <*> v <*> v)
    m | otherwise -> let v = word16max (fromIntegral m)
                      in PpmPixelDataRGB16 <$> S.replicateM (height * width) (PpmPixelRGB16 <$> v <*> v <*> v)

  return $ PPM header raster


pgmBodyParser :: PPMHeader -> Parser PPM
pgmBodyParser header@PPMHeader { ppmWidth = width, ppmHeight = height } = do

  sep

  maxGreyVal <- decimalC
  when (not $ isValidMaxval maxGreyVal) $
    fail $ "PGM: invalid grey maxval " ++ show maxGreyVal
  skipMany comment

  singleWhitespace -- obligatory SINGLE whitespace; starting from here, comments are not allowed any more

  raster <- case maxGreyVal of -- 1 or 2 bytes per pixel
    -- Parse pixel data into vector, making sure that words don't exceed maxGreyVal
    m | m < 256   -> let v = word8max (fromIntegral m)
                      in PgmPixelData8  <$> S.replicateM (height * width) (PgmPixel8  <$> v)
    m | otherwise -> let v = word16max (fromIntegral m)
                      in PgmPixelData16 <$> S.replicateM (height * width) (PgmPixel16 <$> v)

  return $ PPM header raster


pbmBodyParser :: PPMHeader -> Parser PPM
pbmBodyParser header@PPMHeader { ppmWidth = width, ppmHeight = height } = do

  singleWhitespace -- obligatory SINGLE whitespace; starting from here, comments are not allowed any more

  -- From: http://netpbm.sourceforge.net/doc/pbm.html
  --   "Each row is Width bits, packed 8 to a byte, with don't care bits to fill out the last byte in the row."
  let widthBytes = (width + 7) // 8

  -- Parse pixel data first in into a Word8 vector, then translate to a Bool vector, leaving the don't-cares at the end out.
  word8Vector <- S.replicateM (height * widthBytes) anyWord8

  let bits = S.create $ do
        v <- SM.replicate (width * height) (PbmPixel False)
        forM_ [0..height-1] $ \row ->
          forM_ [0..width-1] $ \col ->
            let i            = row * width + col
                (col8, bitN) = col /% 8
                i8           = row * widthBytes + col8
             -- We negate (see "not"), because:
             --   "1 is black, 0 is white."
             -- Also, `testBit` indexes from the right (LSB).
             in SM.write v i (PbmPixel . not $ (word8Vector ! i8) `testBit` (7 - bitN))
        return v

  return $ PPM header (PbmPixelData bits)
  where
    (//) = quot
    (/%) = quotRem


-- | See http://netpbm.sourceforge.net/doc/pbm.html
--
-- We ignore the "No line should be longer than 70 characters" here due to "should".
pbmAsciiBodyParser :: PPMHeader -> Parser PPM
pbmAsciiBodyParser header@PPMHeader { ppmWidth = width, ppmHeight = height } = do

  singleWhitespace -- obligatory SINGLE whitespace; starting from here, comments are not allowed any more

  -- Parse pixel data into Bool vector.
  let n = height * width
  -- There must be whitespace *between* the values.
  -- There can be whitespace *before* the first value since:
  --   "White space in the raster section is ignored."
  -- Don't allow it *after* so that we can check if there is a whitespace between raster and optional junk.
  -- I use `generateM` here instead of fromList . (`sepBy` [whitespace]) because I believe it's faster.
  bits <- S.replicateM n (A.takeWhile isSpace_w8 *> asciiBit)

  -- From the spec (who the heck can even come up with this):
  --   "You can put any junk you want after the raster, if it starts with a white space character."
  -- Note that it says *can*, i.e. the junk can also be empty, so trailing whitespace is allowed.
  -- So let's eat all remaining input:
  option () (A.takeWhile1 isSpace_w8 *> takeLazyByteString *> pure ())

  -- Now we should be at the end of file.
  endOfInput <?> "there is junk after the ASCII raster that is not separated by whitespace"

  return $ PPM header (PbmPixelData bits)
  where
    asciiBit = PbmPixel <$> (anyWord8 >>= toBool)
    -- We flip True/False because "1" means black == False.
    toBool 48 = return True
    toBool 49 = return False
    toBool w  = fail $ "ASCII bit must be '0' or '1', not " ++ show (chr $ fromIntegral w)


pgmAsciiBodyParser :: PPMHeader -> Parser PPM
pgmAsciiBodyParser header@PPMHeader { ppmWidth = width, ppmHeight = height } = do

  sep

  maxGreyVal <- decimalC
  when (not $ isValidMaxval maxGreyVal) $
    fail $ "PGM: invalid grey maxval " ++ show maxGreyVal
  skipMany comment

  singleWhitespace -- obligatory SINGLE whitespace; starting from here, comments are not allowed any more

  let n = height * width

  -- TODO size-check the int by first putting it in Word64 and limiting decimal length
  raster <- case maxGreyVal of -- 1 or 2 bytes per pixel
    -- Parse pixel data into vector, making sure that words don't exceed maxGreyVal
    m | m < 256 -> PgmPixelData8  <$> S.replicateM n (A.takeWhile isSpace_w8 *> (PgmPixel8  <$> decimal))
    _           -> PgmPixelData16 <$> S.replicateM n (A.takeWhile isSpace_w8 *> (PgmPixel16 <$> decimal))

  option () (A.takeWhile1 isSpace_w8 *> takeLazyByteString *> pure ())

  -- Now we should be at the end of file.
  endOfInput <?> "there is junk after the ASCII raster that is not separated by whitespace"

  return $ PPM header raster


ppmAsciiBodyParser :: PPMHeader -> Parser PPM
ppmAsciiBodyParser header@PPMHeader { ppmWidth = width, ppmHeight = height } = do

  sep

  maxColorVal <- decimalC
  when (not $ isValidMaxval maxColorVal) $
    fail $ "PGM: invalid color maxval " ++ show maxColorVal
  skipMany comment

  singleWhitespace -- obligatory SINGLE whitespace; starting from here, comments are not allowed any more

  let n = height * width
      d8  = A.takeWhile isSpace_w8 *> decimal :: Parser Word8
      d16 = A.takeWhile isSpace_w8 *> decimal :: Parser Word16

  -- TODO size-check the int by first putting it in Word64 and limiting decimal length
  raster <- case maxColorVal of -- 1 or 2 bytes per pixel
    -- Parse pixel data into vector, making sure that words don't exceed maxColorVal
    m | m < 256 -> PpmPixelDataRGB8  <$> S.replicateM n (PpmPixelRGB8  <$> d8  <*> d8  <*> d8 )
    _           -> PpmPixelDataRGB16 <$> S.replicateM n (PpmPixelRGB16 <$> d16 <*> d16 <*> d16)

  option () (A.takeWhile1 isSpace_w8 *> takeLazyByteString *> pure ())

  -- Now we should be at the end of file.
  endOfInput <?> "there is junk after the ASCII raster that is not separated by whitespace"

  return $ PPM header raster


imageParserOfType :: Maybe PPMType -> Parser PPM
imageParserOfType mpN = do
  header@PPMHeader { ppmType } <- headerParser

  case mpN of
    Just pN | pN /= ppmType -> fail "an image in a multi-image file is not of the same type as the first image in the file"
    _                       -> return ()

  case ppmType of
    P1 -> pbmAsciiBodyParser header
    P2 -> pgmAsciiBodyParser header
    P3 -> ppmAsciiBodyParser header
    P4 -> pbmBodyParser header
    P5 -> pgmBodyParser header
    P6 -> ppmBodyParser header


imageParser :: Parser PPM
imageParser = imageParserOfType Nothing


-- | Parses a full PPM file, containing one or more images.
--
-- From the spec:
--
-- >"A PPM file consists of a sequence of one or more PPM images.
-- > There are no data, delimiters, or padding before, after, or between images."
--
-- However, you can find PPM files that have trailing whitespace, especially a '\n',
-- so we allow this.
imagesParser :: Parser [PPM]
imagesParser = do
  -- Parse the first image.
  firstImage@PPM { ppmHeader = PPMHeader { ppmType } } <- imageParser <* skipSpace

  -- Force the following images, if any, to be of the same type.
  otherImages <- many (imageParserOfType (Just ppmType) <* skipSpace)

  -- TODO Restructure so that this cannot happen. There is no point of returning [PPM] for ASCII images.
  when (ppmType `elem` [P1, P2, P3] && not (null otherImages)) $
    error "haskell-netpbm bug: ASCII formats should never contain more than one image (they treat remaining data as junk)"

  return $ firstImage:otherImages



-- | The result of a PPM parse.
--
-- See `parsePPM`.
type PpmParseResult = Either String ([PPM], Maybe ByteString)


-- | Parses a PPM file from the given 'ByteString'.
-- On failure, @Left error@ contains the error message.
-- On success, @Right (images, Maybe rest)@ contains the parsed images
-- and potentially an unparsable rest input.
parsePPM :: ByteString -> PpmParseResult
parsePPM bs = case parse imagesParser bs of
  -- The image file ByteStrings are not terminated by '\0',
  -- so Attoparsec will issue a Partial result when it
  -- parses to EOF. Passing in "" signalizes EOF.
  Partial cont -> resultToEither (cont "")
  r            -> resultToEither r
  where
    -- Assumes a Partial result has already been fed with "" (another Partial cannot happen)
    resultToEither r = case r of
      Done ""   images -> Right (images, Nothing)
      Done rest images -> Right (images, Just rest)
      Partial _        -> error "parsePPM bug: Got a partial result after end of input"
      Fail _ cs e      -> Left $ e ++ "; contexts: " ++ show cs


-- * Unbox instance for pixels
--
-- Not used internally, but an Unbox instance might be convenient for users.

derivingUnbox "PpmPixelRGB8"
    [t| PpmPixelRGB8 -> (Word8, Word8, Word8) |]
    [| \ (PpmPixelRGB8 a b c) -> (a, b, c) |]
    [| \ (a, b, c) -> PpmPixelRGB8 a b c |]

derivingUnbox "PpmPixelRGB16"
    [t| PpmPixelRGB16 -> (Word16, Word16, Word16) |]
    [| \ (PpmPixelRGB16 a b c) -> (a, b, c) |]
    [| \ (a, b, c) -> PpmPixelRGB16 a b c |]

derivingUnbox "PbmPixel"
    [t| PbmPixel -> Bool |]
    [| \ (PbmPixel b) -> b |]
    [| \ b -> PbmPixel b |]

derivingUnbox "PgmPixel8"
    [t| PgmPixel8 -> Word8 |]
    [| \ (PgmPixel8 x) -> x |]
    [| \ x -> PgmPixel8 x |]

derivingUnbox "PgmPixel16"
    [t| PgmPixel16 -> Word16 |]
    [| \ (PgmPixel16 x) -> x |]
    [| \ x -> PgmPixel16 x |]
