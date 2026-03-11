{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- avoid a flood of warnings
{-|

Module      :  Codec.Image.PBM
Copyright   :  (c) Claude Heiland-Allen 2012,2018
License     :  BSD3

Maintainer  :  claude@mathr.co.uk
Stability   :  unstable
Portability :  portable

Encode and decode both versions (binary P4 and plain P1) of PBM: the
portable bitmap lowest common denominator monochrome image file format.

References:

  * pbm(5)

  * The PBM Format <http://netpbm.sourceforge.net/doc/pbm.html>

Bugs:

  * This implementation is not fully compliant with the PBM specification,
    with respect to point 8 in the second reference above which states that
    /a comment can actually be in the middle of what you might consider a token/
    Such a pathological PBM file might be rejected by 'decodePBM', but
    may instead be wrongly decoded if (for example) the comment were in
    the middle of the image width token, leading to it being interpreted
    as a (smaller) width and height.

-}
module Codec.Image.PBM
  ( PBM(..)
  -- * Encoding PBM images.
  , encodePBM
  , encodePlainPBM
  , EncodeError(..)
  , encodePBM'
  -- * Decoding PBM images.
  , DecodeError(..)
  , decodePBM
  , decodePlainPBM
  , decodePBMs
  -- * Padding and trimming PBM images.
  , padPBM
  , trimPBM
  , repadPBM
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Ix (range)
import Data.Word (Word8)
import qualified Data.Array.Unboxed as U
import qualified Data.ByteString as BS

import Data.Bits.Bitwise (fromListBE, toListLE)
import Data.Array.BitArray (BitArray, bounds, elems, listArray, false, (//), assocs, ixmap)
import Data.Array.BitArray.ByteString (toByteString, fromByteString)

-- | A decoded PBM image.  'pbmWidth' must be less or equal to the
--   width of the 'pbmPixels' array (which has its first index in Y
--   and the second in X, with lowest coordinates at the top left).
--
--   False pixels are white, True pixels are black.  Pixels to the
--   right of 'pbmWidth' are don't care padding bits.  However, these
--   padding bits are likely to invalidate aggregrate 'BitArray.fold'
--   operations.  See 'trimPBM'.
--
data PBM = PBM{ pbmWidth :: !Int, pbmPixels :: !(BitArray (Int, Int)) }

-- | Encode a binary PBM (P4) image, padding rows to multiples of 8
--   bits as necessary.
--
encodePBM :: BitArray (Int, Int) {- ^ pixels -} -> BS.ByteString
encodePBM pixels = case encodePBM' pbm of
  Right string -> string
  _ -> error "Codec.Image.PBM.encodePBM: internal error"
  where
    ((_, xlo), (_, xhi)) = bounds pixels
    width  = xhi - xlo + 1
    pbm = padPBM PBM{ pbmWidth = width, pbmPixels = pixels }

-- | Possible reasons for encoding to fail.
data EncodeError
  = BadPixelWidth{ encErrPBM :: PBM } -- ^ array width is not a multiple of 8 bits
  | BadSmallWidth{ encErrPBM :: PBM } -- ^ image width is too smaller than array width
  | BadLargeWidth{ encErrPBM :: PBM } -- ^ image width is larger than array width

-- | Encode a plain PBM (P1) image.
--
--   No restrictions on pixels array size, but the file format is
--   exceedingly wasteful of space.
--
encodePlainPBM :: BitArray (Int, Int) {- ^ pixels -} -> String
encodePlainPBM pixels = unlines (header : raster)
  where
    ((ylo, xlo), (yhi, xhi)) = bounds pixels
    width  = xhi - xlo + 1
    height = yhi - ylo + 1
    header = "P1\n" ++ show width ++ " " ++ show height
    raster = concatMap (chunk 64) . chunk width . map char . elems $ pixels
    char False = '0'
    char True  = '1'
    chunk n _ | n <= 0 = error "Codec.Image.PBM.encodePlainPBM: internal error"
    chunk _ [] = []
    chunk n xs = let (ys, zs) = splitAt n xs in ys : chunk n zs

-- | Encode a pre-padded 'PBM' to a binary PBM (P4) image.
--
--   The pixels array must have a multiple of 8 bits per row.  The image
--   width may be less than the pixel array width, with up to 7 padding
--   bits at the end of each row.
--
encodePBM' :: PBM -> Either EncodeError BS.ByteString
encodePBM' pbm
  | (pixelWidth .&. 7) /= 0 = Left (BadPixelWidth pbm)
  | width <= pixelWidth - 8 = Left (BadSmallWidth pbm)
  | width >  pixelWidth     = Left (BadLargeWidth pbm)
  | otherwise = Right (header `BS.append` raster)
  where
    width = pbmWidth pbm
    pixels = pbmPixels pbm
    ((ylo, xlo), (yhi, xhi)) = bounds pixels
    pixelWidth  = xhi - xlo + 1
    pixelHeight = yhi - ylo + 1
    height = pixelHeight
    header = BS.pack $ map (toEnum . fromEnum) headerStr
    headerStr = "P4\n" ++ show width ++ " " ++ show height ++ "\n"
    raster = reverseByteBits (toByteString pixels)

-- | Possible reasons for decoding to fail, with the input that failed.
data DecodeError a
  = BadMagicP a -- ^ First character was not P.
  | BadMagicN a -- ^ Second character was not 4 (binary) or 1 (plain).
  | BadWidth  a -- ^ The width could not be parsed, or was non-positive.
  | BadHeight a -- ^ The height could not be parsed, or was non-positive.
  | BadSpace  a -- ^ Parsing failed at the space before the pixel data.
  | BadPixels a -- ^ There weren't enough bytes of pixel data.
  deriving (Eq, Ord, Read, Show)

-- | Decode a binary PBM (P4) image.
decodePBM :: BS.ByteString -> Either (DecodeError BS.ByteString) (PBM, BS.ByteString)
decodePBM s =                    case BS.uncons s of
  Just (cP, s) | cP == char 'P' -> case BS.uncons s of
    Just (c4, s) | c4 == char '4' -> case int (skipSpaceComment s) of
      Just (iw, s) | iw > 0         -> case int (skipSpaceComment s) of
        Just (ih, s) | ih > 0         -> case skipSingleSpace s of
          Just s                        ->
            let rowBytes = (iw + 7) `shiftR` 3
                imgBytes = ih * rowBytes
            in                             case BS.splitAt imgBytes s of
            (raster, s) | BS.length raster == imgBytes ->
              let ibs = ((0, 0), (ih - 1, (rowBytes `shiftL` 3) - 1))
              in  Right (PBM{ pbmWidth = iw, pbmPixels = fromByteString ibs (reverseByteBits raster) }, s)
            _ -> Left (BadPixels s)
          _ -> Left (BadSpace s)
        _ -> Left (BadHeight s)
      _ -> Left (BadWidth s)
    _ -> Left (BadMagicN s)
  _ -> Left (BadMagicP s)
  where
    skipSpaceComment t = case (\t -> (t, BS.uncons t)) (BS.dropWhile isSpace t) of
      (_, Just (cH, t)) | cH == char '#' -> case BS.uncons (BS.dropWhile (/= char '\n') t) of
        Just (cL, t) | cL == char '\n' -> skipSpaceComment t
        _ -> Left (BadSpace t)
      (t, _) -> Right t
    skipSingleSpace t = case BS.uncons t of
      Just (cS, t) | isSpace cS -> Just t
      _ -> Nothing
    int (Left _) = Nothing
    int (Right t) = case BS.span isDigit t of
      (d, t)
        | BS.length d > 0 &&
          fmap ((/= char '0') . fst) (BS.uncons d) == Just True -> case reads (map unchar $ BS.unpack d) of
            [(d, "")] -> Just (d, t)
            _ -> Nothing
      _ -> Nothing
    isSpace c = c `elem` map char pbmSpace
    isDigit c = c `elem` map char "0123456789"
    char = toEnum . fromEnum
    unchar = toEnum . fromEnum

-- | Decode a sequence of binary PBM (P4) images.
--
--   Keeps decoding until end of input (in which case the 'snd' of the
--   result is 'Nothing') or an error occurred.
--
decodePBMs :: BS.ByteString -> ([PBM], Maybe (DecodeError BS.ByteString))
decodePBMs s
  | BS.null s = ([], Nothing)
  | otherwise = case decodePBM s of
      Left err -> ([], Just err)
      Right (pbm, s) -> prepend pbm (decodePBMs s)
  where
    prepend pbm (pbms, merr) = (pbm:pbms, merr)

-- | Decode a plain PBM (P1) image.
--
--   Note that the pixel array size is kept as-is (with the width not
--   necessarily a multiple of 8 bits).
--
decodePlainPBM :: String -> Either (DecodeError String) (PBM, String)
decodePlainPBM s = case s of
  ('P':s) -> case s of
    ('1':s) -> case int (skipSpaceComment s) of
      Just (iw, s) | iw > 0 -> case int (skipSpaceComment s) of
        Just (ih, s) | ih > 0 -> case collapseRaster (iw * ih) s of
          Just (raster, s) ->
            let ibs = ((0, 0), (ih - 1, iw - 1))
            in  Right (PBM{ pbmWidth = iw, pbmPixels = listArray ibs raster }, s)
          _ -> Left (BadPixels s)
        _ -> Left (BadHeight s)
      _ -> Left (BadWidth s)
    _ -> Left (BadMagicN s)
  _ -> Left (BadMagicP s)
  where
    skipSpaceComment t = case dropWhile isSpace t of
      ('#':t) -> case dropWhile (/= '\n') t of
        ('\n':t) -> skipSpaceComment t
        _ -> Left (BadSpace t)
      t -> Right t
    int (Left _) = Nothing
    int (Right t) = case span isDigit t of
      (d@(d0:_), t) | d0 /= '0' -> case reads d of
        [(d, "")] -> Just (d, t)
        _ -> Nothing
      _ -> Nothing
    collapseRaster 0 t = Just ([], t)
    collapseRaster n t = case dropWhile isSpace t of
      ('0':t) -> prepend False (collapseRaster (n - 1) t)
      ('1':t) -> prepend True  (collapseRaster (n - 1) t)
      _ -> Nothing
    prepend _ Nothing = Nothing
    prepend b (Just (bs, t)) = Just (b:bs, t)
    isSpace c = c `elem` pbmSpace
    isDigit c = c `elem` "0123456789"

-- | Add padding bits at the end of each row to make the array width a
--   multiple of 8 bits, required for binary PBM (P4) encoding.
--
padPBM :: PBM -> PBM
padPBM pbm
  | (pixelWidth .&. 7) == 0 = pbm
  | otherwise = pbm{ pbmPixels = false paddedBounds // assocs (pbmPixels pbm) }
  where
    ((ylo, xlo), (yhi, xhi)) = bounds (pbmPixels pbm)
    pixelWidth = xhi - xlo + 1
    rowBytes = (pixelWidth + 7) `shiftR` 3
    paddedWidth = rowBytes `shiftL` 3
    paddedBounds = ((ylo, xlo), (yhi, xhi'))
    xhi' = paddedWidth + xlo - 1

-- | Trim any padding bits, required for 'fold' operations to give
--   meaningful results.
--
--   Fails for invalid 'PBM' with image width greater than array width.
--
trimPBM :: PBM -> Maybe PBM
trimPBM pbm
  | pbmWidth pbm > pixelWidth = Nothing
  | pbmWidth pbm == pixelWidth = Just pbm
  | otherwise = Just pbm{ pbmPixels = ixmap trimmedBounds id (pbmPixels pbm) }
  where
    ((ylo, xlo), (yhi, xhi)) = bounds (pbmPixels pbm)
    pixelWidth = xhi - xlo + 1
    trimmedBounds = ((ylo, xlo), (yhi, xhi'))
    xhi' = pbmWidth pbm + xlo - 1

-- | Trim then pad.  The resulting 'PBM' (if any) is suitable for
--   encoding to binary PBM (P4), moreover its padding bits will
--   be cleared.
repadPBM :: PBM -> Maybe PBM
repadPBM pbm = padPBM `fmap` trimPBM pbm

-- | Reverse the bit order of all bytes.
--
--   PBM specifies that the most significant bit is leftmost, which is
--   opposite to the convention used by BitArray.
--
reverseByteBits :: BS.ByteString -> BS.ByteString
reverseByteBits = BS.map reverseBits

-- | Fast reversal of the bit order of a byte using a lookup table.
reverseBits :: Word8 -> Word8
reverseBits w = bitReversed U.! w

-- | A lookup table for bit order reversal.
bitReversed :: U.UArray Word8 Word8
bitReversed = U.listArray bs [ bitReverse w | w <- range bs ]
  where bs = (minBound, maxBound)

-- | A slow way to reverse bit order.
bitReverse :: Word8 -> Word8
bitReverse = fromListBE . toListLE

-- | White space characters as defined by the PBM specification.
pbmSpace :: String
pbmSpace = " \t\n\v\f\r"
