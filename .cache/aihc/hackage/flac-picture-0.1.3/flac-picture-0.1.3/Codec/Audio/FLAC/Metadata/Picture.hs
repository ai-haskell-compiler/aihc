{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Picture
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Juicy-Pixels-powered helpers to read\/write images into FLAC metadata
-- blocks. For best player support, use PNG or JPEG (we don't provide
-- helpers for other formats anyway).
module Codec.Audio.FLAC.Metadata.Picture
  ( retrieveImage,
    writeJpegPicture,
    writePngPicture,
  )
where

import Codec.Audio.FLAC.Metadata
import Codec.Picture
import Data.ByteString.Lazy qualified as BL
import Data.Word

-- | Read a picture of 'PictureType' from FLAC metadata as a 'DynamicImage'.
retrieveImage ::
  PictureType ->
  FlacMeta (Either String DynamicImage)
retrieveImage pictureType = do
  mpicture <- retrieve (Picture pictureType)
  case mpicture of
    Nothing -> return (Left "Picture not found")
    Just picture -> (return . decodeImage . pictureData) picture

-- | Write the given image into FLAC metadata block corresponding to a
-- specific 'PictureType'.
writeJpegPicture ::
  -- | Type of picture we're writing
  PictureType ->
  -- | Quality factor, see 'encodeJpegAtQuality'
  Word8 ->
  -- | The picture to write
  Image PixelYCbCr8 ->
  FlacMeta ()
writeJpegPicture pictureType q image =
  Picture pictureType
    =-> Just
      PictureData
        { pictureMimeType = "image/jpeg",
          pictureDescription = "",
          pictureWidth = fromIntegral (imageWidth image),
          pictureHeight = fromIntegral (imageHeight image),
          pictureDepth = 24,
          pictureColors = 0, -- non-indexed
          pictureData = BL.toStrict (encodeJpegAtQuality q image)
        }

-- | Write the given image into FLAC metadata block corresponding to a
-- specific 'PictureType'.
writePngPicture ::
  -- | Type of picture we're writing
  PictureType ->
  -- | The picture to write
  Image PixelRGB8 ->
  FlacMeta ()
writePngPicture pictureType image =
  Picture pictureType
    =-> Just
      PictureData
        { pictureMimeType = "image/png",
          pictureDescription = "",
          pictureWidth = fromIntegral (imageWidth image),
          pictureHeight = fromIntegral (imageHeight image),
          pictureDepth = 24,
          pictureColors = 0, -- non-indexed
          pictureData = BL.toStrict (encodePng image)
        }
