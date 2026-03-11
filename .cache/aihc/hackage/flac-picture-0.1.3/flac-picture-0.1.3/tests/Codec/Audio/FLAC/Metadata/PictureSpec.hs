{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.FLAC.Metadata.PictureSpec (spec) where

import Codec.Audio.FLAC.Metadata
import Codec.Audio.FLAC.Metadata.Picture
import Codec.Picture
import Control.Monad
import Data.ByteString.Lazy qualified as BL
import Data.Function (on)
import System.Directory
import System.IO
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

spec :: Spec
spec = around withSandbox $ do
  describe "writeJpegPicture/retrieveImage" . forM_ [minBound .. maxBound] $ \ptype ->
    it ("writes JPEG picture and reads it back: " ++ show ptype) $ \path -> do
      Right (ImageYCbCr8 pic) <- readJpeg "picture-samples/lenna.jpeg"
      runFlacMeta def path (writeJpegPicture ptype 127 pic)
      Right (ImageYCbCr8 pic'') <-
        (pure . decodeJpeg . BL.toStrict)
          (encodeJpegAtQuality 100 pic)
      Just PictureData {..} <- runFlacMeta def path (retrieve $ Picture ptype)
      pictureMimeType `shouldBe` "image/jpeg"
      pictureDescription `shouldBe` ""
      pictureWidth `shouldBe` fromIntegral (imageWidth pic)
      pictureHeight `shouldBe` fromIntegral (imageHeight pic)
      pictureDepth `shouldBe` 24
      pictureColors `shouldBe` 0
      Right (ImageYCbCr8 pic') <- runFlacMeta def path (retrieveImage ptype)
      pic' `imageShouldBe` pic''
  describe "writePngPicture/retrieveImage" . forM_ [minBound .. maxBound] $ \ptype ->
    it ("writes PNG picture and reads it back: " ++ show ptype) $ \path -> do
      Right (ImageRGB8 pic) <- readPng "picture-samples/lenna.png"
      runFlacMeta def path (writePngPicture ptype pic)
      Just PictureData {..} <- runFlacMeta def path (retrieve $ Picture ptype)
      pictureMimeType `shouldBe` "image/png"
      pictureDescription `shouldBe` ""
      pictureWidth `shouldBe` fromIntegral (imageWidth pic)
      pictureHeight `shouldBe` fromIntegral (imageHeight pic)
      pictureDepth `shouldBe` 24
      pictureColors `shouldBe` 0
      Right (ImageRGB8 pic') <- runFlacMeta def path (retrieveImage ptype)
      pic' `imageShouldBe` pic

----------------------------------------------------------------------------
-- Helpers

-- | Make a temporary copy of @audio-samples/sample.flac@ file and provide
-- the path to the file. Automatically remove the file when the test
-- finishes.
withSandbox :: ActionWith FilePath -> IO ()
withSandbox action = withSystemTempFile "sample.flac" $ \path h -> do
  hClose h
  copyFile "audio-samples/sample.flac" path
  action path

imageShouldBe ::
  (Pixel a, Show (PixelBaseComponent a), Eq (PixelBaseComponent a)) =>
  Image a ->
  Image a ->
  Expectation
imageShouldBe img0 img1 = do
  (shouldBe `on` imageWidth) img0 img1
  (shouldBe `on` imageHeight) img0 img1
  (shouldBe `on` imageData) img0 img1

-- | A shortcut for 'defaultMetaSettings'.
def :: MetaSettings
def = defaultMetaSettings
