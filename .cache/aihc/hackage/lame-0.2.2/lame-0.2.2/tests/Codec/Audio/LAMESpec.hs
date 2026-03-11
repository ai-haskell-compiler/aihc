{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.LAMESpec (spec) where

import Codec.Audio.LAME
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Sound.HTagLib
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

data Info = Info
  { infoTitle :: Title,
    infoArtist :: Artist,
    infoAlbum :: Album,
    infoComment :: Comment,
    infoGenre :: Genre,
    infoYear :: Maybe Year,
    infoTrackNumber :: Maybe TrackNumber,
    infoDuration :: Duration,
    infoBitRate :: BitRate,
    infoSampleRate :: SampleRate,
    infoChannels :: Channels
  }
  deriving (Eq, Show)

spec :: Spec
spec =
  describe "encodeMp3" $
    withVariousWaves $
      it "produces correct MP3 file" $ \(ipath, opath) -> do
        encodeMp3
          defaultEncoderSettings
            { encoderTagTitle = pure tagTitle,
              encoderTagArtist = pure tagArtist,
              encoderTagAlbum = pure tagAlbum,
              encoderTagYear = pure tagYear,
              encoderTagComment = pure tagComment,
              encoderTagTrack = pure tagTrack,
              encoderTagGenre = pure tagGenre
            }
          ipath
          opath
        Info {..} <-
          getTags' opath MPEG $
            Info
              <$> titleGetter
              <*> artistGetter
              <*> albumGetter
              <*> commentGetter
              <*> genreGetter
              <*> yearGetter
              <*> trackNumberGetter
              <*> durationGetter
              <*> bitRateGetter
              <*> sampleRateGetter
              <*> channelsGetter
        unTitle infoTitle `shouldBe` tagTitle
        unArtist infoArtist `shouldBe` tagArtist
        unAlbum infoAlbum `shouldBe` tagAlbum
        fmap (T.pack . show . unYear) infoYear `shouldBe` pure tagYear
        unComment infoComment `shouldBe` tagComment
        fmap unTrackNumber infoTrackNumber
          `shouldBe` (pure . fromIntegral . fst) tagTrack
        unGenre infoGenre `shouldBe` tagGenre
        unDuration infoDuration `shouldBe` 1
        unBitRate infoBitRate `shouldBe` 128
        unSampleRate infoSampleRate `shouldBe` 44100
        unChannels infoChannels `shouldBe` 2

----------------------------------------------------------------------------
-- Helpers

-- | Run given test with various WAVE files.
withVariousWaves :: SpecWith (FilePath, FilePath) -> SpecWith ()
withVariousWaves m =
  forM_ waveFiles $ \(path, desc) ->
    context ("when given " ++ desc) (around (withSandbox path) m)

-- | Make a temporary copy of given file and provide the path to the file in
-- a sandbox directory. Automatically remove the files when the test
-- finishes.
withSandbox :: FilePath -> ActionWith (FilePath, FilePath) -> IO ()
withSandbox path action =
  withSystemTempDirectory "lame-test" $ \dir -> do
    let ipath = dir </> "файл" -- testing Unicode
        opath = dir </> "результат"
    copyFile path ipath
    action (ipath, opath)

waveFiles :: [(FilePath, String)]
waveFiles =
  [ ( "audio-samples/16bit-int.wav",
      "2 channels 44100 Hz 16 bit PCM"
    ),
    ( "audio-samples/32bit-float.wav",
      "2 channels 44100 Hz 32 bit float PCM"
    ),
    ( "audio-samples/64bit-float.wav",
      "2 channels 44100 Hz 64 bit float PCM"
    )
  ]

tagTitle, tagArtist, tagAlbum, tagYear, tagComment, tagGenre :: Text
tagTitle = "Название"
tagArtist = "Исполнитель"
tagAlbum = "Альбом"
tagYear = "2017"
tagComment = "Комментарий тут…"
tagGenre = "Жанр"

tagTrack :: (Word8, Maybe Word8)
tagTrack = (1, Just 10)
