{-# LANGUAGE OverloadedStrings #-}

module Text.CueSheet.RenderSpec (spec) where

import Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.QuickCheck
import Text.CueSheet.Parser
import Text.CueSheet.Render
import Text.CueSheet.Types

spec :: Spec
spec =
  describe "renderCueSheet" $ do
    it "doesn't ever produce trailing whitespace" $
      property $ \csrf cueSheet -> do
        let r = renderCueSheet csrf cueSheet
            f x = if csrf then BL.take (BL.length x - 1) x else x
        BL.split 10 r
          `shouldNotSatisfy` any ((" " `BL.isSuffixOf`) . f)
    it "always ends with the specified new line sequence" $
      property $ \csrf cueSheet -> do
        let r = renderCueSheet csrf cueSheet
            eol = if csrf then "\r\n" else "\n"
        r `shouldSatisfy` (eol `BL.isSuffixOf`)
    it "doesn't look too bad" $ do
      expected <- BL.readFile "cue-sheet-samples/rendered0.cue"
      cueSheet <- testCueSheet
      renderCueSheet False cueSheet `shouldBe` expected
    it "produces content that can be correctly parsed back" $
      property $ \csrf cueSheet ->
        parseCueSheet "" (BL.toStrict $ renderCueSheet csrf cueSheet)
          `shouldBe` Right cueSheet

-- | A manually constructed testing CUE sheet.
testCueSheet :: MonadThrow m => m CueSheet
testCueSheet = do
  mcn <- mkMcn "1112223334445"
  performer <- mkCueText "The Famous Foobar"
  title <- mkCueText "Bobla Fett"
  songwriter <- mkCueText "Paul"
  isrc <- mkIsrc "agree1234567"
  pregap <- fromMmSsFf 1 0 13
  index <- fromMmSsFf 0 0 15
  postgap <- fromMmSsFf 0 8 9
  return
    CueSheet
      { cueCatalog = Just mcn,
        cueCdTextFile = Just "/home/mark/mycdtextfile.txt",
        cuePerformer = Just performer,
        cueTitle = Just title,
        cueSongwriter = Just songwriter,
        cueFirstTrackNumber = 1,
        cueFiles =
          NE.fromList
            [ CueFile
                { cueFileName = "the-famous-foobar-cd1.flac",
                  cueFileType = Wave,
                  cueFileTracks =
                    NE.fromList
                      [ CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = True,
                            cueTrackSerialCopyManagement = True,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Just isrc,
                            cueTrackTitle = Just title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = index :| [],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = True,
                            cueTrackFourChannelAudio = True,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackCdg,
                            cueTrackIsrc = Just isrc,
                            cueTrackTitle = Just title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Just pregap,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = index :| [],
                            cueTrackPostgap = Just postgap
                          }
                      ]
                },
              CueFile
                { cueFileName = "the-famous-foobar-cd2.mp3",
                  cueFileType = MP3,
                  cueFileTracks =
                    NE.fromList
                      [ CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackMode1_2048,
                            cueTrackIsrc = Just isrc,
                            cueTrackTitle = Just title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = index :| [],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = True,
                            cueTrackFourChannelAudio = True,
                            cueTrackPreemphasisEnabled = True,
                            cueTrackSerialCopyManagement = True,
                            cueTrackType = CueTrackMode2_2336,
                            cueTrackIsrc = Just isrc,
                            cueTrackTitle = Just title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = index :| [],
                            cueTrackPostgap = Nothing
                          }
                      ]
                }
            ]
      }
