{-# LANGUAGE OverloadedStrings #-}

module Text.CueSheet.ParserSpec (spec) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.CueSheet.Parser
import Text.CueSheet.Types
import Text.Megaparsec

spec :: Spec
spec =
  describe "parseCueSheet" $ do
    it "correctly deals with whitespace" $ do
      bs <- B.readFile "cue-sheet-samples/parser-whitespace.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "parses commands case-insensitively" $ do
      bs <- B.readFile "cue-sheet-samples/parser-case-insensitivity.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "handles comments properly" $ do
      bs <- B.readFile "cue-sheet-samples/parser-comments.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "at least one file should be declared" $ do
      bs <- B.readFile "cue-sheet-samples/parser-missing-file.cue"
      let es =
            foldMap
              etoks
              [ "CATALOG",
                "CDTEXTFILE",
                "FILE",
                "PERFORMER",
                "REM",
                "SONGWRITER",
                "TITLE"
              ]
      parseCueSheet "" bs
        `shouldFailWith` err 67 (ueof <> es)
    it "at least one track should be declared" $ do
      bs <- B.readFile "cue-sheet-samples/parser-missing-track.cue"
      parseCueSheet "" bs
        `shouldFailWith` err 109 (ueof <> etoks "REM" <> etoks "TRACK")
    it "at least one non-zero index should be declared" $ do
      bs <- B.readFile "cue-sheet-samples/parser-missing-index.cue"
      let e = wrappedTrivial (ueof <> etoks "INDEX" <> etoks "REM")
      parseCueSheet "" bs
        `shouldFailWith` errFancy 174 (cstm (Eec (Just 1) e))
    it "parses a normal CUE file without issues" $ do
      bs <- B.readFile "cue-sheet-samples/parser-normal.cue"
      sheet <- normalCueSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "rejects invalid catalog number" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-catalog.cue"
      parseCueSheet "" bs
        `shouldFailWith` errFancy
          8
          (cstm (Eec Nothing (CueParserInvalidCatalog "123")))
    it "rejects invalid CUE text" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-text.cue"
      parseCueSheet "" bs
        `shouldFailWith` errFancy
          10
          (cstm (Eec Nothing (CueParserInvalidCueText "")))
    it "detects and reports tracks that appear out of order" $ do
      bs <- B.readFile "cue-sheet-samples/parser-track-out-of-order.cue"
      parseCueSheet "" bs
        `shouldFailWith` errFancy
          182
          (cstm (Eec Nothing CueParserTrackOutOfOrder))
    it "rejects invalid ISRC values" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-isrc.cue"
      parseCueSheet "" bs
        `shouldFailWith` errFancy
          161
          (cstm (Eec (Just 3) (CueParserInvalidTrackIsrc "123")))
    it "rejects invalid number of seconds" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-seconds.cue"
      parseCueSheet "" bs
        `shouldFailWith` errFancy
          168
          (cstm (Eec (Just 1) (CueParserInvalidSeconds 61)))
    it "rejects invalid number of frames" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-frames.cue"
      parseCueSheet "" bs
        `shouldFailWith` errFancy
          171
          (cstm (Eec (Just 1) (CueParserInvalidFrames 77)))
    it "detects and reports indices that appear out of order" $ do
      bs <- B.readFile "cue-sheet-samples/parser-index-out-of-order.cue"
      parseCueSheet "" bs
        `shouldFailWith` errFancy
          162
          (cstm (Eec (Just 1) CueParserTrackIndexOutOfOrder))
    it "rejects duplicate CATALOG command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-catalog.cue"
      let es =
            utoks "CATA"
              <> foldMap
                etoks
                [ "CDTEXTFILE",
                  "FILE",
                  "PERFORMER",
                  "REM",
                  "SONGWRITER",
                  "TITLE"
                ]
      parseCueSheet "" bs `shouldFailWith` err 22 es
    it "rejects duplicate CDTEXTFILE command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-cdtextfile.cue"
      let es =
            utoks "CDTE"
              <> foldMap
                etoks
                [ "CATALOG",
                  "FILE",
                  "PERFORMER",
                  "REM",
                  "SONGWRITER",
                  "TITLE"
                ]
      parseCueSheet "" bs `shouldFailWith` err 23 es
    it "rejects duplicate PERFORMER command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-performer.cue"
      let es =
            utoks "PERF"
              <> foldMap
                etoks
                [ "CATALOG",
                  "CDTEXTFILE",
                  "FILE",
                  "REM",
                  "SONGWRITER",
                  "TITLE"
                ]
      parseCueSheet "" bs `shouldFailWith` err 30 es
    it "rejects duplicate TITLE command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-title.cue"
      let es =
            utoks "TITL"
              <> foldMap
                etoks
                [ "CATALOG",
                  "CDTEXTFILE",
                  "FILE",
                  "PERFORMER",
                  "REM",
                  "SONGWRITER"
                ]
      parseCueSheet "" bs `shouldFailWith` err 17 es
    it "rejects duplicate SONGWRITER command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-songwriter.cue"
      let es =
            utoks "SONG"
              <> foldMap
                etoks
                [ "CATALOG",
                  "CDTEXTFILE",
                  "FILE",
                  "PERFORMER",
                  "REM",
                  "TITLE"
                ]
      parseCueSheet "" bs `shouldFailWith` err 18 es
    it "rejects duplicate FLAGS command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-flags.cue"
      let es =
            wrappedTrivial $
              utoks "FLAGS"
                <> foldMap
                  etoks
                  [ "INDEX",
                    "ISRC",
                    "PERFORMER",
                    "PREGAP",
                    "REM",
                    "SONGWRITER",
                    "TITLE"
                  ]
      parseCueSheet "" bs
        `shouldFailWith` errFancy 144 (cstm (Eec (Just 1) es))
    it "rejects duplicate ISRC command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-isrc.cue"
      let es =
            wrappedTrivial $
              utoks "ISRC "
                <> foldMap
                  etoks
                  [ "FLAGS",
                    "INDEX",
                    "PERFORMER",
                    "PREGAP",
                    "REM",
                    "SONGWRITER",
                    "TITLE"
                  ]
      parseCueSheet "" bs
        `shouldFailWith` errFancy 152 (cstm (Eec (Just 1) es))
    it "rejects duplicate PERFORMER command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-performer.cue"
      let es =
            wrappedTrivial $
              utoks "PERFO"
                <> foldMap
                  etoks
                  [ "FLAGS",
                    "INDEX",
                    "ISRC",
                    "PREGAP",
                    "REM",
                    "SONGWRITER",
                    "TITLE"
                  ]
      parseCueSheet "" bs
        `shouldFailWith` errFancy 156 (cstm (Eec (Just 1) es))
    it "rejects duplicate TITLE command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-title.cue"
      let es =
            wrappedTrivial $
              utoks "TITLE"
                <> foldMap
                  etoks
                  [ "FLAGS",
                    "INDEX",
                    "ISRC",
                    "PERFORMER",
                    "PREGAP",
                    "REM",
                    "SONGWRITER"
                  ]
      parseCueSheet "" bs
        `shouldFailWith` errFancy 146 (cstm (Eec (Just 1) es))
    it "rejects duplicate SONGWRITER command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-songwriter.cue"
      let es =
            wrappedTrivial $
              utoks "SONGW"
                <> foldMap
                  etoks
                  [ "FLAGS",
                    "INDEX",
                    "ISRC",
                    "PERFORMER",
                    "PREGAP",
                    "REM",
                    "TITLE"
                  ]
      parseCueSheet "" bs
        `shouldFailWith` errFancy 150 (cstm (Eec (Just 1) es))
    it "rejects duplicate PREGAP command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-pregap.cue"
      let es =
            wrappedTrivial $
              utoks "PREGA"
                <> foldMap
                  etoks
                  [ "FLAGS",
                    "INDEX",
                    "ISRC",
                    "PERFORMER",
                    "REM",
                    "SONGWRITER",
                    "TITLE"
                  ]
      parseCueSheet "" bs
        `shouldFailWith` errFancy 176 (cstm (Eec (Just 1) es))
    it "rejects duplicate POSTGAP command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-postgap.cue"
      let es =
            foldMap
              etoks
              [ "FILE",
                "REM",
                "TRACK"
              ]
      parseCueSheet "" bs
        `shouldFailWith` err 199 (utok 80 <> es <> eeof)

testSheet :: MonadThrow m => m CueSheet
testSheet = do
  performer <- mkCueText "Faithless"
  title <- mkCueText "Live in Berlin"
  trackTitle <- mkCueText "Reverence"
  return
    CueSheet
      { cueCatalog = Nothing,
        cueCdTextFile = Nothing,
        cuePerformer = Just performer,
        cueTitle = Just title,
        cueSongwriter = Nothing,
        cueFirstTrackNumber = 1,
        cueFiles =
          NE.fromList
            [ CueFile
                { cueFileName = "Faithless - Live in Berlin.mp3",
                  cueFileType = MP3,
                  cueFileTracks =
                    NE.fromList
                      [ CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just trackTitle,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices =
                              NE.fromList
                                [CueTime 3, CueTime 151],
                            cueTrackPostgap = Nothing
                          }
                      ]
                }
            ]
      }

normalCueSheet :: MonadThrow m => m CueSheet
normalCueSheet = do
  performer <- mkCueText "Faithless"
  catalog <- mkMcn "1112223334445"
  title <- mkCueText "Live in Berlin"
  track1Title <- mkCueText "Reverence"
  track2Title <- mkCueText "She's My Baby"
  track3Title <- mkCueText "Take the Long Way Home"
  track4Title <- mkCueText "Insomnia"
  track5Title <- mkCueText "Bring the Family Back"
  track6Title <- mkCueText "Salva Mea"
  track7Title <- mkCueText "Dirty Old Man"
  track8Title <- mkCueText "God Is a DJ"
  return
    CueSheet
      { cueCatalog = Just catalog,
        cueCdTextFile = Just "blah-blah.txt",
        cuePerformer = Just performer,
        cueTitle = Just title,
        cueSongwriter = Nothing,
        cueFirstTrackNumber = 1,
        cueFiles =
          NE.fromList
            [ CueFile
                { cueFileName = "Faithless - Live in Berlin.mp3",
                  cueFileType = MP3,
                  cueFileTracks =
                    NE.fromList
                      [ CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track1Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Just (CueTime 75),
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = NE.fromList [CueTime 0],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track2Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = NE.fromList [CueTime 30150],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track3Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = NE.fromList [CueTime 49050],
                            cueTrackPostgap = Just (CueTime 76)
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track4Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices =
                              NE.fromList
                                [CueTime 76800, CueTime 76805],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = True,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track5Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = NE.fromList [CueTime 115800],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track6Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = NE.fromList [CueTime 138750],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track7Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = NE.fromList [CueTime 172800],
                            cueTrackPostgap = Nothing
                          },
                        CueTrack
                          { cueTrackDigitalCopyPermitted = False,
                            cueTrackFourChannelAudio = False,
                            cueTrackPreemphasisEnabled = False,
                            cueTrackSerialCopyManagement = False,
                            cueTrackType = CueTrackAudio,
                            cueTrackIsrc = Nothing,
                            cueTrackTitle = Just track8Title,
                            cueTrackPerformer = Just performer,
                            cueTrackSongwriter = Nothing,
                            cueTrackPregap = Nothing,
                            cueTrackPregapIndex = Nothing,
                            cueTrackIndices = NE.fromList [CueTime 191625],
                            cueTrackPostgap = Nothing
                          }
                      ]
                }
            ]
      }

-- | Construct 'CueParserFailure' from given @'ET' 'Word8'@ thing.
wrappedTrivial :: ET ByteString -> CueParserFailure
wrappedTrivial xs =
  case err 0 xs of
    TrivialError _ us es -> CueParserTrivialError us es
    _ -> error "Ooops!"

-- | Quickly make a custom error component.
cstm :: e -> EF e
cstm = fancy . ErrorCustom
{-# INLINE cstm #-}
