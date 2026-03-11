{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.CueSheet.Render
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module contains a CUE sheet render. You probably want to import
-- "Text.CueSheet" instead.
module Text.CueSheet.Render
  ( renderCueSheet,
  )
where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer.Lazy
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, isSpace)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Numeric.Natural
import Text.CueSheet.Types
import Text.Printf (printf)

-- | Render a CUE sheet as a lazy 'BL.ByteString'. All 'Text' values in the
-- 'CueSheet' will be UTF-8 encoded.
renderCueSheet ::
  -- | Use CRLF sequence as “end of line” separator
  Bool ->
  -- | The 'CueSheet' to render
  CueSheet ->
  -- | The result
  BL.ByteString
renderCueSheet csrf CueSheet {..} =
  BB.toLazyByteString . execWriter . flip evalStateT (max cueFirstTrackNumber 1) $ do
    let eol = tell (if csrf then "\r\n" else "\n")
        tellText x =
          let raw = T.encodeUtf8 x
           in tell . BB.byteString $
                if B.any (isSpace . chr . fromIntegral) raw || B.null raw
                  then "\"" <> raw <> "\""
                  else raw
    forM_ cueCatalog $ \x -> do
      tell "CATALOG "
      tellText (unMcn x)
      eol
    forM_ cueCdTextFile $ \x -> do
      tell "CDTEXTFILE "
      tellText (T.pack x)
      eol
    forM_ cuePerformer $ \x -> do
      tell "PERFORMER "
      tellText (unCueText x)
      eol
    forM_ cueTitle $ \x -> do
      tell "TITLE "
      tellText (unCueText x)
      eol
    forM_ cueSongwriter $ \x -> do
      tell "SONGWRITER "
      tellText (unCueText x)
      eol
    forM_ cueFiles $ \CueFile {..} -> do
      tell "FILE "
      tellText (T.pack cueFileName)
      tell " "
      tellText (renderFileType cueFileType)
      eol
      forM_ cueFileTracks $ \CueTrack {..} -> do
        currentTrack <- get
        modify succ
        tell "  TRACK "
        tellText (formatNat currentTrack)
        tell " "
        tellText (renderTrackType cueTrackType)
        eol
        let flags =
              catMaybes
                [ if cueTrackDigitalCopyPermitted then Just "DCP" else Nothing,
                  if cueTrackFourChannelAudio then Just "4CH" else Nothing,
                  if cueTrackPreemphasisEnabled then Just "PRE" else Nothing,
                  if cueTrackSerialCopyManagement then Just "SCMS" else Nothing
                ]
        unless (null flags) $ do
          tell "    FLAGS "
          (tell . BB.byteString) (B.intercalate " " flags)
          eol
        forM_ cueTrackIsrc $ \x -> do
          tell "    ISRC "
          tellText (unIsrc x)
          eol
        forM_ cueTrackTitle $ \x -> do
          tell "    TITLE "
          tellText (unCueText x)
          eol
        forM_ cueTrackPerformer $ \x -> do
          tell "    PERFORMER "
          tellText (unCueText x)
          eol
        forM_ cueTrackSongwriter $ \x -> do
          tell "    SONGWRITER "
          tellText (unCueText x)
          eol
        forM_ cueTrackPregap $ \x -> do
          tell "    PREGAP "
          tellText (showMmSsFf x)
          eol
        forM_ cueTrackPregapIndex $ \x -> do
          tell "    INDEX 00 "
          tellText (showMmSsFf x)
          eol
        forM_ (NE.zip (NE.fromList [1 ..]) cueTrackIndices) $ \(n, index) -> do
          tell "    INDEX "
          tellText (formatNat n)
          tell " "
          tellText (showMmSsFf index)
          eol
        forM_ cueTrackPostgap $ \x -> do
          tell "    POSTGAP "
          tellText (showMmSsFf x)
          eol

----------------------------------------------------------------------------
-- Helpers

-- | Format a 'Natural' padding it with zeros to 2 digits.
formatNat :: Natural -> Text
formatNat = T.pack . printf "%02d"

-- | Render a 'CueFileType' as per the CUE specs.
renderFileType :: CueFileType -> Text
renderFileType Binary = "BINARY"
renderFileType Motorola = "MOTOROLA"
renderFileType Aiff = "AIFF"
renderFileType Wave = "WAVE"
renderFileType MP3 = "MP3"

-- | Render a 'CueTrackType' as per the CUE specs.
renderTrackType :: CueTrackType -> Text
renderTrackType CueTrackAudio = "AUDIO"
renderTrackType CueTrackCdg = "CDG"
renderTrackType CueTrackMode1_2048 = "MODE1/2048"
renderTrackType CueTrackMode1_2352 = "MODE1/2352"
renderTrackType CueTrackMode2_2336 = "MODE2/2336"
renderTrackType CueTrackMode2_2352 = "MODE2/2352"
renderTrackType CueTrackCdi2336 = "CDI/2336"
renderTrackType CueTrackCdi2352 = "CDI/2352"
