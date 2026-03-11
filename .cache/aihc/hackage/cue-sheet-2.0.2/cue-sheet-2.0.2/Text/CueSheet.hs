-- |
-- Module      :  Text.CueSheet
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module allows us to construct, read, and write CUE sheets. The data
-- types are defined in such a way that incorrect CUE sheets cannot be
-- represented. See 'parseCueSheet' for parsing of plain text CUE sheet
-- files and 'renderCueSheet' for rendering.
module Text.CueSheet
  ( -- * Types
    CueSheet (..),
    CueFile (..),
    CueFileType (..),
    CueTrack (..),
    CueTrackType (..),
    CueTime (..),
    fromMmSsFf,
    toMmSsFf,
    showMmSsFf,
    Mcn,
    mkMcn,
    unMcn,
    CueText,
    mkCueText,
    unCueText,
    Isrc,
    mkIsrc,
    unIsrc,
    CueSheetException (..),

    -- * Parsing
    parseCueSheet,
    CueParserFailure (..),
    Eec (..),

    -- * Rendering
    renderCueSheet,
  )
where

import Text.CueSheet.Parser
import Text.CueSheet.Render
import Text.CueSheet.Types
