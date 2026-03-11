------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer
-- Copyright : (c) Henning Thielemann, 2010
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- Overview: <http://www.alsa-project.org/alsa-doc/alsa-lib/seq.html>
--
-- WARNING: This whole library does not seem to be particlarly thread aware.
-- Perhaps place the sequencer handle in an MVar?

module Sound.ALSA.Sequencer
  ( -- * Sequencer
    Seq.T
  , OpenMode
  , AllowOutput
  , AllowInput
  , OutputMode(..)
  , InputMode(..)
  , DuplexMode(..)

  , BlockMode(..)

  , open
  , openDefault
  , close
  , with
  , withDefault
  , defaultName
  , getName
  , setBlocking

   -- ** Manage user-space buffers
  , getOutputBufferSize
  , setOutputBufferSize
  , getInputBufferSize
  , setInputBufferSize

  -- ** Manage kernel-space memory pools
  , setPoolOutput
  , setPoolOutputRoom
  , resetPoolOutput
  , setPoolInput
  , resetPoolInput
  ) where

import Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import Sound.ALSA.Sequencer.Sequencer
