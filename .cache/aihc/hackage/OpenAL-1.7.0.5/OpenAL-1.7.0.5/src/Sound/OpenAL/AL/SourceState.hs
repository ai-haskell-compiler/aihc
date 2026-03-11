{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.SourceState
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.SourceState (
   SourceState(..), unmarshalSourceState
) where

import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.Constants

--------------------------------------------------------------------------------

-- | Each source can be in one of four possible execution states: 'Initial',
-- 'Playing', 'Paused', 'Stopped'. Sources that are either 'Playing' or 'Paused'
-- are considered active. Sources that are 'Stopped' or 'Initial' are considered
-- inactive. Only 'Playing' sources are included in the processing. The
-- implementation is free to skip those processing stages for sources that have
-- no effect on the output (e.g. mixing for a source muted by zero gain, but not
-- sample offset increments). Depending on the current state of a source certain
-- (e.g. repeated) state transition commands are legal NOPs: they will be
-- ignored, no error is generated.

data SourceState =
     Initial
   | Playing
   | Paused
   | Stopped
   deriving ( Eq, Ord, Show )

unmarshalSourceState :: ALint -> SourceState
unmarshalSourceState x
   | x == al_INITIAL = Initial
   | x == al_PLAYING = Playing
   | x == al_PAUSED = Paused
   | x == al_STOPPED = Stopped
   | otherwise = error ("unmarshalSourceState: illegal value " ++ show x)
