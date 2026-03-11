{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.QueryUtils
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module with utilities to query OpenAL state.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.QueryUtils (
   GetPName(..), marshalGetPName,
   StringName(..), getString
) where

#if __GLASGOW_HASKELL__ >= 704
-- Make the foreign imports happy.
import Foreign.C.Types
#endif

import Foreign.Ptr ( Ptr )

import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.AL.String
import Sound.OpenAL.Constants

--------------------------------------------------------------------------------

data GetPName =
     GetDistanceModel
   | GetDopplerFactor
   | GetSpeedOfSound

   | GetPosition

   | GetVelocity
   | GetGain

   | GetOrientation

   | GetSourceRelative
   | GetSourceType
   | GetLooping
   | GetBuffer
   | GetBuffersQueued
   | GetBuffersProcessed
   | GetMinGain
   | GetMaxGain
   | GetReferenceDistance
   | GetRolloffFactor
   | GetMaxDistance
   | GetPitch
   | GetDirection
   | GetConeInnerAngle
   | GetConeOuterAngle
   | GetConeOuterGain
   | GetSecOffset
   | GetSampleOffset
   | GetByteOffset
   | GetSourceState

marshalGetPName :: GetPName -> ALenum
marshalGetPName x = case x of
   GetDistanceModel -> al_DISTANCE_MODEL
   GetDopplerFactor -> al_DOPPLER_FACTOR
   GetSpeedOfSound -> al_SPEED_OF_SOUND

   GetPosition -> al_POSITION
   GetVelocity -> al_VELOCITY
   GetGain -> al_GAIN

   GetOrientation -> al_ORIENTATION

   GetSourceRelative -> al_SOURCE_RELATIVE
   GetSourceType -> al_SOURCE_TYPE
   GetLooping -> al_LOOPING
   GetBuffer -> al_BUFFER
   GetBuffersQueued -> al_BUFFERS_QUEUED
   GetBuffersProcessed -> al_BUFFERS_PROCESSED
   GetMinGain -> al_MIN_GAIN
   GetMaxGain -> al_MAX_GAIN
   GetReferenceDistance -> al_REFERENCE_DISTANCE
   GetRolloffFactor -> al_ROLLOFF_FACTOR
   GetMaxDistance -> al_MAX_DISTANCE
   GetPitch -> al_PITCH
   GetDirection -> al_DIRECTION
   GetConeInnerAngle -> al_CONE_INNER_ANGLE
   GetConeOuterAngle -> al_CONE_OUTER_ANGLE
   GetConeOuterGain -> al_CONE_OUTER_GAIN
   GetSecOffset -> al_SEC_OFFSET
   GetSampleOffset -> al_SAMPLE_OFFSET
   GetByteOffset -> al_BYTE_OFFSET
   GetSourceState -> al_SOURCE_STATE

--------------------------------------------------------------------------------

data StringName =
     Vendor
   | Renderer
   | Version
   | Extensions
   | ALErrorCategory ALenum

marshalStringName :: StringName -> ALenum
marshalStringName x = case x of
   Vendor -> al_VENDOR
   Renderer -> al_RENDERER
   Version -> al_VERSION
   Extensions -> al_EXTENSIONS
   ALErrorCategory e -> e

getString :: StringName -> IO String
getString n = alGetString (marshalStringName n) >>= peekALString 

foreign import ccall unsafe "alGetString"
   alGetString :: ALenum -> IO (Ptr ALchar)
