{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.Constants
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This purely internal module defines all AL\/ALC constants, which have been
-- figured out by configure. In contrast to OpenGL and GLUT, these constants
-- varied on different platforms in the past and have evolved quite a bit.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.Constants where

import Sound.OpenAL.Config (
   ALboolean, ALint, ALenum, ALCboolean, ALCint, ALCenum )

--------------------------------------------------------------------------------

al_FALSE, al_TRUE :: ALboolean
al_FALSE                            = 0x0000
al_TRUE                             = 0x0001

al_NO_ERROR, al_INVALID_NAME, al_INVALID_ENUM, al_INVALID_VALUE,
   al_INVALID_OPERATION, al_OUT_OF_MEMORY :: ALenum
al_NO_ERROR                         = 0x0000
al_INVALID_NAME                     = 0xA001
al_INVALID_ENUM                     = 0xA002
al_INVALID_VALUE                    = 0xA003
al_INVALID_OPERATION                = 0xA004
al_OUT_OF_MEMORY                    = 0xA005

--------------------------------------------------------------------------------

al_DISTANCE_MODEL, al_DOPPLER_FACTOR, al_SPEED_OF_SOUND :: ALenum
al_DISTANCE_MODEL                   = 0xD000
al_DOPPLER_FACTOR                   = 0xC000
al_SPEED_OF_SOUND                   = 0xC003

al_VERSION, al_RENDERER, al_VENDOR, al_EXTENSIONS :: ALenum
al_VERSION                          = 0xB002
al_RENDERER                         = 0xB003
al_VENDOR                           = 0xB001
al_EXTENSIONS                       = 0xB004

al_NONE, al_INVERSE_DISTANCE, al_INVERSE_DISTANCE_CLAMPED, al_LINEAR_DISTANCE,
   al_LINEAR_DISTANCE_CLAMPED, al_EXPONENT_DISTANCE,
   al_EXPONENT_DISTANCE_CLAMPED :: ALenum
al_NONE                             = 0x0000
al_INVERSE_DISTANCE                 = 0xD001
al_INVERSE_DISTANCE_CLAMPED         = 0xD002
al_LINEAR_DISTANCE                  = 0xD003
al_LINEAR_DISTANCE_CLAMPED          = 0xD004
al_EXPONENT_DISTANCE                = 0xD005
al_EXPONENT_DISTANCE_CLAMPED        = 0xD006

--------------------------------------------------------------------------------

al_POSITION, al_VELOCITY, al_GAIN :: ALenum
al_POSITION                         = 0x1004
al_VELOCITY                         = 0x1006
al_GAIN                             = 0x100A

al_ORIENTATION :: ALenum
al_ORIENTATION                      = 0x100F

al_SOURCE_RELATIVE, al_SOURCE_TYPE, al_LOOPING, al_BUFFER, al_BUFFERS_QUEUED,
   al_BUFFERS_PROCESSED, al_MIN_GAIN, al_MAX_GAIN, al_REFERENCE_DISTANCE,
   al_ROLLOFF_FACTOR, al_MAX_DISTANCE, al_PITCH, al_DIRECTION,
   al_CONE_INNER_ANGLE, al_CONE_OUTER_ANGLE, al_CONE_OUTER_GAIN, al_SEC_OFFSET,
   al_SAMPLE_OFFSET, al_BYTE_OFFSET, al_SOURCE_STATE :: ALenum
al_SOURCE_RELATIVE                  = 0x0202
al_SOURCE_TYPE                      = 0x1027
al_LOOPING                          = 0x1007
al_BUFFER                           = 0x1009
al_BUFFERS_QUEUED                   = 0x1015
al_BUFFERS_PROCESSED                = 0x1016
al_MIN_GAIN                         = 0x100D
al_MAX_GAIN                         = 0x100E
al_REFERENCE_DISTANCE               = 0x1020
al_ROLLOFF_FACTOR                   = 0x1021
al_MAX_DISTANCE                     = 0x1023
al_PITCH                            = 0x1003
al_DIRECTION                        = 0x1005
al_CONE_INNER_ANGLE                 = 0x1001
al_CONE_OUTER_ANGLE                 = 0x1002
al_CONE_OUTER_GAIN                  = 0x1022
al_SEC_OFFSET                       = 0x1024
al_SAMPLE_OFFSET                    = 0x1025
al_BYTE_OFFSET                      = 0x1026
al_SOURCE_STATE                     = 0x1010

al_UNDETERMINED, al_STATIC, al_STREAMING :: ALint
al_UNDETERMINED                     = 0x1030
al_STATIC                           = 0x1028
al_STREAMING                        = 0x1029

al_INITIAL, al_PLAYING, al_PAUSED, al_STOPPED :: ALint
al_INITIAL                          = 0x1011
al_PLAYING                          = 0x1012
al_PAUSED                           = 0x1013
al_STOPPED                          = 0x1014

--------------------------------------------------------------------------------

al_FREQUENCY, al_SIZE, al_BITS, al_CHANNELS :: ALenum
al_FREQUENCY                        = 0x2001
al_SIZE                             = 0x2004
al_BITS                             = 0x2002
al_CHANNELS                         = 0x2003

al_FORMAT_MONO8, al_FORMAT_MONO16, al_FORMAT_STEREO8,
   al_FORMAT_STEREO16 :: ALenum
al_FORMAT_MONO8                     = 0x1100
al_FORMAT_MONO16                    = 0x1101
al_FORMAT_STEREO8                   = 0x1102
al_FORMAT_STEREO16                  = 0x1103

--------------------------------------------------------------------------------

alc_FALSE, alc_TRUE :: ALCboolean
alc_FALSE                           = 0x0000
alc_TRUE                            = 0x0001

alc_FREQUENCY, alc_REFRESH, alc_SYNC, alc_MONO_SOURCES,
   alc_STEREO_SOURCES :: ALCint
alc_FREQUENCY                       = 0x1007
alc_REFRESH                         = 0x1008
alc_SYNC                            = 0x1009
alc_MONO_SOURCES                    = 0x1010
alc_STEREO_SOURCES                  = 0x1011

alc_NO_ERROR, alc_INVALID_DEVICE, alc_INVALID_CONTEXT, alc_INVALID_ENUM,
   alc_INVALID_VALUE, alc_INVALID_OPERATION, alc_OUT_OF_MEMORY :: ALCenum
alc_NO_ERROR                        = 0x0000
alc_INVALID_DEVICE                  = 0xA001
alc_INVALID_CONTEXT                 = 0xA002
alc_INVALID_ENUM                    = 0xA003
alc_INVALID_VALUE                   = 0xA004
alc_INVALID_OPERATION               = 0xA006
alc_OUT_OF_MEMORY                   = 0xA005

alc_DEFAULT_DEVICE_SPECIFIER, alc_DEVICE_SPECIFIER, alc_EXTENSIONS,
   alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER, alc_CAPTURE_DEVICE_SPECIFIER :: ALCenum
alc_DEFAULT_DEVICE_SPECIFIER        = 0x1004
alc_DEVICE_SPECIFIER                = 0x1005
alc_EXTENSIONS                      = 0x1006
alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER= 0x0311
alc_CAPTURE_DEVICE_SPECIFIER        = 0x0310

alc_ATTRIBUTES_SIZE, alc_ALL_ATTRIBUTES, alc_MAJOR_VERSION, alc_MINOR_VERSION,
   alc_CAPTURE_SAMPLES :: ALCenum
alc_ATTRIBUTES_SIZE                 = 0x1002
alc_ALL_ATTRIBUTES                  = 0x1003
alc_MAJOR_VERSION                   = 0x1000
alc_MINOR_VERSION                   = 0x1001
alc_CAPTURE_SAMPLES                 = 0x0312
