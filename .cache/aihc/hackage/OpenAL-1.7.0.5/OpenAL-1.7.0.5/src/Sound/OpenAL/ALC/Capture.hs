{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.Capture
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 6.4.2. (Capture) of the OpenAL
-- Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.Capture (
   NumSamples, captureOpenDevice, captureStart, captureNumSamples,
   captureSamples, captureStop, captureCloseDevice,
   captureDefaultDeviceSpecifier, captureDeviceSpecifier,
   allCaptureDeviceSpecifiers
) where

#if __GLASGOW_HASKELL__ >= 704
-- Make the foreign imports happy.
import Foreign.C.Types
#endif

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.StateVar ( get, GettableStateVar, makeGettableStateVar )
import Foreign.Ptr ( Ptr, nullPtr, FunPtr )

import Sound.OpenAL.AL.Buffer
import Sound.OpenAL.AL.Format
import Sound.OpenAL.ALC.ALCboolean
import Sound.OpenAL.ALC.BasicTypes
import Sound.OpenAL.ALC.Context
import Sound.OpenAL.ALC.Device
import Sound.OpenAL.ALC.Extensions
import Sound.OpenAL.ALC.QueryUtils
import Sound.OpenAL.ALC.String
import Sound.OpenAL.Config

--------------------------------------------------------------------------------

type NumSamples = ALCsizei

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

getCaptureFunc :: String -> IO (FunPtr a)
getCaptureFunc = get . alcProcAddress Nothing

--------------------------------------------------------------------------------

captureOpenDevice :: MonadIO m =>
   Maybe String -> Frequency -> Format -> NumSamples -> m (Maybe Device)
captureOpenDevice maybeDeviceSpec frequency format size = liftIO $ do
   funPtr <- getCaptureFunc "alcCaptureOpenDevice"
   let open deviceSpec =
          invokeCaptureOpenDevice funPtr deviceSpec (round frequency)
                                  (fromIntegral (marshalFormat format)) size
   fmap unmarshalDevice $
      (maybe (open nullPtr)   -- use preferred device
             (flip withALCString open)
             maybeDeviceSpec)

foreign import ccall unsafe "dynamic"
   invokeCaptureOpenDevice :: Invoker (Ptr ALCchar -> ALCuint -> ALCenum -> ALCsizei -> IO ALCdevice)

--------------------------------------------------------------------------------

captureStart :: MonadIO m => Device -> m ()
captureStart = captureStartStop "alcCaptureStart"

captureStartStop :: MonadIO m => String -> Device -> m ()
captureStartStop funName device = liftIO $ do
   funPtr <- getCaptureFunc funName
   invokeCaptureStartStop funPtr (marshalDevice device)

foreign import ccall unsafe "dynamic"
   invokeCaptureStartStop :: Invoker (ALCdevice -> IO ()) 

--------------------------------------------------------------------------------

captureNumSamples :: Device -> GettableStateVar NumSamples
captureNumSamples device = makeGettableStateVar $
   fmap fromIntegral (getInteger (Just device) CaptureSamples)

--------------------------------------------------------------------------------

captureSamples :: MonadIO m => Device -> Ptr a -> NumSamples -> m ()
captureSamples device buf n = liftIO $ do
   funPtr <- getCaptureFunc "alcCaptureSamples"
   invokeCaptureSamples funPtr (marshalDevice device) buf n

foreign import ccall unsafe "dynamic"
   invokeCaptureSamples :: Invoker (ALCdevice -> Ptr a -> NumSamples -> IO ())

--------------------------------------------------------------------------------

captureStop :: MonadIO m => Device -> m ()
captureStop = captureStartStop "alcCaptureStop"

--------------------------------------------------------------------------------

captureCloseDevice :: MonadIO m => Device -> m Bool
captureCloseDevice device = liftIO $ do
   funPtr <- getCaptureFunc "alcCaptureCloseDevice"
   fmap unmarshalALCboolean .
      invokeCaptureCloseDevice funPtr . marshalDevice $ device

foreign import ccall unsafe "dynamic"
   invokeCaptureCloseDevice :: Invoker (ALCdevice -> IO ALCboolean) 

--------------------------------------------------------------------------------

-- | Contains the name of the default capture device.

captureDefaultDeviceSpecifier :: GettableStateVar String
captureDefaultDeviceSpecifier = makeGettableStateVar $
   getString Nothing CaptureDefaultDeviceSpecifier

--------------------------------------------------------------------------------

-- | Contains the specifier string for the given capture device.

captureDeviceSpecifier :: Device -> GettableStateVar String
captureDeviceSpecifier device = makeGettableStateVar $
   getString (Just device) CaptureDeviceSpecifier

--------------------------------------------------------------------------------

-- | Contains a list of specifiers for all available capture devices.

allCaptureDeviceSpecifiers :: GettableStateVar [String]
allCaptureDeviceSpecifiers = makeGettableStateVar $ do
   enumExtPresent <- get (alcIsExtensionPresent Nothing "ALC_ENUMERATION_EXT")
   if enumExtPresent
      then peekALCStrings =<< getStringRaw Nothing CaptureDeviceSpecifier
      else fmap (\s -> [s]) $ get captureDefaultDeviceSpecifier
