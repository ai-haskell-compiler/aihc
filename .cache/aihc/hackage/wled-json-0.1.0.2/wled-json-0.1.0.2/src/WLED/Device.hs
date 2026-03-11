{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : WLED.Device
Copyright   : (c) Andreas Ländle, 2024-2025
License     : BSD-3
Stability   : experimental

Interacting with the WLED device.
-}

module WLED.Device
    ( getLampState,
      setLampState
    ) where

import           Data.Aeson                  (eitherDecodeStrict, encode)
import           Data.ByteString             (ByteString, toStrict)
import           Network.HTTP.Client.Conduit (Request (method), path)
import           Network.HTTP.Simple         (getResponseBody, httpBS, parseRequest, setRequestBodyJSON)
import           WLED.Types                  (StateComplete, StatePatch)


{- |
Retrieves the current lamp state.

==== __Example__

>>> getLampState "http://192.168.178.34"
Right (State {stateOn = True, stateBri = 128, stateTransition = 7, statePs = -1, statePl = -1, stateNl = Nightlight {nightlightOn = False, nightlightDur = 60, nightlightMode = 1, nightlightTbri = 0, nightlightRem = -1}, stateLor = 0, stateMainseg = 0, stateSeg = [Segment {segmentId = 0, segmentStart = 0, segmentStop = 101, segmentLen = 101, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,160,0],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0}]})
-}
getLampState :: String -> IO (Either String StateComplete)
getLampState wledUrl = do
    req <- parseRequest wledUrl
    res <- httpBS req { path = "json/state" }
    pure (eitherDecodeStrict $ getResponseBody res)

{- |
Alters the current lamp state.

==== __Examples__

>>> setLampState "http://192.168.178.34" $ (mempty :: StatePatch) { stateBri = Just 255 } <> (mempty :: StatePatch) { stateOn = Nothing }
("{\"bri\":255}","{\"success\":true}")

>>> setLampState "http://192.168.178.34" france
("{\"seg\":[{\"start\":0,\"stop\":5,\"col\":[[255,255,255]]},{\"start\":5,\"stop\":16,\"col\":[[255,0,0]]},{\"start\":16,\"stop\":23,\"col\":[[255,255,255]]},{\"start\":23,\"stop\":34,\"col\":[[0,0,255]]},{\"start\":34,\"stop\":41,\"col\":[[255,255,255]]},{\"start\":41,\"stop\":52,\"col\":[[0,0,255]]},{\"start\":52,\"stop\":57,\"col\":[[255,255,255]]},{\"start\":57,\"stop\":68,\"col\":[[255,0,0]]},{\"start\":68,\"stop\":101,\"col\":[[255,255,255]]}]}","{\"success\":true}")
-}
setLampState :: String -> StatePatch -> IO (ByteString, ByteString)
setLampState wledUrl patch =
    let body = encode patch
    in do
      req <- parseRequest wledUrl
      res <- httpBS $ setRequestBodyJSON patch req { method = "POST", path = "json/state" }
      pure (toStrict body, getResponseBody res)
