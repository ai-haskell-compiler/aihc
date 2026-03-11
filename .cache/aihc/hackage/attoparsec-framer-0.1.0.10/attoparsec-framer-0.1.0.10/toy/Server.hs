{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Server
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

A demo server that responds to the demo client via a framed protocol.

* once started, it waits for client connections
  * it loops forever, eventually it needs to be shutdown using CTRL-C
* when a client connects, it responds to valid client requests, on invalid
  requests the connection is closed
* valid requests are @'Header's@ that indicate the number of frames to send and
  their maximum size
  * it sends a series of responses that match the request specification
  * if the requested response size is 0, it closes the connection
-}
module Main (main) where

import Attoparsec.ToyFrame (Header (..), asBytes, genAscFullFrames, parseHeader)
import Data.Attoparsec.Framer (
  Framer,
  Progression (..),
  mkFramer',
  runFramer,
  setOnBadParse,
  setOnClosed,
 )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)


main :: IO ()
main = runTCPServer Nothing "3927" $ \s -> do
  Text.putStrLn "a toy client connected"
  runFramer $ mkServerFramer s


type ByteSink = BS.ByteString -> IO ()


mkServerFramer :: Socket -> Framer IO Header
mkServerFramer s =
  let onHeader' = onHeader $ sendAll s
   in setOnClosed onClosed $
        setOnBadParse onFailedParse $
          mkFramer' parseHeader onHeader' (recv s . fromIntegral)


onHeader :: ByteSink -> Header -> IO Progression
onHeader sink Header {hResponseSize, hMaxPayloadSize} = do
  if (hResponseSize == 0)
    then -- hResponseSize is 0; the client means 'bye', stop waiting for input
    do
      Text.putStrLn "a toy client sent bye"
      pure Stop
    else do
      -- hResponseSize > 0; starting from 1, send a frame with a body whose max size is hMaxPayloadSize
      -- generate a list of frames counting up to the index provided in the header
      toSend <- genAscFullFrames hResponseSize hMaxPayloadSize
      mapM_ sink $ map asBytes toSend
      pure Continue


onFailedParse :: Text -> IO ()
onFailedParse cause = do
  -- if does not parse as a frame header terminate the connection
  -- no explicit exception is raised here, so runFramer throws
  Text.putStrLn $ "parse error ended a connection from a toy client: " <> cause


onClosed :: IO ()
onClosed = Text.putStrLn "a toy client closed a connection"
