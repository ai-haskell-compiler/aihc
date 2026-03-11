module Main (main) where

import           Control.Concurrent (threadDelay)
import           WLED.Device        (getLampState, setLampState)
import           WLED.Lamp.Octocat  (octocatSpec)
import           WLED.Pattern.Flags (bulgaria)
import           WLED.Types         (diff)

main :: IO ()
main = do
    -- Connect to a WLED device
    lampState <- getLampState wledUrl
    case lampState of
        Left errMsg -> putStrLn errMsg
        Right initialState -> do
            -- Display the Bulgarian flag on an Octolamp
            _ <- setLampState wledUrl (bulgaria octocatSpec)

            -- Just sleep five seconds
            threadDelay 5000000

            -- Restore the initial state
            Right currentState <- getLampState wledUrl
            _ <- setLampState wledUrl (diff currentState initialState)
            pure ()
  where
    wledUrl = "http://192.168.178.34"
