# Monad Logger functions for JSON

![TravisCI](https://travis-ci.org/fpco/monad-logger-json.svg)
![Hackage](https://img.shields.io/hackage/v/monad-logger-json.svg)

## Install

As a library:

    cabal install monad-logger-json

## Usage (example)

    {-# LANGUAGE TemplateHaskell #-}

    import Control.Monad.IO.Class ( MonadIO(liftIO) )
    import Control.Monad.Logger ( runStdoutLoggingT )
    import Control.Monad.Logger.JSON ( logInfoJ, logDebugJ )
    import Data.Aeson.TH ( defaultOptions, deriveJSON )
    import Data.Time.Clock ( UTCTime, getCurrentTime )

    data Message = Message { time :: UTCTime }

    $( deriveJSON defaultOptions ''Message )

    main :: IO ()
    main =
      runStdoutLoggingT
        (do now <- liftIO getCurrentTime
            $logDebugJ (Message now)
            $logInfoJ "Hello world")
