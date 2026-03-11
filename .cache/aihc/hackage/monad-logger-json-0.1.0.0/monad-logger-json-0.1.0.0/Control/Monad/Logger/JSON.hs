{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Control.Monad.Logger.JSON
Description : Easy functions for logging ToJSON instances
Copyright   : (c) FPComplete, 2015
License     : MIT
Maintainer  : FP Complete Developers <dev@fpcomplete.com>
Stability   : experimental
Portability : POSIX

Template Haskell logging functions to compliment Control.Monad.Logger.
These functions handle encoding ToJSON types to the log.

Example:
```
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
```
|-}

module Control.Monad.Logger.JSON
       (logDebugJ, logInfoJ, logWarnJ, logErrorJ, logOtherJ) where

import Control.Monad.Logger ( LogLevel(..), liftLoc, monadLoggerLog )
import Data.Aeson ( encode )
import Data.Text ( Text, pack )
import Language.Haskell.TH ( Exp, Q )
import Language.Haskell.TH.Syntax ( lift, qLocation )

logJSON :: LogLevel -> Q Exp
logJSON level =
  [|monadLoggerLog $(qLocation >>= liftLoc) (pack "") $(lift level) . encode|]

logDebugJ :: Q Exp
logDebugJ = logJSON LevelDebug

logInfoJ :: Q Exp
logInfoJ = logJSON LevelInfo

logWarnJ :: Q Exp
logWarnJ = logJSON LevelWarn

logErrorJ :: Q Exp
logErrorJ = logJSON LevelError

logOtherJ :: Text -> Q Exp
logOtherJ = logJSON . LevelOther
