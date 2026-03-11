{-|
Module      : Control.Monad.Logger.Syslog
Description : Functions for directing monad-logger output to syslog
Copyright   : FPComplete, 2015
License     : MIT
Maintainer  : FP Complete Developers <dev@fpcomplete.com>
Stability   : experimental
Portability : POSIX

Example:

@
import Control.Monad.Logger ( logDebugN  )
import Control.Monad.Logger.Syslog ( runSyslogLoggingT )

main :: IO ()
main = runSyslogLoggingT (logDebugN "HELLO!")
@

-}
module Control.Monad.Logger.Syslog
       ( module System.Log.MonadLogger.Syslog )
       where

import System.Log.MonadLogger.Syslog
