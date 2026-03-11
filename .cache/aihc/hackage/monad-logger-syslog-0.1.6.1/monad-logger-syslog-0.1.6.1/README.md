# Monad Logger for Syslog

![TravisCI](https://travis-ci.org/fpco/monad-logger-syslog.svg)
![Hackage](https://img.shields.io/hackage/v/monad-logger-syslog.svg)

## Install

    cabal install monad-logger-syslog

## Usage (example)

### Default to syslog `user` facility with name `hslogger`

```haskell
import Control.Monad.Logger ( logDebugN  )
import Control.Monad.Logger.Syslog ( runSyslogLoggingT )

main :: IO ()
main = runSyslogLoggingT (logDebugN "HELLO!")
```

### Log under `Local1` facility with name `mylogger`

```haskell
import Control.Monad.Logger ( logDebugN  )
import Control.Monad.Logger.Syslog ( runCustomSyslogLoggingT )

main :: IO ()
main = runCustomSyslogLoggingT "mylogger" Local1 (logDebugN "HELLO!")
```
