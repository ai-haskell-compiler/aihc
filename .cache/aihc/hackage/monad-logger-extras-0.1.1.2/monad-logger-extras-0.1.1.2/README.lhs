monad-logger-extras
===================
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/monad-logger-extras.svg)](https://hackage.haskell.org/package/monad-logger-extras) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/monad-logger-extras/badge)](https://matrix.hackage.haskell.org/#/package/monad-logger-extras) [![Github CI](https://github.com/obsidiansystems/monad-logger-extras/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/monad-logger-extras/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/monad-logger-extras/blob/master/LICENSE)

Composable logging, syslog integration, and more with [monad-logger](https://hackage.haskell.org/package/monad-logger).

Description
-----------

This package provides a way to compose logging actions so that you can conveniently log to multiple destinations. It also includes implementations of a few common logging actions: logging to stdout, stderr, nowhere (similar to `NoLoggingT`), and to a posix syslog (using [hsyslog](https://hackage.haskell.org/package/hsyslog)).

Logs can be emitted in color by using the `colorize` or `colorizeWith` function on your `Logger`.

This package also contains a couple of orphan instances for [`LoggingT`](https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:LoggingT): `MonadPlus` and `Alternative`.

Example usage
-------------

Watch for the system log message by running:

```bash
journalctl --user -t log-test -f
```

This example can be built and run using cabal (either `cabal repl example` or `cabal build example`).

```haskell

> {-# LANGUAGE OverloadedStrings #-}
> 
> import Control.Monad.Logger
> import Control.Monad.Logger.Extras
> 
> main :: IO ()
> main = do
>   let logger = colorize logToStdout <> logToStderr <> logToSyslog "log-test"
>   flip runLoggerLoggingT logger $ do
>     logInfoN "This is a test. You should see this on stdout, stderr, and in your system log."
>     logDebugN "This is a debug message."
>     logWarnN "This is a warning."
>     logErrorN "This is an error!"

```

This should produce output that looks like this (note that the stdout log has been `colorize`d):

![Example output](https://i.imgur.com/nkVAHrM.png)
