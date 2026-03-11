# om-logging

This package provides various combinators for composing loggers for use
with the [monad-logger](https://hackage.haskell.org/package/monad-logger)
package.

It also provides an opinion about what a good log message looks like.

For instance, the opinionated "standard" log format is defined using
the other combinators:

```haskell
{- |
  Log to the indicated destination, applying the "standard" filters
  and formats.
-}
withStandardFormat
  :: LogLevel {- ^ The minimum log level that will be logged. -}
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ The base logger. -}
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
withStandardFormat logLevel =
  filterLogging (levelFilter logLevel)
  . withPrefix ": "
  . withThread
  . withPackage
  . withLevel
  . withTime
```
