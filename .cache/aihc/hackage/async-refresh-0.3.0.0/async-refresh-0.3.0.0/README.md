# async-refresh [![Hackage version](https://img.shields.io/hackage/v/async-refresh.svg?label=Hackage)](https://hackage.haskell.org/package/async-refresh) [![Stackage version](https://www.stackage.org/package/async-refresh/badge/lts?label=Stackage)](https://www.stackage.org/package/async-refresh) [![Build Status](https://travis-ci.org/mtesseract/async-refresh.svg?branch=master)](https://travis-ci.org/mtesseract/async-refresh)

### About

This is Haskell library implementing the logic for refreshing of
expiring data according to user-provided actions.

### Usage

- Create a new configuration using `newAsyncRefreshConf`, providing
  the action to be used for data refreshing.

- Adjust the configuration using the `asyncRefreshConfSet*` functions,
  in particular using `asyncRefreshConfSetCallback`.

- Use `newAsyncRefresh` to initiate a new thread managing the
  asynchronous refreshing.

### Example

The following IO action produces a `TVar` which is updated every ten
seconds to contain the current time (wrapped in an `Either
SomeException`, because refreshing may fail).

```
periodicTimeUpdater :: IO (TVar (Either SomeException UTCTime))
periodicTimeUpdater = runStderrLoggingT $ do
  timeStore <- liftIO $ newTVarIO (Left (toException NotFound))
  let conf = newAsyncRefreshConf (RefreshResult <$> liftIO getCurrentTime <*> pure Nothing)
        & asyncRefreshConfSetLabel "CurrentTime updated every 10 seconds"
        & asyncRefreshConfSetDefaultInterval (10 * 10^3)
        & asyncRefreshConfSetCallback (liftIO . atomically . writeTVar timeStore . fmap refreshResult)
  _ <- newAsyncRefresh conf
  return timeStore
```
