wai-transformers
================

Simple parameterization of Wai's `Application` and `Middleware` types.


## Overview

Wai's `Application` type is defined as follows:

```haskell
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
```

This is great for the server - we can just `flip ($)` the middlewares together to get
an effectful server. However, what if we want to encode useful information in our
middleware chain / end-point application? Something like a `ReaderT Env` environment,
where useful data like a universal salt, current hostname, or global mutable references can
be referenced later if it were wedged-in.

The design looks like this:

```haskell
type ApplicationT m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
```

Now we can encode MTL-style contexts with applications we write

```haskell
type MiddlewareT m = ApplicationT m -> ApplicationT m


data AuthConfig = AuthConfig
  { authFunction :: Request -> Either AuthException (Response -> Response)
  }

simpleAuth :: ( MonadReader AuthConfig m
              , MonadError AuthException m
              ) => MiddlewareT m
simpleAuth app req resp = do
  auth <- authFunction <$> ask
  case auth req of
    Left e = throwError e
    Right f = app req (resp . f)

simpleAuth' :: Middleware
simpleAuth' app req resp =
  eReceived <- runExceptT $ runReaderT (simpleAuth app req resp) myAuthConfig
  case eReceived of
    Left e = resp $ respondLBS "Unauthorized!"
    Right r = return r
```
