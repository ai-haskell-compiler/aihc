# async-refresh-tokens [![Hackage version](https://img.shields.io/hackage/v/async-refresh-tokens.svg?label=Hackage)](https://hackage.haskell.org/package/async-refresh-tokens) [![Stackage version](https://www.stackage.org/package/async-refresh-tokens/badge/lts?label=Stackage)](https://www.stackage.org/package/async-refresh-tokens) [![Build Status](https://travis-ci.org/mtesseract/async-refresh-tokens.svg?branch=master)](https://travis-ci.org/mtesseract/async-refresh-tokens)

### About

This is Haskell library built on top of the async-refresh package
implementing the logic for refreshing of expiring access tokens.

### Usage

- Create new token types.

- Make the tokens be instances of the `IsToken` type classes by
  defining the `tokenScopes` method and (optionally) `tokenName` (a
  human readable label for this token).

- Use `newEmptyTokenStore` to create a new token stores (token stores
  are basically `TVar`s containing the tokens wrapped in `Either
  SomeException`).

- Create a new configuration by adjusting `defaultTokenConf` using the
  functions `tokenConfAddRequest` and `tokenConfSetFactor`. The
  function `tokenConfAddRequest` expects values of type `RequestToken`
  — these values encapsulate the token stores together with a
  token-refreshing action.

- Use `newTokenRefresher` to initiate token refreshing for each
  registered token refreshing request.

- To use the current token, extract it from the `TVar` using
  `readTVar` (and pattern matching on `Right`).

### Example

```
{-# LANGUAGE OverloadedStrings   #-}

data TokenFoo

instance IsToken TokenFoo where
  tokenScopes _ = ["foo.read", "foo.write"]

createTokenStoreFoo :: LoggingT IO (TokenStore TokenFoo)
createTokenStoreFoo = do
  tokenFoo <- newEmptyTokenStore (Proxy :: Proxy TokenFoo)
  let conf = defaultTokenConf
             & tokenConfAddRequest (RequestToken tokenFoo actionFoo)
  _ <- newTokenRefresher conf
  return tokenFoo

  where actionFoo :: (MonadIO m, IsToken t) => m (RefreshResult (Token t))
        actionFoo =
          return $ RefreshResult (Token "secret-foo-token") Nothing
```
