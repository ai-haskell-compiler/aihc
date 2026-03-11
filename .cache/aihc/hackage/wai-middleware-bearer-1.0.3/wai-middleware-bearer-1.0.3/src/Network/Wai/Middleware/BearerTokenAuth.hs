{-|
Module      : Network.Wai.Middleware.BearerTokenAuth
Description : Implements HTTP Bearer Token Authentication.
Copyright   : (c) Martin Bednar, 2022
License     : MIT
Maintainer  : bednam17@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Implements Bearer Token Authentication as a WAI 'Middleware'.

This module is based on 'Network.Wai.Middleware.HttpAuth'.

-}
{-# LANGUAGE OverloadedStrings #-}

-- The implementation is based on 'Network.Wai.Middleware.HttpAuth'.

module Network.Wai.Middleware.BearerTokenAuth
  ( -- * Middleware
    --
    -- | You can choose from three functions to use this middleware:
    --
    -- 1. 'tokenListAuth' is the simplest to use and accepts a list of valid tokens;
    --
    -- 2. 'tokenAuth' can be used to perform a more sophisticated validation of the accepted token (such as database lookup);
    --
    -- 3. 'tokenAuth'' is similar to 'tokenAuth', but it also passes the 'Request' to the validation function.
    tokenListAuth
  , tokenAuth
  , tokenAuth'
    -- * Token validation
  , TokenValidator
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.Word8 (isSpace, toLower)
import Network.HTTP.Types (hAuthorization, hContentType, status401)
import Network.Wai (Middleware, Request(requestHeaders), Response, responseLBS)

-- | Type synonym for validating a token 
type TokenValidator = ByteString -> IO Bool

-- | Perform token authentication
-- based on a list of allowed tokens.
--
-- > tokenListAuth ["secret1", "secret2"]
tokenListAuth :: [ByteString] -> Middleware
tokenListAuth tokens = tokenAuth (\tok -> return $ tok `elem` tokens)

-- | Performs token authentication.
--
-- If the token is accepted, leaves the Application unchanged.
-- Otherwise, sends a @401 Unauthorized@ HTTP response.
--
-- > tokenAuth (\tok -> return $ tok == "abcd" )
tokenAuth 
  :: TokenValidator -- ^ Function that determines whether the token is valid 
  -> Middleware
tokenAuth checker = tokenAuth' (const checker)

-- | Like 'tokenAuth', but also passes the 'Request' to the validator function.
--
tokenAuth' 
  :: (Request -> TokenValidator) -- ^ Function that determines whether the token is valid
  -> Middleware
tokenAuth' checkByReq app req sendRes = do
  let checker = checkByReq req
  let pass = app req sendRes
  authorized <- check checker req
  if authorized
    then pass -- Pass the Application on successful auth
    else sendRes rspUnauthorized -- Send a @401 Unauthorized@ response on failed auth

check :: TokenValidator -> Request -> IO Bool
check checkCreds req =
  case extractBearerFromRequest req of
    Nothing -> return False
    Just token -> checkCreds token

rspUnauthorized :: Response
rspUnauthorized =
  responseLBS
    status401
    [(hContentType, "text/plain"), ("WWW-Authenticate", "Bearer")]
    "Bearer token authentication is required"

extractBearerFromRequest :: Request -> Maybe ByteString
extractBearerFromRequest req = do
  authHeader <- lookup hAuthorization (requestHeaders req)
  extractBearerAuth authHeader

-- | Extract bearer authentication data from __Authorization__ header
-- value. Returns bearer token
--
-- Source: https://hackage.haskell.org/package/wai-extra-3.1.11/docs/Network-Wai-Middleware-HttpAuth.html
extractBearerAuth :: ByteString -> Maybe ByteString
extractBearerAuth bs =
  let (x, y) = S.break isSpace bs
   in if S.map toLower x == "bearer"
        then Just $ S.dropWhile isSpace y
        else Nothing
