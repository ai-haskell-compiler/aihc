{-# LANGUAGE CPP #-}

module Amazon.SNS.Verify.ValidURI
  ( validScheme
  , validRegPattern
  , devRegPattern
  , prodRegPattern
  ) where

import Amazon.SNS.Verify.Prelude

_devScheme :: String
_devScheme = "http:"

_prodScheme :: String
_prodScheme = "https:"

{- FOURMOLU_DISABLE -}

validScheme :: String
validScheme =
#ifdef DEVELOPMENT
  _devScheme
#else
  _prodScheme
#endif

validRegPattern :: String
validRegPattern =
#ifdef DEVELOPMENT
  devRegPattern
#else
  prodRegPattern
#endif

{- FOURMOLU_ENABLE -}

devRegPattern :: String
devRegPattern = "^localhost$"

prodRegPattern :: String
prodRegPattern = "^sns\\.[a-zA-Z0-9\\-]{3,}\\.amazonaws\\.com(\\.cn)?$"
