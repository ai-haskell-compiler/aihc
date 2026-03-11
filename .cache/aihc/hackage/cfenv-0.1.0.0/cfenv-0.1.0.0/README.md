# Haskell CFEnv

[![Build Status](https://travis-ci.org/tomphp/haskell-cfenv.svg?branch=master)](https://travis-ci.org/tomphp/haskell-cfenv)

A port of [go-cfenv](https://github.com/cloudfoundry-community/go-cfenv) for
Haskell.

The purpose of this library is to assist you in writing Haskell apps that run
on Cloud Foundry. It provides convenience functions and structures that map to
Cloud Foundry environment variable primitives.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.String (fromString)
import Data.Monoid (mconcat)

import Web.Scotty

import qualified System.CloudFoundry.Environment as CfEnv

main = do
  app <- CfEnv.current
  
  scotty (CfEnv.port app) $
    get "/" $ do
      html $ mconcat ["<pre>", (fromString (show app)), "</pre>"] 
```

## Missing Functionality

- Find services by pattern matching
- Some cases from [go-cfenv](https://github.com/cloudfoundry-community/go-cfenv)
  around handling missing data

