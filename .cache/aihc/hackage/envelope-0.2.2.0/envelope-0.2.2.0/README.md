
Web.Envelope
============

[![Build Status](https://secure.travis-ci.org/cdepillabout/envelope.svg)](http://travis-ci.org/cdepillabout/envelope)
[![Hackage](https://img.shields.io/hackage/v/envelope.svg)](https://hackage.haskell.org/package/envelope)
[![Stackage LTS](http://stackage.org/package/envelope/badge/lts)](http://stackage.org/lts/package/envelope)
[![Stackage Nightly](http://stackage.org/package/envelope/badge/nightly)](http://stackage.org/nightly/package/envelope)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

This module exports an `Envelope` type that can be used to wrap reponses from a JSON REST API.  It provides a successful `Success` response, and a failure `Err` response.

Here is a small demonstration of returning a success response:

```haskell
>>> import qualified Data.ByteString.Lazy.Char8 as C8
>>> import Data.Aeson (decode, encode)
>>> let successEnvelope = toSuccessEnvelope 3 :: Envelope Text Int
>>> C8.putStrLn $ encode successEnvelope
{"data":3}
>>> decode "{\"data\":3}" :: Maybe (Envelope Text Int)
Just (EnvelopeSuccess (Success 3))
```

Your data gets wrapped in an object with a single `"data"` field:

```json
{
    "data": 3
}
```

Now lets look at how an error response is encoded and decoded.  It is encoded
as an object with two members: @\"extra\"@ and @\"error\"@.

```haskell
>>> let errorEnvelope = toErrEnvelope "DB_ERROR" "there was an error in the database" :: Envelope String Int
>>> C8.putStrLn $ encode errorEnvelope
{"extra":"there was an error in the database","error":"DB_ERROR"}
>>> decode "{\"extra\":\"there was an error in the database\",\"error\":\"DB_ERROR\"}" :: Maybe (Envelope String Int)
Just (EnvelopeErr (Err {errErr = "DB_ERROR", errExtra = Just "there was an error in the database"}))
```

Your error type and extra message get wrapped in an object:

```json
{
    "extra": "there was an error in the database",
    "error": "DB_ERROR"
}
```
