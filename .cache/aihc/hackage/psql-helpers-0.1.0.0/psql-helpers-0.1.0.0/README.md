# psql-helpers

[![Build Status](https://travis-ci.org/agrafix/psql-helpers.svg)](https://travis-ci.org/agrafix/psql-helpers)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/psql-helpers.svg)](http://packdeps.haskellers.com/reverse/psql-helpers)

A small collection of helper functions to generate PostgreSQL queries

## Examples

### Insert

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField

foo :: Connection -> IO ()
foo conn = insert "foo_table" ["bar" @= 5, "baz" @= True]
```
