{- ORACLE_TEST
id: named-puns-mixed
category: expressions
expected: pass
reason: parser now supports mixed NamedFieldPuns and explicit field bindings
-}
{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsMixed where

data Config = Config {host :: String, port :: Int, secure :: Bool}

normalize :: Config -> Config
normalize Config {host, port, secure = _} = Config {host, port, secure = True}
