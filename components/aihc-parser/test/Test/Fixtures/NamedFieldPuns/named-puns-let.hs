{- ORACLE_TEST
id: named-puns-let
category: expressions
expected: pass
reason: parser now supports NamedFieldPuns in let bindings
-}
{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsLet where

data User = User {userName :: String, userId :: Int}

render :: User -> String
render u =
  let User {userName, userId} = u
   in userName ++ "#" ++ show userId
