{- ORACLE_TEST
id: lambda-case-in-application
category: expressions
expected: pass
reason: parser now supports multiline application around lambda-case
-}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseInApplication where

describeMany :: [Maybe Int] -> [String]
describeMany =
  map
    (\case
        Just n -> "just"
        Nothing -> "nothing"
    )
