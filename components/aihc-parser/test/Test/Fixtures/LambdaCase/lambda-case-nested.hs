{- ORACLE_TEST
id: lambda-case-nested
category: expressions
expected: pass
reason: parser now supports multiline application around lambda-case
-}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseNested where

choose :: Either Int String -> Int
choose = \case
  Left n -> n
  Right _ ->
    (\case
        Just k -> k
        Nothing -> 0
    )
      (Just 1)
