{- ORACLE_TEST xfail multiline string with escaped quote and backslash-newline continuation -}
{-# LANGUAGE OverloadedStrings #-}

x = [
    (A, "\
\")
  , (B, "\
\c")
  ]
