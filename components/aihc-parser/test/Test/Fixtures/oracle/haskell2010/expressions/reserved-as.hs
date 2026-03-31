{- ORACLE_TEST xfail reserved keyword used as parameter -}
module ReservedKeywordAs where

reserved :: Int
reserved = 42

arg as = as
