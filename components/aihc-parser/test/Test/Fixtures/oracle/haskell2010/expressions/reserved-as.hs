{- ORACLE_TEST xfail reserved keyword as identifier -}
module ReservedKeywordAs where

reserved :: as
reserved = undefined

arg as = as
