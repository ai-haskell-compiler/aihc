{- ORACLE_TEST xfail reason="'pattern' treated as reserved keyword but should be valid identifier in Haskell2010 without PatternSynonyms" -}
module PatternAsIdentifier where

pattern :: Int -> Int
pattern x = x
