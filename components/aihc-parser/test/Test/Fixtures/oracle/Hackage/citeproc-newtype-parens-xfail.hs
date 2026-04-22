{- ORACLE_TEST xfail parser rejects parenthesised newtype constructor in newtype declaration -}
module CiteprocNewtypeParens where

newtype (ReferenceMap a) = ReferenceMap { unReferenceMap :: [(a)] }
  deriving (Show)
