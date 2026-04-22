{- ORACLE_TEST pass -}
module CiteprocNewtypeParens where

newtype (ReferenceMap a) = ReferenceMap { unReferenceMap :: [(a)] }
  deriving (Show)
