{- ORACLE_TEST pass -}
module M where

data T = forall (a :: T). C a
