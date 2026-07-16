{- ORACLE_TEST pass -}
-- From GHC testsuite/tests/layout/layout007.hs.
{-# LANGUAGE TemplateHaskell #-}

module M where

-- The paren here closes the open-splice - it doesn't match an
-- opening paren

f :: IO ()
f = do print $( [| 'a' |] )
