{- ORACLE_TEST pass -}
-- From GHC testsuite/tests/layout/layout006.hs.
module M where

-- GHC's GHC.Parser.PostProcess had a piece of code like this

f :: IO ()
f
 | True = do
 let x = ()
     y = ()
 return ()
 | True = return ()
