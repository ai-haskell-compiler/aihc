{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module DoEmptyLetExpression where

x = do let in ()
