{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module InfixFunlhsThSplicePattern where

$splice `fn` () = ()
