{- ORACLE_TEST xfail record wildcard in function pattern -}
{-# LANGUAGE RecordWildCards #-}
module FunctionPattern where

data Record = Record { a :: Int, b :: Double }
fn Record{..} = ()
