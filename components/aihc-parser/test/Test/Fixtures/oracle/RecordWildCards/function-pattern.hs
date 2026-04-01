{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
module FunctionPattern where

data Record = Record { a :: Int, b :: Double }
fn Record{..} = ()
