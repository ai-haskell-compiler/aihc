{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
module ThymeLambdaRecordWildcard where

f = (\ TimeZone {..} -> undefined, \ TimeZone {..} -> undefined)
