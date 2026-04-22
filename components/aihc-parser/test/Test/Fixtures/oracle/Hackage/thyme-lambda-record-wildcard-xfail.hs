{- ORACLE_TEST xfail pretty-printer wraps Con{..} lambda patterns in extra parens causing roundtrip mismatch -}
{-# LANGUAGE RecordWildCards #-}
module ThymeLambdaRecordWildcard where

f = (\ TimeZone {..} -> undefined, \ TimeZone {..} -> undefined)
