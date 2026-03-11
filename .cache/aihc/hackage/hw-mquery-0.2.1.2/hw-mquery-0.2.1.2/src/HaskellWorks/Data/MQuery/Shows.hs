{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Data.MQuery.Shows where

import HaskellWorks.Data.MQuery.Micro

showsVs :: Show a => [a] -> String -> String
showsVs (kv:kvs) = shows kv . foldl (.) id ((\jv -> (", " ++) . shows jv) `map` kvs)
showsVs []       = id

showKvs :: Show (Micro a) => [a] -> String -> String
showKvs (kv:kvs) = shows (Micro kv) . foldl (.) id ((\jv -> (", " ++) . shows (Micro jv)) `map` kvs)
showKvs []       = id
