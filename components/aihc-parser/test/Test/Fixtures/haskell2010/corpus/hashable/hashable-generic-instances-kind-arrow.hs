{- ORACLE_TEST
id: hashable-generic-instances-kind-arrow
category: corpus
expected: pass
reason: from hashable/src/Data/Hashable/Generic/Instances.hs; parser now accepts multiline LANGUAGE pragma lists
-}
{-# LANGUAGE BangPatterns, FlexibleInstances, KindSignatures,
             ScopedTypeVariables, TypeOperators,
             MultiParamTypeClasses, GADTs, FlexibleContexts #-}
module X where

x :: Int
x = 1
