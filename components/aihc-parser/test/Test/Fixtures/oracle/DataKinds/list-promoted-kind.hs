{- ORACLE_TEST xfail list promoted kind -}
{-# LANGUAGE DataKinds #-}
module ListPromotedKind where

import Data.Proxy
fn :: Proxy (() ': '[])
fn = undefined
