{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
module ListPromotedKind where

import Data.Proxy
fn :: Proxy (() ': '[])
fn = undefined
