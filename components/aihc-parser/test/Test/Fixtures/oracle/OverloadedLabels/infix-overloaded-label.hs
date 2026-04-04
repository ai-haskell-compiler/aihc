{- ORACLE_TEST xfail yggdrasil-schema infix overloaded label after operator -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}

module InfixOverloadedLabel where

import GHC.OverloadedLabels

data L = L

instance IsLabel "a" L where
  fromLabel = L

(^.) :: a -> L -> ()
(^.) = undefined

f :: ()
f = undefined ^. #a
