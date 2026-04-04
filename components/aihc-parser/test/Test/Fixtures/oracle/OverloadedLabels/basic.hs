{- ORACLE_TEST xfail proto-lens-protobuf-types overloaded label expression -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
module OverloadedLabelsBasic where

import GHC.OverloadedLabels (IsLabel (..))

data Label = Label

instance IsLabel "typeUrl" Label where
  fromLabel = Label

x :: Label
x = #typeUrl
