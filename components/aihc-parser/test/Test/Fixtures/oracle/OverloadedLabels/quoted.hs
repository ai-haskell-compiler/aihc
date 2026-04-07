{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
module OverloadedLabelsQuoted where

import GHC.OverloadedLabels (IsLabel (..))

data Label = Label

instance IsLabel "The quick brown fox" Label where
  fromLabel = Label

x :: Label
x = #"The quick brown fox"
