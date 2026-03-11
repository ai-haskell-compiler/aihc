{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.MQuery.Entry where

import Prettyprinter

data Entry k v = Entry k v

instance (Pretty k, Pretty v) => Pretty (Entry k v) where
  pretty (Entry k v) = pretty k <> ": " <> pretty v
