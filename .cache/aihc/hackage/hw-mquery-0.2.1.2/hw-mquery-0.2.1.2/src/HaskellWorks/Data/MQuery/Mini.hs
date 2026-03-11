{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.MQuery.Mini where

import HaskellWorks.Data.MQuery.AtLeastSize
import HaskellWorks.Data.MQuery.Micro
import Prettyprinter

import qualified Data.DList as DL

newtype Mini a = Mini a

instance Pretty (Micro a) => Pretty (Mini [a]) where
  pretty (Mini xs) | xs `atLeastSize` 11  = "[" <> nest 2 (prettyVs (take 10 (Micro `map` xs))) <> ", ..]"
  pretty (Mini xs) | xs `atLeastSize` 1   = "[" <> nest 2 (prettyVs (take 10 (Micro `map` xs))) <> "]"
  pretty (Mini _ ) = "[]"

instance Pretty (Mini a) => Pretty (Mini (DL.DList a)) where
  pretty (Mini xs) = vcat (punctuate "," ((pretty . Mini) `map` take 10 (DL.toList xs)))
