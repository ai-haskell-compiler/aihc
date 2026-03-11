{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.MQuery.Row where

import Prettyprinter

import qualified Data.DList as DL

type MaxChars = Int

data Row a = Row MaxChars a

instance Pretty a => Pretty (Row (DL.DList a)) where
  -- TODO implement max chars for Row
  pretty (Row _ xs) = vcat (("==> " <>) `map` prettyRows)
    where prettyRows :: [Doc ann]
          prettyRows = fmap pretty $ DL.toList xs

prettyRowOfString :: Show a => Row (DL.DList a) -> Doc ann
prettyRowOfString (Row _ xs) = vcat (("==> " <>) `map` prettyRows)
  where prettyRows :: [Doc ann]
        prettyRows = (pretty . show) `map` DL.toList xs
