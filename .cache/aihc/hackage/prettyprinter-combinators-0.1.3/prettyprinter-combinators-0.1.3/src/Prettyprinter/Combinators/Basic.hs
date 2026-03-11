----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.Combinators.Basic
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Combinators.Basic
  ( (##)
  , ppListWithDelimSep
  ) where

import Data.Foldable
import Data.Semigroup as Semigroup
import Prettyprinter as PP

infixr 6 ##

(##) :: Doc ann -> Doc ann -> Doc ann
(##) x y = PP.nest 2 $ x Semigroup.<> PP.line <> y

{-# INLINABLE ppListWithDelimSep #-}
ppListWithDelimSep
  :: forall f ann. Foldable f
  => Doc ann
  -> Doc ann
  -> Doc ann
  -> f (Doc ann)
  -> Doc ann
ppListWithDelimSep separator left right xs =
  case toList xs of
    []   -> left <> right
    [y]  -> PP.flatAlt (left <+> y <+> right) (left <> y <> right)
    y:ys ->
      PP.align $
        PP.group $
          PP.flatAlt
            (left PP.<+> y <> PP.line' <>
             PP.vcat fields <> PP.line <>
             right)
            (left PP.<> y <> PP.vcat fields <> right)
      where
        fields :: [Doc ann]
        fields = fmap (\x -> separator PP.<+> x) ys

