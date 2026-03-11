{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Base
-- Description: Base functor for Egison pattern expression
-- Stability:   experimental
--
-- This module defines a base functor of 'Expr' that is useful to abstract recursive computations on 'Expr'.

module Language.Egison.Syntax.Pattern.Base
  ( ExprF(..)
  )
where

import           Data.Functor.Foldable.TH       ( makeBaseFunctor )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )


-- | Base functor of 'Expr'.
$(makeBaseFunctor ''Expr)
