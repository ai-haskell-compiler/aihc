{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase, TemplateHaskellQuotes #-}

module THQuoteArrowType where

data Type = ArrowT | AppT Type Type

headOfType :: Type -> Name
headOfType = \case
  ArrowT -> ''(->)
  AppT t _ -> headOfType t
