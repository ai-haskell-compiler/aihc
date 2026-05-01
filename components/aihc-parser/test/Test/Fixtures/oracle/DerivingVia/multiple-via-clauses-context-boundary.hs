{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeOperators #-}
module MultipleViaClausesContextBoundary where

class (a :: *) :+ (b :: Symbol)

data C a
  deriving () via *
  deriving () via (:+) => 'm'
