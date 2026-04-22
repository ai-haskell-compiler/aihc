{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module StandaloneDerivingInfixPromotedLHS where

class a :+ b

deriving instance ((C) '()) :+ ()
