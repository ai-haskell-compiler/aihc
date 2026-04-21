{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}

module TypeComposeConstraint where

import Data.Functor.Compose

class (f (g x)) => (f `Compose` g) x
