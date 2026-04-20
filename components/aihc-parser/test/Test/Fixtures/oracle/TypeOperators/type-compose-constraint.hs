{- ORACLE_TEST xfail parser rejects type operator applied to type arguments in class context -}
{-# LANGUAGE TypeOperators #-}

module TypeComposeConstraint where

import Data.Functor.Compose

class (f (g x)) => (f `Compose` g) x
