{- ORACLE_TEST pass -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module M where

import Data.Kind (Constraint)

type family All (cs :: [Constraint]) = (c :: Constraint) | c -> cs where
  All '[] = ()
