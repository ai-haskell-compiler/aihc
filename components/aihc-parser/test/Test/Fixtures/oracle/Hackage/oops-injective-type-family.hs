{- ORACLE_TEST xfail reason="injective type families with result kind signatures stop at the equals sign" -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module M where

import Data.Kind (Constraint)

type family All (cs :: [Constraint]) = (c :: Constraint) | c -> cs where
  All '[] = ()
