{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Magic.Dict (WithDict (withDict)) where

import GHC.Types (Constraint, RuntimeRep, TYPE, Type)

-- | Supply a dictionary for a class with one method and no superclasses.
class WithDict (cls :: Constraint) (meth :: Type) where
  withDict :: forall {rr :: RuntimeRep} (r :: TYPE rr). meth -> ((cls) => r) -> r
