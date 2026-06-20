{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( catch#,
    raise#,
    State#,
    RealWorld,
    TYPE,
  )
where

import "ghc-prim" GHC.Prim qualified as Prim

type State# = Prim.State#

type RealWorld = Prim.RealWorld

type TYPE = Prim.TYPE

raise# :: a -> b
raise# = Prim.raise#

catch# ::
  (State# RealWorld -> (# State# RealWorld, a #)) ->
  (b -> State# RealWorld -> (# State# RealWorld, a #)) ->
  State# RealWorld ->
  (# State# RealWorld, a #)
catch# = Prim.catch#
