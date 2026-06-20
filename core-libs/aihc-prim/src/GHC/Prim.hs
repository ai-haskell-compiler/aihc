{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( catch#,
    raise#,
    State#,
    RealWorld,
    TYPE,
  )
where

import GHC.Types (TYPE)

data State# s

data RealWorld

raise# :: a -> b
raise# = raise#

catch# ::
  (State# RealWorld -> (# State# RealWorld, a #)) ->
  (b -> State# RealWorld -> (# State# RealWorld, a #)) ->
  State# RealWorld ->
  (# State# RealWorld, a #)
catch# = catch#
