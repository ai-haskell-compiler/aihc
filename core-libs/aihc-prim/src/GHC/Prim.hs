{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( catch#,
    compareInt#,
    raise#,
    State#,
    RealWorld,
    TYPE,
  )
where

import GHC.Types (TYPE)

data State# s

data RealWorld

foreign import prim raise# :: a -> b

foreign import prim compareInt# :: Int# -> Int# -> Int#

foreign import prim
  catch# ::
    (State# RealWorld -> (# State# RealWorld, a #)) ->
    (b -> State# RealWorld -> (# State# RealWorld, a #)) ->
    State# RealWorld ->
    (# State# RealWorld, a #)
