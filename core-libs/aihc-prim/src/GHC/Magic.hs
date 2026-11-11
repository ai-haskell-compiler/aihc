{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions that GHC gives special treatment in its optimiser.
--
-- Here, each function is an ordinary function. The optimiser does not get
-- the hints that GHC gives.
module GHC.Magic
  ( inline,
    noinline,
    lazy,
    oneShot,
    runRW#,
  )
where

import GHC.Prim (runRW#)
import GHC.Types (RuntimeRep, TYPE)

-- | The value is returned unchanged. Inlining hints do not apply.
inline :: a -> a
inline value = value

-- | The value is returned unchanged. Inlining hints do not apply.
noinline :: a -> a
noinline value = value

-- | The value is returned unchanged. Strictness analysis does not apply.
lazy :: a -> a
lazy value = value

-- | The function is returned unchanged. Arity hints do not apply.
--
-- The argument and the result may have any runtime representation: a
-- worker that takes an @Int#@ index is the common caller.
oneShot :: forall (q :: RuntimeRep) (r :: RuntimeRep) (a :: TYPE q) (b :: TYPE r). (a -> b) -> a -> b
oneShot function = function
