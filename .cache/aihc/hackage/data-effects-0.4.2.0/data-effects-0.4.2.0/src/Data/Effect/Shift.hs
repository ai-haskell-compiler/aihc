{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Shift where

import Control.Monad ((<=<))
import Data.Effect (CC (Jump, SubFork))
import Data.Function (fix)
import Data.Functor.Contravariant (Op (Op))

data Shift ans ref :: Effect where
    SubShiftFork :: Shift ans ref f (Either (ref a) a)
    Call :: ref a -> a -> Shift ans ref f ans
    Abort :: ans -> Shift ans ref f a
makeEffectF ''Shift

subShift
    :: forall a b es ans ref ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => (ref a -> Eff ff es b)
    -> (a -> Eff ff es b)
    -> Eff ff es b
subShift p q = subShiftFork >>= either p q
{-# INLINE subShift #-}

shift
    :: forall a es ans ref ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => ((a -> Eff ff es ans) -> Eff ff es ans)
    -> Eff ff es a
shift f = subShift (abort <=< f . call) pure
{-# INLINE shift #-}

getShiftCC
    :: forall es ans ref ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es (Eff ff es ans)
getShiftCC = shift fix
{-# INLINE getShiftCC #-}

runCCAsShift
    :: forall a es ans ref ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (CC ref ': es) a
    -> Eff ff es a
runCCAsShift = interpret \case
    SubFork -> subShiftFork
    Jump ref x -> call ref x >>= abort
{-# INLINE runCCAsShift #-}

runCCOnShift
    :: forall a es ans ref ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (CC (Op (Eff ff es ans)) ': es) a
    -> Eff ff es a
runCCOnShift = interpret \case
    SubFork -> shift \exit -> exit . Left . Op $ exit . Right
    Jump (Op exit) x -> exit x >>= abort
{-# INLINE runCCOnShift #-}
