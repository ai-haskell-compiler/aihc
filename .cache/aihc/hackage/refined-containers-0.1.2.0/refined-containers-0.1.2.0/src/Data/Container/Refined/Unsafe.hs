{-# LANGUAGE ImpredicativeTypes #-}
module Data.Container.Refined.Unsafe where

import Data.Container.Refined.Proofs
import Data.Type.Coercion
import Refined
import Refined.Unsafe


unsafeSubset :: p :-> q
unsafeSubset = reallyUnsafeRefine . unrefine
{-# INLINE unsafeSubset #-}

unsafeSubsetWith2 :: p' :-> q' -> p'' :-> q'' -> p :-> q
unsafeSubsetWith2 f g = reallyUnsafeRefine . unrefine
  . f . reallyUnsafeRefine . unrefine
  . g . reallyUnsafeRefine . unrefine
{-# INLINE unsafeSubsetWith2 #-}

-- Because `Refined p x` is a newtype over `x`, by parametricity a `p :-> q`
-- can either diverge or be `id`. This ensures that it does not diverge.
rnfProof :: (p :-> q) -> ()
rnfProof f = unrefine $ f $ reallyUnsafeRefine ()
{-# INLINE rnfProof #-}

-- | This function can be used to freely convert between @Element@ and @Key@
-- types of various flavors ('Regular', v'Int', 'Hashed'), corresponding to the
-- different implementations of sets and maps.
castFlavor
  :: forall (f :: Flavor) (g :: Flavor) s a. Coercion
    (Refined (InSet f s) a)
    (Refined (InSet g s) a)
castFlavor = sym (reallyUnsafeUnderlyingRefined @a @(InSet f s))
  `trans` reallyUnsafeUnderlyingRefined @a @(InSet g s)
{-# INLINE castFlavor #-}

castRefined
  :: forall a p q. (p :-> q)
  -> (q :-> p)
  -> Coercion (Refined p a) (Refined q a)
castRefined f g
  | () <- rnfProof f
  , () <- rnfProof g
  = sym (reallyUnsafeUnderlyingRefined @a @p)
    `trans` reallyUnsafeUnderlyingRefined @a @q
{-# INLINE castRefined #-}
