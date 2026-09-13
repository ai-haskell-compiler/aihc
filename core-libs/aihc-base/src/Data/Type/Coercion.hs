{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | GHC's @repr :: (a :~: b) -> Coercion a b@ is absent: matching its
-- 'Data.Type.Equality.Refl' refines @b@ to @a@ only inside the solver, so the
-- body builds a @Coercible a a@ dictionary where the result type asks for
-- @Coercible a b@ and the FC lint rejects it. Restoring it needs the case
-- alternative to cast by the equality the match brings into scope.
module Data.Type.Coercion
  ( Coercion (..),
    coerceWith,
    gcoerceWith,
    sym,
    trans,
    TestCoercion (..),
  )
where

import Data.Coerce (Coercible, coerce)
import Data.Maybe (Maybe)

-- | Representational equality. A value of type @Coercion a b@ witnesses
-- that @a@ and @b@ have the same representation, and matching on it brings
-- the @Coercible a b@ evidence back into scope.
data Coercion a b where
  Coercion :: (Coercible a b) => Coercion a b

-- | Change the representation of a value along a witness.
coerceWith :: Coercion a b -> a -> b
coerceWith Coercion = coerce

-- | Make the evidence of a witness available to a computation.
gcoerceWith :: Coercion a b -> ((Coercible a b) => r) -> r
gcoerceWith Coercion result = result

-- | Representational equality is symmetric.
sym :: Coercion a b -> Coercion b a
sym Coercion = Coercion

-- | Representational equality is transitive.
trans :: Coercion a b -> Coercion b c -> Coercion a c
trans Coercion Coercion = Coercion

-- | Comparison of the type arguments of two values of the same container.
class TestCoercion f where
  testCoercion :: f a -> f b -> Maybe (Coercion a b)
