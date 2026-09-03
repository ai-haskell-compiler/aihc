module Data.Coerce (coerce) where

import Unsafe.Coerce (unsafeCoerce)

-- | Change the type of a value that has the same runtime representation.
-- The standin has no @Coercible@ solver, so the type gives no proof that the
-- two representations agree. The caller must keep that property.
coerce :: a -> b
coerce = unsafeCoerce
