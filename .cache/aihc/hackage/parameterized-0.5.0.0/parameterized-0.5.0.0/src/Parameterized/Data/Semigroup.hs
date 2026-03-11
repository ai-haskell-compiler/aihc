{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Parameterized.Data.Semigroup where

import Data.Kind

type family PNullary (n :: k -> Type) (t :: k) = (r :: Type) | r -> n t

-- | Parameterized version of (<>) in Semigroup
-- If used in conjunction with 'Parameterized.Data.Empty', ie as a parameterized Monoid,
-- then the instance should follow the following laws:
--  * @pmempty' `pmappend'` x = x@
--  * @x `pmappend'` pempty' = x@
--  * @x `pmappend'` (y `pmappend'` z) = (x `pmappend'` y) `pmappend'` z@
class PSemigroup (n :: k -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    pmappend :: PNullary n t -> PNullary n u -> PNullary n v

(&<>) :: (PSemigroup n t u v) => PNullary n t -> PNullary n u -> PNullary n v
(&<>) = pmappend
infixr 6 &<>
infixr 6 `pmappend`
