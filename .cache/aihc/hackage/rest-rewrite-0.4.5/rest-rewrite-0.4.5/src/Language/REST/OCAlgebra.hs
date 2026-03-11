{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.REST.OCAlgebra where

-- | The "Ordering Constraint Algebra", as described in section 4.2 of the paper.
--  @OCAlgebra c a m@ is an OCA with language of constraints @c@, applied to terms
--  of type @a@. @m@ is the computation context for @isSat@.
data OCAlgebra c a m = OCAlgebra
  { isSat  :: c -> m Bool       -- ^ Checks if the constraints are satisfiable
  , refine :: c -> a -> a -> c  -- ^ @refine c t u@ strengthens @c@ to permit @t >= u@
  , top    :: c                 -- ^ Initial constraints for use in REST

  , union  :: c -> c -> c       -- ^ Computes the union of constraints; used in 'ExploredTerms' as an optimization
                                --   A safe default implementation is @union c1 c2 = c2@

  , notStrongerThan :: c -> c -> m Bool -- ^ @c1 `notStrongerThan c2@ if @c1@ permits all orderings allowed by @c2@
                                        -- A safe default implementation is @notStrongerThan _ _ = return false@
  }

-- | @fuelOC n@ is an OCA that permits @n@ rewrite steps
fuelOC :: (Monad m) => Int -> OCAlgebra Int a m
fuelOC initFuel = OCAlgebra isSat' refine' initFuel union' notStrongerThan'
  where
    isSat'  c             = return $ c >= 0
    refine' c _ _         = c - 1
    union'                = max
    notStrongerThan' c c' = return $ c >= c'

-- | @contramap f oca@ transforms an OCA of terms of type @a@ terms of type @b@,
--   by using @f@ to convert terms of @b@ to equivalent ones of @a@
contramap :: forall c a b m .
     (b -> a)
  -> OCAlgebra c a m
  -> OCAlgebra c b m
contramap f oca = oca{refine = refine'}
  where
    refine' :: c -> b -> b -> c
    refine' c t1 t2 = refine oca c (f t1) (f t2)

-- | @bimapConstraints to from oca@ yields an oca using @d@ to track constraints; @to@ and @from@ should
--   define an isomorphism between c and d
bimapConstraints :: forall c d a m .
     (c -> d)
  -> (d -> c)
  -> OCAlgebra c a m
  -> OCAlgebra d a m
bimapConstraints to from oca = OCAlgebra isSat' refine' (to (top oca)) union' notStrongerThan'
  where
    isSat' :: d -> m Bool
    isSat' c = isSat oca (from c)

    refine' :: d -> a -> a -> d
    refine' c t1 t2 = to $ refine oca (from c) t1 t2

    union' :: d -> d -> d
    union' c1 c2 = to $ union oca (from c1) (from c2)

    notStrongerThan' :: d -> d -> m Bool
    notStrongerThan' c1 c2 = notStrongerThan oca (from c1) (from c2)
