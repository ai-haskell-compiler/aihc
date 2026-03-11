{-# LANGUAGE AllowAmbiguousTypes #-}  -- for ReifyRelOp
{-# LANGUAGE UndecidableInstances #-} -- for WidenRelOp

-- | Relational operator definitions.

module Rerefined.Predicate.Relational.Internal where

import GHC.TypeNats
import Data.Type.Ord ( OrdCond )
import GHC.TypeLits ( Symbol )

{- | Relational operator.

Constructor order is arbitrary due to @NEQ@, which obstructs ordering in a
meaningful way.

Note that these operators may be defined by combining 'Ordering's in certain
ways: for example @'LT' OR 'EQ'@ could be @LTE@, @'LT' OR 'GT'@ could be @NEQ@.
This is convenient for user intuition, permitting the use of e.g. 'LT' as a
relational operator directly. However, it complicates type-level work, as now we
can't restrict relational operators to a given kind, and we have to handle
non-standard orderings like @'GT' OR 'LT'@.
-}
data RelOp
  = RelOpEQ  -- ^ @==@              equal to
  | RelOpNEQ -- ^ @/=@ less than             or greater than (also not equal to)
  | RelOpLT  -- ^ @<@  less than
  | RelOpLTE -- ^ @<=@ less than or equal to
  | RelOpGTE -- ^ @>=@              equal to or greater than
  | RelOpGT  -- ^ @>@                           greater than

-- | Reify a 'RelOp'.
class ReifyRelOp (op :: RelOp) where
    -- | Pretty @op@.
    type ShowRelOp op :: Symbol

    -- | The term-level relational operator that @op :: 'RelOp'@ describes.
    reifyRelOp :: forall a. Ord a => a -> a -> Bool

instance ReifyRelOp RelOpLT  where
    type ShowRelOp RelOpLT  = "<"
    reifyRelOp = (<)

instance ReifyRelOp RelOpLTE where
    type ShowRelOp RelOpLTE = "<="
    reifyRelOp = (<=)

instance ReifyRelOp RelOpEQ  where
    type ShowRelOp RelOpEQ  = "=="
    reifyRelOp = (==)

instance ReifyRelOp RelOpNEQ where
    type ShowRelOp RelOpNEQ = "/="
    reifyRelOp = (/=)

instance ReifyRelOp RelOpGTE where
    type ShowRelOp RelOpGTE = ">="
    reifyRelOp = (>=)

instance ReifyRelOp RelOpGT  where
    type ShowRelOp RelOpGT  = ">"
    reifyRelOp = (>)

-- | Can we widen the given 'RelOp' on the given 'Natural' from @n@ to @m@?
type WidenRelOp :: RelOp -> Natural -> Natural -> Bool
type family WidenRelOp op n m where
    -- @n == m@? no problem
    WidenRelOp op  n n = True

    -- I'd love to simplify this, but 'CmpNat' is opaque.
    WidenRelOp RelOpLT  n m = OrdCond (CmpNat n m) True  True False
    WidenRelOp RelOpLTE n m = OrdCond (CmpNat n m) True  True False
    WidenRelOp RelOpGTE n m = OrdCond (CmpNat n m) False True True
    WidenRelOp RelOpGT  n m = OrdCond (CmpNat n m) False True True

    -- can't widen (==) or (/=)
    WidenRelOp op n m = False

-- | Flip a 'RelOp' to give the opposite comparison.
type FlipRelOp :: RelOp -> RelOp
type family FlipRelOp op where
    FlipRelOp RelOpEQ  = RelOpNEQ
    FlipRelOp RelOpNEQ = RelOpEQ
    FlipRelOp RelOpLT  = RelOpGTE
    FlipRelOp RelOpLTE = RelOpGT
    FlipRelOp RelOpGTE = RelOpLT
    FlipRelOp RelOpGT  = RelOpLTE
