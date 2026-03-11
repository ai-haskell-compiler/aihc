{-# LANGUAGE UndecidableInstances #-}

module TypeLevelShow.Utils where

import GHC.TypeLits
import Singleraeh.Symbol ( sConsSymbol )

-- | Surround the given 'Symbol' with parentheses when the 'Bool' parameter is
--   'True'.
type ShowParen :: Bool -> Symbol -> Symbol
type family ShowParen b p where
    ShowParen False p = p
    ShowParen True  p = ConsSymbol '(' p `AppendSymbol` ")"

-- | Surround the given 'Symbol' with parentheses if the given precedence is
--   greater than the given 'Natural'.
--
-- Type-level relational operations are a pain. This may be easier to use.
type ShowParenIfGt :: Natural -> Natural -> Symbol -> Symbol
type ShowParenIfGt n d p = ShowParen (OrderingGT (CmpNat d n)) p

type family OrderingGT (a :: Ordering) :: Bool where
    OrderingGT LT = False
    OrderingGT EQ = False
    OrderingGT GT = True

type ShowChar ch = ConsSymbol ch ""
sShowChar :: SChar ch -> SSymbol (ShowChar ch)
sShowChar ch = sConsSymbol ch symbolSing

type l ++ r = AppendSymbol l r
type l >  r = OrderingGT (CmpNat l r)
