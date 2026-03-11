{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.C99.Util.Wrap
  ( Wrap
  , wrap
  ) where

import Language.C99.AST


{- Wraps only a single layer -}
class WrapStep a b | a -> b  where
  wrapstep :: a -> b

instance WrapStep Expr PrimExpr where
  wrapstep = PrimExpr . wrap

instance WrapStep PrimExpr PostfixExpr where
  wrapstep = PostfixPrim . wrap

instance WrapStep PostfixExpr UnaryExpr where
  wrapstep = UnaryPostfix . wrap

instance WrapStep UnaryExpr CastExpr where
  wrapstep = CastUnary . wrap

instance WrapStep CastExpr MultExpr where
  wrapstep = MultCast . wrap

instance WrapStep MultExpr AddExpr where
  wrapstep = AddMult . wrap

instance WrapStep AddExpr ShiftExpr where
  wrapstep = ShiftAdd . wrap

instance WrapStep ShiftExpr RelExpr where
  wrapstep = RelShift . wrap

instance WrapStep RelExpr EqExpr where
  wrapstep = EqRel . wrap

instance WrapStep EqExpr AndExpr where
  wrapstep = AndEq . wrap

instance WrapStep AndExpr XOrExpr where
  wrapstep = XOrAnd . wrap

instance WrapStep XOrExpr OrExpr where
  wrapstep = OrXOr . wrap

instance WrapStep OrExpr LAndExpr where
  wrapstep = LAndOr . wrap

instance WrapStep LAndExpr LOrExpr where
  wrapstep = LOrAnd . wrap

instance WrapStep LOrExpr CondExpr where
  wrapstep = CondLOr . wrap

instance WrapStep CondExpr AssignExpr where
  wrapstep = AssignCond . wrap

instance WrapStep AssignExpr Expr where
  wrapstep = ExprAssign . wrap


{- Wraps multiple layers -}
{- We write specific instances to help Haskell's type system. Using variables
   allows us to wrap _anything_, which will lead to inifite loops if no
   suitable instance is found.
-}
class Wrap a b where
  wrap    :: a -> b

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b PrimExpr)
  => Wrap a PrimExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b PostfixExpr)
  => Wrap a PostfixExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b UnaryExpr)
  => Wrap a UnaryExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b CastExpr)
  => Wrap a CastExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b MultExpr)
  => Wrap a MultExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b AddExpr)
  => Wrap a AddExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b ShiftExpr)
  => Wrap a ShiftExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b RelExpr)
  => Wrap a RelExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b EqExpr)
  => Wrap a EqExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b AndExpr)
  => Wrap a AndExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b OrExpr)
  => Wrap a OrExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b XOrExpr)
  => Wrap a XOrExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b LAndExpr)
  => Wrap a LAndExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b LOrExpr)
  => Wrap a LOrExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b CondExpr)
  => Wrap a CondExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b AssignExpr)
  => Wrap a AssignExpr where
    wrap = wrap . wrapstep

instance {-# OVERLAPPABLE #-} (WrapStep a b, Wrap b Expr)
  => Wrap a Expr where
    wrap = wrap . wrapstep


{- We provide specific identity instances as well, to eliminate unsolvable
   overlapping instances.
-}
instance {-# OVERLAPPABLE #-} Wrap PrimExpr PrimExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap PostfixExpr PostfixExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap UnaryExpr UnaryExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap CastExpr CastExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap MultExpr MultExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap AddExpr AddExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap ShiftExpr ShiftExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap RelExpr RelExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap EqExpr EqExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap AndExpr AndExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap OrExpr OrExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap XOrExpr XOrExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap LAndExpr LAndExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap LOrExpr LOrExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap CondExpr CondExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap AssignExpr AssignExpr where
  wrap = id

instance {-# OVERLAPPABLE #-} Wrap Expr Expr where
  wrap = id
