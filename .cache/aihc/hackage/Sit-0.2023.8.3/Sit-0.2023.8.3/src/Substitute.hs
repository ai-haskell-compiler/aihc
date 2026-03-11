{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Substitution and weak head evaluation

module Substitute where

import Internal

import Impossible
#include "undefined.h"

-- | Substitutions are lists of terms.

type Subst = [Term]

-- | Weakening substitution @Γ.Δ ⊢ wkS |Δ| : Γ@

wkS :: Int -> Subst
wkS n = map (Var . Index) [n,n+1..]

-- | Identity substitution @Γ ⊢ idS : Γ@.

idS :: Subst
idS = wkS 0

-- | Composing substitution
--   @
--      Γ₁ ⊢ τ : Γ₂    Γ₂ ⊢ σ : Γ₃
--      -------------------------
--      Γ₁ ⊢ compS τ σ : Γ₃
--   @

compS :: Subst -> Subst -> Subst
compS = subst

-- | Extending a substitution
--   @
--      Γ ⊢ σ : Δ    Δ ⊢ T    Γ ⊢ t : Tσ
--      --------------------------------
--      Γ ⊢ consS t σ : Δ.T
--   @

consS :: Term -> Subst -> Subst
consS = (:)

-- | Lifting a substitution under a binder.
--   @
--      Γ ⊢ σ : Δ      Δ ⊢ T
--      --------------------
--      Γ.Tσ ⊢ liftS σ : Δ.T
--   @

liftS :: Subst -> Subst
liftS s = consS (Var 0) $ weakS s

-- | Weakening a substitution.
--
--   @
--     Γ ⊢ σ : Δ    Γ ⊢ T
--     ------------------
--     Γ.T ⊢ weakS σ : Δ
--   @

weakS :: Subst -> Subst
weakS = compS (wkS 1)

-- | Looking up an entry in a substitution.

lookupS :: Subst -> Index -> Term
lookupS s i =  s !! dbIndex i

-- | Substitution for various syntactic categories.

class Substitute a where
  subst :: Subst -> a -> a

instance Substitute a => Substitute [a] where
  subst s = map (subst s)

instance Substitute a => Substitute (Dom a) where
  subst s = fmap (subst s)

instance Substitute a => Substitute (Arg a) where
  subst s = fmap (subst s)

instance Substitute a => Substitute (Elim' a) where
  subst s = fmap (subst s)

instance Substitute Term where
  subst s = \case
    Type l  -> Type $ subst s l
    Size    -> Size
    Nat a   -> Nat $ subst s a
    Zero a  -> Zero $ subst s a
    Suc a t -> Suc (subst s a) $ subst s t
    Infty   -> Infty
    Pi u t  -> Pi (subst s u) $ subst s t
    Lam r t -> Lam r $ subst s t
    Var i   -> lookupS s i
    Def f   -> Def f
    App t u -> App (subst s t) (subst s u)

instance Substitute (Abs Term) where
  subst s (Abs   x t) = Abs   x $ subst (liftS s) t
  subst s (NoAbs x t) = NoAbs x $ subst s t

raise :: Substitute a => Int -> a -> a
raise n = subst (wkS n)


-- | Construct the type of the functional for fix.
--
--   @fixType t = .(i : Size) -> ((x : Nat i) -> T i x) -> (x : Nat (i + 1)) -> T (i + 1) x

fixType :: Term -> Term
fixType t =
  Pi (Dom Irrelevant Size) $ Abs "i" $
    Pi (Dom Relevant $ f $ Var 0) $ NoAbs "_" $
      f $ sSuc $ Var 0
  where
  f a = Pi (Dom Relevant (Nat a)) $ Abs "x" $
          raise 2 t
            `App` Apply (Arg ShapeIrr $ raise 1 a)
            `App` Apply (Arg Relevant $ Var 0)
