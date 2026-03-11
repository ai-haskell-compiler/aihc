{-# language AllowAmbiguousTypes  #-}
{-# language ConstraintKinds      #-}
{-# language DataKinds            #-}
{-# language GADTs                #-}
{-# language ImpredicativeTypes   #-}
{-# language PolyKinds            #-}
{-# language ScopedTypeVariables  #-}
{-# language TypeApplications     #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}
-- | 'Atom's represent the shape of a type, possibly with variables,
-- which can be 'Interpret'ed given a list of types.
module Data.PolyKinded.Atom (
  -- * Atoms
  Atom(..), (:$:), (:~:), (:~~:)
  -- * Type variables
, TyVar(..)
  -- ** Aliases for specific variables
, Var0, Var1, Var2, Var3, Var4, Var5, Var6, Var7, Var8, Var9
  -- * Intepretation
, Interpret, InterpretVar, Satisfies, ContainsTyVar
  -- ** Auxiliary types for interpretation
, ForAllI(..), SuchThatI(..)
  -- ** Records a type as constructor + list of variables
, WrappedI(..), toWrappedI, fromWrappedI
) where

import           Data.Kind
import           Data.PolyKinded
import           GHC.Exts
import           Fcf.Core (Exp, Eval)

-- $setup
-- >>> :set -XTypeOperators -XDataKinds -XNoStarIsType
-- >>> import Data.PolyKinded

-- | Well-scoped de Bruijn representation of type variables.
-- @TyVar d@ represents all the possible type variables which
-- can refer to the holes in kind @d@.
--
-- We recommend using the aliases 'Var0', 'Var1', ...
-- instead of the constructors, for further clarity.
data TyVar (d :: Type) (k :: TYPE r) where
  -- | First hole in @d@.
  VZ :: TyVar (x -> xs) x
  -- | Successor hole, increases the hole reference by 1.
  VS :: TyVar xs k -> TyVar (x -> xs) k

type Var0 = 'Var 'VZ
type Var1 = 'Var ('VS 'VZ)
type Var2 = 'Var ('VS ('VS 'VZ))
type Var3 = 'Var ('VS ('VS ('VS 'VZ)))
type Var4 = 'Var ('VS ('VS ('VS ('VS 'VZ))))
type Var5 = 'Var ('VS ('VS ('VS ('VS ('VS 'VZ)))))
type Var6 = 'Var ('VS ('VS ('VS ('VS ('VS ('VS 'VZ))))))
type Var7 = 'Var ('VS ('VS ('VS ('VS ('VS ('VS ('VS 'VZ)))))))
type Var8 = 'Var ('VS ('VS ('VS ('VS ('VS ('VS ('VS ('VS 'VZ))))))))
type Var9 = 'Var ('VS ('VS ('VS ('VS ('VS ('VS ('VS ('VS ('VS 'VZ)))))))))

infixr 5 :&:
infixr 5 :=>>:
-- | Shape of a type, possibly with type variables.
--
-- >>> -- the type [a] for unknown a
-- >>> :kind Kon [] :@: Var0
-- Kon [] :@: Var0 :: Atom (Type -> xs) Type
--
-- === __Representation of type families__
--
-- Type families are represented using
-- <https://hackage.haskell.org/package/first-class-families first-class-families>.
--
-- For example, the type-level @n + m :: 'GHC.TypeNats.Nat'@-- may expand to the following--
--
-- @
-- n + m         -- using @('GHC.TypeNats.+')@ from "GHC.TypeNats"
-- ~
-- 'Fcf.Core.Eval' (n 'Fcf.Data.Nat.+' m)  -- using 'Fcf.Core.Eval' from "Fcf.Core" and @('Fcf.Data.Nat.+')@ from "Fcf.Data.Nat"
-- @
--
-- which may be encoded as the following 'Atom' (using 'Var0' for @n@ and 'Var1' for @m@):
--
-- @
-- 'Data.PolyKinded.Atom.Eval' (('Kon' ('Fcf.Data.Nat.+') ':@:' 'Var0') ':@:' 'Var1')  -- 'Data.PolyKinded.Atom.Eval' as 'Atom'\'s constructor
--   :: 'Atom' (Nat -> Nat -> Type) Nat
-- @
--
-- <https://hackage.haskell.org/package/kind-generics kind-generics>
-- uses a different, more systematic encoding of type families for @GenericK@ instances;
-- see <https://hackage.haskell.org/package/fcf-family fcf-family> for more details.
-- For example, @n + m@ is instead expanded to the following:
--
-- @
-- n + m
-- ~
-- 'Fcf.Core.Eval' ('Fcf.Family.NDFamily' ('Fcf.Family.MkName' "base" \"GHC.TypeNats\" \"+\") 'Fcf.Family.P0' \'(n, \'(m, \'())))
-- @
--
-- which gets encoded as the following 'Atom':
--
-- @
-- 'Data.PolyKinded.Atom.Eval' ('Kon' ('Fcf.Family.NDFamily' ('Fcf.Family.MkName' "base" \"GHC.TypeNats\" \"+\") 'Fcf.Family.P0')
--         ':@:' (('Kon' \'(,) ':@:' 'Var0') ':@:' (('Kon' \'(,) ':@:' 'Var1') ':@:' 'Kon' \'())))
--   :: 'Atom' (Nat -> Nat -> Type) Nat
-- @
data Atom (d :: Type) (k :: TYPE r) where
  -- | Represents a type variable.
  Var :: TyVar d k -> Atom d k
  -- | Represents a constant type, like 'Int'.
  Kon :: k         -> Atom d k
  -- | Represents type application.
  (:@:) :: Atom d (k1 -> k2) -> Atom d k1 -> Atom d k2
  -- | Represents the conjunction of two constraints.
  (:&:) :: Atom d Constraint -> Atom d Constraint -> Atom d Constraint
  -- | Represents universal quantification.
  ForAll  :: Atom (d1 -> d) Type -> Atom d Type
  -- | Represents constraint requirement, the "thick arrow" @=>@.
  (:=>>:) :: Atom d Constraint -> Atom d Type -> Atom d Type
  -- | Represents a type family application.
  Eval :: Atom d (Exp k) -> Atom d k

-- | Represents an applied constructor.
-- Instead of @Kon [] :@: Var0$ you can write @[] :$: Var0$.
type f :$:  x = 'Kon f ':@: x
-- | Represents (homogeneous) type equality.
type a :~:  b = 'Kon (~) ':@: a ':@: b
-- | Represents heterogeneous type equality.
type a :~~: b = 'Kon (~~) ':@: a ':@: b

-- | Obtains the type in the list @tys@ referenced
-- by the type variable @t@.
--
-- >>> :kind! Interpret Var0 (LoT2 Int Bool)
-- Interpret Var0 (LoT2 Int Bool) :: Type
-- = Int
-- >>> :kind! Interpret Var1 (LoT2 Int Bool)
-- Interpret Var1 (LoT2 Int Bool) :: Type
-- = Bool
type family InterpretVar (t :: TyVar d k) (tys :: LoT d) :: k where
  InterpretVar 'VZ     tys = HeadLoT tys
  InterpretVar ('VS v) tys = InterpretVar v (TailLoT tys)

-- | Replaces the holes in the 'Atom' @t@ by the elements of
-- the list of types @tys@. The amount and kind of types in @tys@
-- must match statically those required by the 'Atom'.
--
-- >>> :kind! Interpret ([] :$: Var0) (LoT1 Int)
-- Interpret ([] :$: Var0) (LoT1 Int) :: Type
-- = [Int]
type family Interpret (t :: Atom d k) (tys :: LoT d) :: k where
  Interpret ('Var v)     tys = InterpretVar v tys
  Interpret ('Kon t)     tys = t
  Interpret (f ':@: x)   tys = (Interpret f tys) (Interpret x tys)
  Interpret (c ':&: d)   tys = (Interpret c tys, Interpret d tys)
  Interpret ('ForAll f)  tys = ForAllI f tys
  Interpret (c ':=>>: f) tys = SuchThatI c f tys
  Interpret ('Eval f)    tys = Eval (Interpret f tys)

-- | Auxiliary type for interpretation of the 'ForAll' atom.
-- Required because a type family like 'Interpret' cannot return
-- a polymorphic type.
newtype ForAllI (f :: Atom (d1 -> d) Type) (tys :: LoT d) where
  ForAllI :: (forall t. Interpret f (t ':&&: tys)) -> ForAllI f tys

-- | Records a value of type @f@ applied to the list @tys@.
--
-- >>> :t WrapI [1] :: WrappedI ([] :$: Var0) (LoT1 Int)
-- WrapI [1] :: WrappedI ([] :$: Var0) (LoT1 Int)
--   :: WrappedI ([] :$: Var0) (LoT1 Int)
newtype WrappedI (f :: Atom d Type) (tys :: LoT d) =
  WrapI { unwrapI :: Interpret f tys }

toWrappedI :: forall f tys t. ForAllI f tys -> WrappedI f (t ':&&: tys)
toWrappedI (ForAllI x) = WrapI (x @t)

fromWrappedI :: forall f tys. (forall t. WrappedI f (t ':&&: tys)) -> ForAllI f tys
fromWrappedI = coerce @(forall t. WrappedI f (t ':&&: tys))

-- | Auxiliary type for interpretation of the '(:=>>:)' atom.
-- Required because a type family like 'Interpret' cannot return
-- a type with constraints.
newtype SuchThatI (c :: Atom d Constraint) (f :: Atom d Type) (tys :: LoT d) where
  SuchThatI :: { unSuchThatI :: Interpret c tys => Interpret f tys } -> SuchThatI c f tys

-- | Interprets a list of 'Atom' representing constraints
-- into the actual constraints. This is a specialization of
-- 'Interpret' for the case of constraints.
--
-- >>> :kind! Satisfies '[Eq :$: Var0, Show :$: Var0] (LoT1 Int)
-- Satisfies '[Eq :$: Var0, Show :$: Var0] (LoT1 Int) :: Constraint
-- = (Eq Int, (Show Int, () :: Constraint))
type family Satisfies (cs :: [Atom d Constraint]) (tys :: LoT d) :: Constraint where
  Satisfies '[]       tys = ()
  Satisfies (c ': cs) tys = (Interpret c tys, Satisfies cs tys)

-- | Determines whether a given type variable @v@ is used within an 'Atom' @t@.
-- If not, we know that the atom is constant with respect to that variable.
type family ContainsTyVar (v :: TyVar d k) (t :: Atom d p) :: Bool where
  ContainsTyVar v ('Var v)     = 'True
  ContainsTyVar v ('Var w)     = 'False
  ContainsTyVar v ('Kon t)     = 'False
  ContainsTyVar v (f ':@: x)   = Or (ContainsTyVar v f) (ContainsTyVar v x)
  ContainsTyVar v (x ':&: y)   = Or (ContainsTyVar v x) (ContainsTyVar v y)
  ContainsTyVar v (c ':=>>: f) = Or (ContainsTyVar v c) (ContainsTyVar v f)
  ContainsTyVar v ('ForAll f)  = ContainsTyVar ('VS v) f

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True  thing  = 'True
  Or thing 'True   = 'True
  Or 'False 'False = 'False
