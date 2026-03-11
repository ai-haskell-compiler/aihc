{-# LANGUAGE UndecidableInstances #-} -- due to type class design
{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to type class design

module Generic.Data.Function.Contra.Constructor where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

import GHC.Generics
import Data.Kind ( type Type, type Constraint )

import Generic.Data.Wrappers ( NoRec0, type ENoRec0, EmptyRec0 )
import GHC.TypeLits ( TypeError )

-- | TODO
--
-- The type variable is uninstantiated, used purely as a tag.
-- Good types include the type class used inside (providing you define the
-- type class/it's not an orphan instance), or a custom void data type.
-- See the binrep library on Hackage for an example.
class GenericContra tag where
    type GenericContraF tag :: Type -> Type
    type GenericContraC tag a :: Constraint
    genericContraF :: GenericContraC tag a => GenericContraF tag a

-- | over types with no fields in any constructor
instance GenericContra (NoRec0 (f :: Type -> Type)) where
    type GenericContraF (NoRec0 f) = f
    type GenericContraC (NoRec0 f) _ = TypeError ENoRec0
    genericContraF = undefined

-- | over types where all fields map to 'mempty'
instance GenericContra (EmptyRec0 (f :: Type -> Type)) where
    type GenericContraF (EmptyRec0 f) = f
    type GenericContraC (EmptyRec0 f) _ = Divisible f
    genericContraF = conquer

class GContraC tag gf where gContraC :: GenericContraF tag (gf p)

instance
  ( Divisible (GenericContraF tag)
  , GContraC tag l, GContraC tag r
  ) => GContraC tag (l :*: r) where
    gContraC = divide (\(l :*: r) -> (l, r)) (gContraC @tag) (gContraC @tag)

instance
  ( Contravariant (GenericContraF tag)
  , GenericContra tag, GenericContraC tag a
  ) => GContraC tag (S1 c (Rec0 a)) where
    gContraC = contramap (unK1 . unM1) (genericContraF @tag)

instance Divisible (GenericContraF tag) => GContraC tag U1 where
    gContraC = conquer
