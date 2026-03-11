{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Function.Contra
  ( GenericContra(..)
  , genericContraNonSum, GContraNonSum
  , genericContraSum,    GContraSum
  ) where

import GHC.Generics

import Generic.Data.Function.Contra.NonSum
import Generic.Data.Function.Contra.Sum
import Generic.Data.Function.Contra.Constructor
import Data.Functor.Contravariant

-- | Generic contra over a term of non-sum data type @a@.
--
-- @a@ must have exactly one constructor.
genericContraNonSum
    :: forall {k} (tag :: k) a
    .  ( Generic a
       , Contravariant (GenericContraF tag)
       , GContraNonSum tag (Rep a)
    ) => GenericContraF tag a
genericContraNonSum = contramap from (gContraNonSum @tag)

-- | Generic contra over a term of sum data type @a@.
--
-- You must provide a contra function for constructor names.
--
-- This is the most generic option, but depending on your string manipulation
-- may be slower.
genericContraSum
    :: forall {k} (tag :: k) a
    .  ( Generic a
       , Contravariant (GenericContraF tag)
       , GContraSum tag (Rep a)
    ) => GenericContraF tag String -> GenericContraF tag a
genericContraSum f = contramap from (gContraSum @tag f)
