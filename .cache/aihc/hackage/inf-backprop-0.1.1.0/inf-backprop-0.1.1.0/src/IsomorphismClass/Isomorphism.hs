{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  IsomorphismClass.Isomorphism
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Isomorphism class and instances.
module IsomorphismClass.Isomorphism
  ( Isomorphism,
    iso,
  )
where

import Control.Applicative (pure)
import Control.Arrow (Kleisli (Kleisli))
import Control.Category ((.))
import Control.Comonad (Cokleisli (Cokleisli), Comonad, extract)
import Control.Monad (Monad)
import GHC.Base (Type)
import IsomorphismClass (IsomorphicTo, from, to)
import Prelude (($))

-- | A generalization of isomorphism.
-- Type argument @c@ is usually a category.
class Isomorphism (c :: Type -> Type -> Type) where
  -- | Categorical morphism that that is related to an isomorphism map from @a@ to @b@.
  iso :: IsomorphicTo a b => c a b

-- | Trivial instance of 'Isomorphism' that is the map type @(->)@.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Int, fst, Either (Right))
-- >>> import Data.Void (Void)
-- >>> import IsomorphismClass.Extra ()
--
-- >>> (iso :: (->) (a, b) (b, a)) (1, "x")
-- ("x",1)
--
-- >>> (iso :: (->) (a, ()) a) (42, ())
-- 42
--
-- >>> (iso :: (->) (Either Void a) a) (Right 42)
-- 42
instance Isomorphism (->) where
  iso :: IsomorphicTo a b => a -> b
  iso = from

-- | Kleisli (monadic) instance of 'Isomorphism'.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Int, fst, Either (Right))
-- >>> import Data.Void (Void)
-- >>> import Control.Arrow (runKleisli)
-- >>> import IsomorphismClass.Extra ()
--
-- >>> runKleisli (iso :: (Kleisli []) (a, b) (b, a)) (1, "x")
-- [("x",1)]
instance Monad m => Isomorphism (Kleisli m) where
  iso :: IsomorphicTo a b => Kleisli m a b
  iso = Kleisli $ pure . to

-- | Cokleisli (comonadic) instance of 'Isomorphism'.
--
-- ==== __Examples of usage__
--
-- >>> import Prelude (Int, fst, Either (Right), (+))
-- >>> import Data.Void (Void)
-- >>> import Control.Comonad (Cokleisli(Cokleisli), runCokleisli)
-- >>> import Control.Comonad.Store (store, runStore, Store)
-- >>> import IsomorphismClass.Extra ()
--
-- >>> runCokleisli (iso :: (Cokleisli (Store Int)) (a, b) (b, a)) (store (\x -> (x + 1, x + 2)) 0)
-- (2,1)
instance Comonad w => Isomorphism (Cokleisli w) where
  iso :: IsomorphicTo a b => Cokleisli w a b
  iso = Cokleisli $ to . extract
