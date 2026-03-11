{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Throughout this module, we use the following GADT and @ArgDict@ instance
-- in our examples:
--
-- > {-# LANGUAGE StandaloneDeriving #-}
-- >
-- > data Tag a where
-- >   I :: Tag Int
-- >   B :: Tag Bool
-- > deriving instance Show (Tag a)
-- >
-- > $(deriveArgDict ''Tag)
--
-- The constructors of @Tag@ mean that a type variable @a@ in @Tag a@
-- must come from the set { @Int@, @Bool@ }. We call this the "set of
-- types @a@ that could be applied to @Tag@".
module Data.Constraint.Extras
  ( -- * The Has typeclass
    Has(..)
  , argDict'
  , argDictV
    -- * Bringing instances into scope
  , Has'
  , has'
  , HasV
  , hasV
  , whichever
    -- * Misc
  , Implies1(..)
  ) where

import Data.Constraint
import Data.Constraint.Compose
import Data.Constraint.Flip
import Data.Constraint.Forall
import Data.Functor.Sum (Sum(..))
import Data.Kind
import GHC.Generics ((:+:)(..))

-- | The constraint @Has c f@ means that given any value of type @f a@, we can determine
-- that there is an instance of @c a@. For example, @Has Show Tag@ means that given any
-- @x :: Tag a@, we can conclude @Show a@. Most commonly, the type @f@ will be a GADT,
-- where we can enumerate all the possible index types through pattern matching, and
-- discover that there is an appropriate instance in each case. In this sort of
-- situation, the @c@ can be left entirely polymorphic in the instance for @Has@, and
-- this is the sort of instance that the provided Template Haskell code writes.

-- It is also sometimes possible to hand-write instances of @Has c f@ for specific
-- classes @c@ in cases where @f@ is a data type that packs an appropriate dictionary
-- into its constructors.
class Has c f where
  -- | Use the @f a@ to show that there is an instance of @c a@, and
  -- bring it into scope.
  --
  -- The order of type variables is chosen to work
  -- with @-XTypeApplications@.
  --
  -- > -- Hold a value of type a, along with a tag identifying the a.
  -- > data SomeTagged tag where
  -- >   SomeTagged :: a -> tag a -> SomeTagged tag
  -- >
  -- > -- Use the stored tag to identify the thing we have, allowing us to call 'show'. Note that we
  -- > -- have no knowledge of the tag type.
  -- > showSomeTagged :: Has Show tag => SomeTagged tag -> String
  -- > showSomeTagged (SomeTagged a tag) = has @Show tag $ show a
  has :: forall a r. f a -> (c a => r) -> r
  has x r | Dict <- argDict @c x = r

  -- | Use an @f a@ to obtain a dictionary for @c a@
  --
  -- > argDict @Show I :: Dict (Show Int)
  argDict :: forall a. f a -> Dict (c a)
  argDict x = has @c x Dict
  {-# MINIMAL has | argDict #-}

-- | @since 0.3.2.0
instance (Has c f, Has c g) => Has c (f :+: g) where
  argDict = \case
    L1 f -> argDict f
    R1 g -> argDict g

-- | @since 0.3.2.0
instance (Has c f, Has c g) => Has c (Sum f g) where
  argDict = \case
    InL f -> argDict f
    InR g -> argDict g

-- | The constraint @Has' c f g@ means that given a value of type @f a@, we can satisfy the constraint @c (g a)@.
type Has' (c :: k -> Constraint) f (g :: k' -> k) = Has (ComposeC c g) f

-- | The constraint @HasV c f g@ means that given a value of type @f v@, we can satisfy the constraint @c (v g)@.
type HasV c f g = Has (FlipC (ComposeC c) g) f

-- | Get a dictionary for @c (g a)@, using a value of type @f a@.
--
-- > argDict' @Show @Identity B :: Dict (Show (Identity Bool))
argDict' :: forall c g f a. (Has' c f g) => f a -> Dict (c (g a))
argDict' x = has @(ComposeC c g) x Dict

-- | Get a dictionary for @c (v g)@, using a value of type @f v@.
argDictV :: forall f c g v. (HasV c f g) => f v -> Dict (c (v g))
argDictV x = has @(FlipC (ComposeC c) g) x Dict

-- | Like 'has', but we get a @c (g a)@ instance brought into scope
-- instead. Use @-XTypeApplications@ to specify @c@ and @g@.
--
-- > -- From dependent-sum:Data.Dependent.Sum
-- > data DSum tag f = forall a. !(tag a) :=> f a
-- >
-- > -- Show the value from a dependent sum. (We'll need 'whichever', discussed later, to show the key.)
-- > showDSumVal :: forall tag f . Has' Show tag f => DSum tag f -> String
-- > showDSumVal (tag :=> fa) = has' @Show @f tag $ show fa
has' :: forall c g f a r. (Has' c f g) => f a -> (c (g a) => r) -> r
has' k r = has @(ComposeC c g) k r

-- | Similar to 'has', but given a value of type @f v@, we get a @c (v g)@ instance brought into scope instead.
hasV :: forall c g f v r. (HasV c f g) => f v -> (c (v g) => r) -> r
hasV k r = has @(FlipC (ComposeC c) g) k r

-- | Given "forall a. @c (t a)@" (the @ForallF c t@ constraint), select a
-- specific @a@, and bring @c (t a)@ into scope. Use @-XTypeApplications@ to
-- specify @c@, @t@ and @a@.
--
-- > -- Show the tag of a dependent sum, even though we don't know the tag type.
-- > showDSumKey :: forall tag f . ForallF Show tag => DSum tag f -> String
-- > showDSumKey ((tag :: tag a) :=> fa) = whichever @Show @tag @a $ show tag
whichever :: forall c t a r. ForallF c t => (c (t a) => r) -> r
whichever r = r \\ (instF :: ForallF c t :- c (t a))

-- | Allows explicit specification of constraint implication.
class Implies1 c d where
  implies1 :: c a :- d a
