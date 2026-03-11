{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module is not required to use 'Cauldron's, but it provides a 'Builder' monad which lets you
-- define them in a manner which more closely resembles the syntax of wiring things \"manually\" in 'IO' or 'Managed'.
--
-- >>> :{
-- data Foo
--   = EndFoo
--   | FooToBar Bar
--   deriving stock (Show)
-- --
-- data Bar
--   = EndBar
--   | BarToFoo Foo
--   deriving stock (Show)
-- --
-- newtype Serializer a = Serializer {runSerializer :: a -> String}
-- --
-- makeFooSerializer :: Serializer Bar -> Serializer Foo
-- makeFooSerializer Serializer {runSerializer = runBar} =
--   Serializer
--     { runSerializer = \case
--         EndFoo -> ".EndFoo"
--         FooToBar bar -> ".FooToBar" ++ runBar bar
--     }
-- --
-- makeBarSerializer :: Serializer Foo -> Serializer Bar
-- makeBarSerializer Serializer {runSerializer = runFoo} =
--   Serializer
--     { runSerializer = \case
--         EndBar -> ".EndBar"
--         BarToFoo foo -> ".BarToFoo" ++ runFoo foo
--     }
-- --
-- builder :: Builder Identity ()
-- builder = mdo
--   foo <- _val_ $ makeFooSerializer <$> bar
--   bar <- _val_ $ makeBarSerializer <$> foo
--   pure ()
-- --
-- cauldron :: Either DuplicateBeans (Cauldron Identity)
-- cauldron = execBuilder builder
-- :}
--
-- Note that in the 'Builder' monad the values that we bind with @<-@ when using
-- functions like 'add', '_val_', or '_eff_' are really 'Args' values which
-- merely carry type information. We can dispense with them and use 'arg' or
-- 'wire' instead:
--
-- >>> :{
-- builder2 :: Builder Identity ()
-- builder2 = mdo
--   _ <- add $ val_ $ makeFooSerializer <$> arg
--   _ <- _val_ $ wire makeBarSerializer
--   pure ()
-- :}
module Cauldron.Builder
  ( Builder,
    add,
    execBuilder,

    -- * Two beans of the same type are forbidden
    DuplicateBeans (..),
    prettyDuplicateBeans,
    prettyDuplicateBeansLines,

    -- * Being polymorphic on the wiring monad
    MonadWiring (..),
    _ioEff_,
  )
where

import Cauldron
import Cauldron.Managed
import Control.Exception (Exception (..))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor.Identity
import Data.Kind
import Data.List qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified
import Data.Typeable
import GHC.Exception (CallStack, prettyCallStackLines)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

data Builder m a = Builder (Cauldron m) (Map TypeRep (Seq CallStack)) a
  deriving stock (Functor)

combineCallStackMaps :: Map TypeRep (Seq CallStack) -> Map TypeRep (Seq CallStack) -> Map TypeRep (Seq CallStack)
combineCallStackMaps = Map.unionWith (Data.Sequence.><)

instance Applicative (Builder m) where
  pure a = Builder Cauldron.empty Map.empty a
  Builder c1 m1 f <*> Builder c2 m2 a2 =
    Builder (c1 <> c2) (combineCallStackMaps m1 m2) (f a2)

instance Monad (Builder m) where
  (Builder c1 m1 a) >>= k =
    let Builder c2 m2 r = k a
     in Builder (c1 <> c2) (combineCallStackMaps m1 m2) r

instance MonadFix (Builder m) where
  mfix f =
    let b = f a
        ~(Builder _ _ a) = b
     in b

execBuilder :: Builder m a -> Either DuplicateBeans (Cauldron m)
execBuilder (Builder c m _) =
  let beanDefinitions =
        m
          & Map.mapMaybe \case
            c1 Data.Sequence.:<| c2 Data.Sequence.:<| rest -> Just (c1, c2, Data.Foldable.toList rest)
            _ -> Nothing
   in if (not $ Data.Foldable.null beanDefinitions)
        then Left $ DuplicateBeans beanDefinitions
        else Right c

-- | Because 'Cauldron's inject dependencies based on their types, a do-notation block which
-- binds two or more values of the same type would be ambiguous.
--
-- >>> :{
-- builderOops :: Builder Identity ()
-- builderOops = do
--   foo1 <- _val_ $ pure (5 :: Int)
--   foo2 <- _val_ $ pure (6 :: Int)
--   pure ()
-- :}
--
-- >>> :{
-- case execBuilder builderOops of
--    Left (DuplicateBeans _) -> "this should be the result"
--    Right _ -> "won't happen"
-- :}
-- "this should be the result"
data DuplicateBeans = DuplicateBeans (Map TypeRep (CallStack, CallStack, [CallStack]))
  deriving stock (Show)

instance Exception DuplicateBeans where
  displayException = prettyDuplicateBeans

prettyDuplicateBeans :: DuplicateBeans -> String
prettyDuplicateBeans = Data.List.intercalate "\n" . prettyDuplicateBeansLines

prettyDuplicateBeansLines :: DuplicateBeans -> [String]
prettyDuplicateBeansLines (DuplicateBeans beanMap) =
  [ "Some bean types defined more than once in builder:"
  ]
    ++ ( beanMap & Map.foldMapWithKey \rep (c1, c2, rest) ->
           ( [ "- Bean type " ++ show rep ++ " was defined in these locations:"
             ]
               ++ ( (c1 : c2 : rest) & foldMap \location ->
                      (("\t" ++) <$> prettyCallStackLines location)
                  )
           )
       )

-- | Add a 'Recipe' to the 'Cauldron' that is being built.
add ::
  forall {recipelike} {m} (bean :: Type).
  (Typeable bean, ToRecipe recipelike, HasCallStack) =>
  -- | A 'Recipe' or a 'Constructor'.
  recipelike m bean ->
  Builder m (Args bean)
add recipelike =
  Builder
    (Cauldron.empty & Cauldron.insert recipelike)
    (Map.singleton (typeRep (Proxy @bean)) (Data.Sequence.singleton callStack))
    (arg @bean)

-- | This class allows you to define polymorphic \"wirings\" which can work in
-- the 'Builder' monad to produce 'Cauldron's, but also wire beans directly in
-- 'IO' or 'Managed'.
--
-- If we limit ourselves exclusively to the methods of this class, it's not
-- possible to define decorators or secondary beans.
--
-- This class can help migrating from \"direct\"-style wirings to 'Cauldron's.
--
-- >>> :{
-- data A = A deriving Show
-- data B = B deriving Show
-- data C = C deriving Show
-- makeA :: A
-- makeA = A
-- makeB :: A -> B
-- makeB = \_ -> B
-- makeC :: A -> B -> IO C
-- makeC = \_ _ -> pure C
-- instantiations :: (Builder IO (Args C), IO (Identity C))
-- instantiations =
--    let polymorphicWiring = do
--           a <- _val_ $ pure makeA
--           b <- _val_ $ makeB <$> a
--           c <- _ioEff_ $ makeC <$> a <*> b
--           pure c
--     in (polymorphicWiring, polymorphicWiring)
-- :}
class (Monad m, Applicative (ArgsApplicative m), Monad (ConstructorMonad m)) => MonadWiring m where
  -- | Wraps every bean type that we bind using methods of this class.
  -- Will be 'Args' for 'Builder', but simply 'Identity' for 'IO' and 'Managed'.
  type ArgsApplicative m :: Type -> Type

  -- | The monad in which constructors have effects.
  type ConstructorMonad m :: Type -> Type

  _val_ :: (Typeable bean, HasCallStack) => ArgsApplicative m bean -> m (ArgsApplicative m bean)
  _eff_ :: (Typeable bean, HasCallStack) => ArgsApplicative m (ConstructorMonad m bean) -> m (ArgsApplicative m bean)

-- | Like '_eff_', but lifts 'IO' constructor effects into a general 'MonadIO'.
_ioEff_ ::
  (MonadWiring m, MonadIO (ConstructorMonad m), Typeable bean, HasCallStack) =>
  ArgsApplicative m (IO bean) ->
  m (ArgsApplicative m bean)
_ioEff_ args = withFrozenCallStack $ _eff_ $ liftIO <$> args

instance (Monad m) => MonadWiring (Builder m) where
  type ArgsApplicative (Builder m) = Args
  type ConstructorMonad (Builder m) = m
  _val_ :: (Typeable bean, HasCallStack) => Args bean -> Builder m (Args bean)
  _val_ v = withFrozenCallStack $ add (val_ v)
  _eff_ :: (Typeable bean, HasCallStack) => Args (m bean) -> Builder m (Args bean)
  _eff_ action = withFrozenCallStack $ add (eff_ action)

instance MonadWiring IO where
  type ArgsApplicative IO = Identity
  type ConstructorMonad IO = IO
  _val_ :: Identity bean -> IO (Identity bean)
  _val_ = pure
  _eff_ :: Identity (IO bean) -> IO (Identity bean)
  _eff_ = sequence

instance MonadWiring Managed where
  type ArgsApplicative Managed = Identity
  type ConstructorMonad Managed = Managed
  _val_ :: Identity a -> Managed (Identity a)
  _val_ = pure
  _eff_ :: Identity (Managed a) -> Managed (Identity a)
  _eff_ = sequence

-- $setup
-- >>> :set -XBlockArguments
-- >>> :set -XOverloadedLists
-- >>> :set -XLambdaCase
-- >>> :set -XRecursiveDo
-- >>> :set -XDerivingStrategies
-- >>> :set -Wno-incomplete-uni-patterns
-- >>> import Data.Functor.Identity
-- >>> import Data.Function ((&))
-- >>> import Data.Monoid
-- >>> import Data.Either (either)
-- >>> import Control.Exception (throwIO)
