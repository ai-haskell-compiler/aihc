{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--  A registry supports the creation of values out of existing values and functions.
--
--  It contains 3 parts:
--
--  * entries: they can be either values or functions used to create values
--  * specializations: description of specific values to use while trying to build another value of a given type
--  * modifiers: function to apply to a newly built value before storing it for future use
--
--  The `<:` operator, to append functions or values to a registry:
--
--  > registry =
--  >      val (Config 1)
--  >   <: val "hello"
--  >   <: fun add1
--  >   <: fun show1
--
--  At the type level a list of all the function inputs and all the outputs is being kept to
--  check that when we add a function, all the inputs of that function can be
--  built by the registry. This also ensures that we cannot introduce cycles
--  by adding function which would require each other to build their output
--
--  It is possible to use the `<+>` operator to "override" some configurations:
--
--  >  mocks =
--  >       fun noLogging
--  >    <: fun inMemoryDb
--  >
--  >  mocks <+> registry
module Data.Registry.Registry where

import Data.Dynamic
import Data.Registry.Internal.Types
import Data.Registry.Lift
import Data.Registry.Solver
import Data.Semigroup ((<>))
import Data.Type.Equality
import Protolude as P hiding ((<>))
import Type.Reflection
import qualified Prelude (show)

-- | Container for a list of functions or values
--   Internally all functions and values are stored as 'Dynamic' values
--   so that we can access their representation
data Registry (inputs :: [Type]) (outputs :: [Type]) = Registry
  { _entries :: Entries,
    _specializations :: Specializations,
    _modifiers :: Modifiers
  }

instance Show (Registry inputs outputs) where
  show (Registry es ss@(Specializations ss') ms@(Modifiers ms')) =
    toS . unlines $
      [ "Values\n",
        describeValues es,
        "Constructors\n",
        describeFunctions es
      ]
        <> ( if not (null ss')
               then
                 [ "Specializations\n",
                   describeSpecializations ss
                 ]
               else []
           )
        <> ( if not (null ms')
               then
                 [ "Modifiers\n",
                   describeModifiers ms
                 ]
               else []
           )

instance Semigroup (Registry inputs outputs) where
  (<>) (Registry fs1 ss1 ms1) (Registry fs2 ss2 ms2) =
      Registry (fs1 <> fs2) (ss1 <> ss2) (ms1 <> ms2)

instance Semigroup (Registry inputs outputs) => Monoid (Registry inputs outputs) where
  mempty = Registry mempty mempty mempty
  mappend = (<>)

-- | Append 2 registries together
infixr 4 <+>

(<+>) :: Registry is1 os1 -> Registry is2 os2 -> Registry (is1 :++ is2) (os1 :++ os2)
(<+>)(Registry fs1 ss1 ms1) (Registry fs2 ss2 ms2) =
      Registry (fs1 <> fs2) (ss1 <> ss2) (ms1 <> ms2)

-- | Store an element in the registry
--   Internally elements are stored as 'Dynamic' values
--   The signature checks that a constructor of type a can be fully
--   constructed from elements of the registry before adding it
register :: (Typeable a, IsSubset (Inputs a) out a) => Typed a -> Registry ins out -> Registry (Inputs a :++ ins) (Output a ': out)
register = registerUnchecked

-- | Store an element in the registry
--   Internally elements are stored as 'Dynamic' values
registerUnchecked :: (Typeable a) => Typed a -> Registry ins out -> Registry (Inputs a :++ ins) (Output a ': out)
registerUnchecked t (Registry entries specializations modifiers) =
  Registry (addEntry t entries) specializations modifiers

-- | Store an element in the registry, at the end of the registry
--   Internally elements are stored as 'Dynamic' values
appendUnchecked :: (Typeable a) => Registry ins out -> Typed a -> Registry (ins :++ Inputs a) (out :++ '[Output a])
appendUnchecked (Registry entries specializations modifiers) t =
  Registry (appendEntry t entries) specializations modifiers

-- | Add 2 typed values together to form an initial registry
addTypedUnchecked :: (Typeable a, Typeable b, ins ~ (Inputs a :++ Inputs b), out ~ '[Output a, Output b]) => Typed a -> Typed b -> Registry ins out
addTypedUnchecked t1 t2 = Registry (fromUntyped [untype t1, untype t2]) mempty mempty

-- | Add an element to the Registry but do not check that the inputs of a
--   can already be produced by the registry
infixr 5 +:

-- | Prepend an element to the registry with no checks at all
(+:) :: (Typeable a) => Typed a -> Registry ins out -> Registry (Inputs a :++ ins) (Output a ': out)
(+:) = registerUnchecked

-- Unification of +: and <+>
infixr 5 <:

-- | Typeclass for appending values and or registries together, with static checks
class AddRegistryLike a b c | a b -> c where
  (<:) :: a -> b -> c

instance (insr ~ (ins1 :++ ins2), outr ~ (out1 :++ out2), AreSubset ins1 outr out1) => AddRegistryLike (Registry ins1 out1) (Registry ins2 out2) (Registry insr outr) where
  (<:) = (<+>)

instance
  (Typeable a, IsSubset (Inputs a) out2 a, insr ~ (Inputs a :++ ins2), outr ~ (Output a : out2)) =>
  AddRegistryLike (Typed a) (Registry ins2 out2) (Registry insr outr)
  where
  (<:) = register

instance
  (Typeable a, AreSubset ins2 outr out2, insr ~ (ins2 :++ Inputs a), outr ~ (out2 :++ '[Output a])) =>
  AddRegistryLike (Registry ins2 out2) (Typed a) (Registry insr outr)
  where
  (<:) = appendUnchecked

instance
  (Typeable a, IsSubset (Inputs a) '[Output b] a, Typeable b, insr ~ (Inputs a :++ Inputs b), outr ~ (Output a : '[Output b])) =>
  AddRegistryLike (Typed a) (Typed b) (Registry insr outr)
  where
  (<:) = addTypedUnchecked

-- Unchecked unification of +: and <+>
infixr 5 <+

-- | Typeclass for appending values and or registries together, without static checks
class AddRegistryUncheckedLike a b c | a b -> c where
  (<+) :: a -> b -> c

instance (insr ~ (ins1 :++ ins2), outr ~ (out1 :++ out2)) => AddRegistryUncheckedLike (Registry ins1 out1) (Registry ins2 out2) (Registry insr outr) where
  (<+) = (<+>)

instance
  (Typeable a, insr ~ (Inputs a :++ ins2), outr ~ (Output a : out2)) =>
  AddRegistryUncheckedLike (Typed a) (Registry ins2 out2) (Registry insr outr)
  where
  (<+) = registerUnchecked

instance
  (Typeable a, insr ~ (ins2 :++ Inputs a), outr ~ (out2 :++ '[Output a])) =>
  AddRegistryUncheckedLike (Registry ins2 out2) (Typed a) (Registry insr outr)
  where
  (<+) = appendUnchecked

instance
  (Typeable a, Typeable b, insr ~ (Inputs a :++ Inputs b), outr ~ '[Output a, Output b]) =>
  AddRegistryUncheckedLike (Typed a) (Typed b) (Registry insr outr)
  where
  (<+) = addTypedUnchecked

-- | Make the lists of types in the Registry unique, either for better display
--   or for faster compile-time resolution with the make function
normalize :: Registry ins out -> Registry (Normalized ins) (Normalized out)
normalize (Registry es ss ms) = Registry es ss ms

-- | Remove the parameters list of the registry and replace it with an empty type
--   This makes it easier to read compilation errors where less types are being displayed
--   On the other hand the resulting registry cannot be type-checked anymore when trying to get values out of it
eraseTypes :: Registry ins out -> Registry '[ERASED_TYPES] '[ERASED_TYPES]
eraseTypes (Registry entries specializations modifiers) = Registry entries specializations modifiers

-- | Singleton type representing erased types
data ERASED_TYPES

-- | In case it is hard to show that the types of 2 registries align
--   for example with conditional like
--     if True then fun myFunctionWithKnownOutputs <: r else r
safeCoerce :: (IsSameSet out out1) => Registry ins out -> Registry ins1 out1
safeCoerce (Registry a b c) = Registry a b c

-- | And for extreme cases where you know you're doing the right thing but can't prove it
unsafeCoerce :: Registry ins out -> Registry ins1 out1
unsafeCoerce (Registry a b c) = Registry a b c

-- | The empty Registry
end :: Registry '[] '[]
end = Registry mempty mempty mempty

-- | Create a value which can be added to the Registry
val :: (Typeable a, Show a) => a -> Typed a
val a = TypedValue (ProvidedValue (toDyn a) (describeValue a))

-- | Create a value which can be added to the Registry and "lift" it to an 'Applicative' context
valTo :: forall m a. (Applicative m, Typeable a, Typeable (m a), Show a) => a -> Typed (m a)
valTo a = TypedValue (liftProvidedValue @m a)

-- | Create a "lifted" a Value
liftProvidedValue :: forall m a. (Applicative m, Typeable a, Typeable (m a), Show a) => a -> Value
liftProvidedValue a = ProvidedValue (toDyn (pure a :: m a)) (describeValue a)

-- | Create a function which can be added to the Registry
fun :: (Typeable a) => a -> Typed a
fun a = TypedFunction (createFunction a)

-- | This is a shortcut to @fun . allTo@ where @allTo@ lifts all the inputs and output
--   to an 'Applicative' context
funTo :: forall m a b. (ApplyVariadic m a b, Typeable a, Typeable b) => a -> Typed b
funTo a = fun (allTo @m a)

-- | This is a shortcut to @fun . argsTo@ where @allTo@ lifts the inputs only
--   to an 'Applicative' context
--   In general `funTo` should work, even with function already returning an m a
--   but if this is not the case (see issue #7) then funAs can be used
funAs :: forall m a b. (ApplyVariadic1 m a b, Typeable a, Typeable b) => a -> Typed b
funAs a = fun (argsTo @m a)

-- | For a given type a being currently built
--   when a value of type b is required pass a specific value
specialize :: forall a b ins out. (Typeable a) => Typed b -> Registry ins out -> Registry ins out
specialize b (Registry entries (Specializations c) modifiers) = do
  let ss = Specializations (Specialization (pure $ someTypeRep (Proxy :: Proxy a)) (untype b) : c)
  Registry entries ss modifiers

-- | Specialize a function for a specific path of types
specializePath :: forall path b ins out. (PathToTypeReps path) => Typed b -> Registry ins out -> Registry ins out
specializePath b (Registry entries (Specializations c) modifiers) = do
  let ss = Specializations (Specialization (someTypeReps (Proxy :: Proxy path)) (untype b) : c)
  Registry entries ss modifiers

-- | Typeclass for extracting type representations out of a list of types
class PathToTypeReps (path :: [Type]) where
  someTypeReps :: Proxy path -> NonEmpty SomeTypeRep

instance {-# OVERLAPPING #-} (Typeable a) => PathToTypeReps '[a] where
  someTypeReps = const $ pure (someTypeRep (Proxy :: Proxy a))

instance (Typeable a, PathToTypeReps rest) => PathToTypeReps (a : rest) where
  someTypeReps = const $ someTypeRep (Proxy :: Proxy a) :| toList (someTypeReps (Proxy :: Proxy rest))

-- | Once a value has been computed allow to modify it before storing it
--   This keeps the same registry type
tweak :: forall a ins out. (Typeable a) => (a -> a) -> Registry ins out -> Registry ins out
tweak f (Registry entries specializations (Modifiers mf)) =
  Registry
    entries
    specializations
    (Modifiers ((someTypeRep (Proxy :: Proxy a), createConstModifierFunction f) : mf))

-- | Once a value has been computed allow to modify it before storing it
--   This keeps the same registry type
--   This only tweaks unspecialized values!
tweakUnspecialized :: forall a ins out. (Typeable a) => (a -> a) -> Registry ins out -> Registry ins out
tweakUnspecialized f (Registry entries specializations (Modifiers mf)) =
  Registry
    entries
    specializations
    (Modifiers ((someTypeRep (Proxy :: Proxy a), createUnspecializedModifierFunction @a f) : mf))
