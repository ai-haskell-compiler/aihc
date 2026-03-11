{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--  List of types used inside the Registry
module Data.Registry.Internal.Types where

import Data.Dynamic
import Data.Hashable
import Data.List (elemIndex, intersect)
import Data.List.NonEmpty as NonEmpty (head, last)
import Data.MultiMap (MultiMap)
import Data.MultiMap qualified as MM
import Data.Registry.Internal.MultiMap ()
import Data.Registry.Internal.Reflection
import Data.Text qualified as T hiding (last)
import Protolude as P hiding (show)
import Protolude qualified as P
import Type.Reflection
import Prelude (show)

-- | A 'Value' is the 'Dynamic' representation of a Haskell value + its description
--   It is either provided by the user of the Registry or created as part of the
--   resolution algorithm.
--
--   A value can simply be provided by the user of the registry or created as the
--   result of function application
--
--   Dependencies is the transitive list of all the values used to create a CreatedValue
--
--   The optional SpecializationContext is used for values created as the result of a specialization
--   It stores the context of creation (the list of types we are currently trying to build) and
--   the desired specialization (which must be a subtype of the context)
data Value
  = CreatedValue Dynamic ValueDescription (Maybe SpecializationContext) Dependencies
  | ProvidedValue Dynamic ValueDescription
  deriving (Show)

instance Eq Value where
  CreatedValue _ vd1 _sc1 ds1 == CreatedValue _ vd2 _sc2 ds2 =
    (vd1, ds1) == (vd2, ds2)
  ProvidedValue _ vd1 == ProvidedValue _ vd2 =
    vd1 == vd2
  _ == _ = False

instance Hashable Value where
  hash value = hash (valDescription value)
  hashWithSalt n value = hashWithSalt n (valDescription value)

-- | Description of a value. It might just have
--   a description for its type when it is a value
--   created by the resolution algorithm
data ValueDescription = ValueDescription
  { _valueType :: Text,
    _valueValue :: Maybe Text
  }
  deriving (Eq, Show)

instance Hashable ValueDescription where
  hash (ValueDescription d v) = hash (d, v)
  hashWithSalt n (ValueDescription d v) = hashWithSalt n (d, v)

-- | Describe a value with its type and actual content
describeValue :: (Typeable a, Show a) => a -> ValueDescription
describeValue a = ValueDescription (showFullValueType a) (Just . toS $ show a)

-- | Describe a value with only its type
describeTypeableValue :: (Typeable a) => a -> ValueDescription
describeTypeableValue a = ValueDescription (showFullValueType a) Nothing

-- | Show a Value from the Registry
showValue :: Value -> Text
showValue = valDescriptionToText . valDescription

-- | Create a Value from a Haskell value, using its Show instance for its description
createValue :: (Typeable a, Show a) => a -> Value
createValue a = makeProvidedValue (toDyn a) (describeValue a)

-- | Make a ProvidedValue
makeProvidedValue :: Dynamic -> ValueDescription -> Value
makeProvidedValue = ProvidedValue

-- | make a CreatedValue in no particular context
makeCreatedValue :: Dynamic -> ValueDescription -> Dependencies -> Value
makeCreatedValue d desc = CreatedValue d desc Nothing

-- | Create a Value from a Haskell value, with only its 'Typeable' description
createTypeableValue :: Typeable a => a -> Value
createTypeableValue a = ProvidedValue (toDyn a) (describeTypeableValue a)

-- | Create a Value from a 'Dynamic' value and some description
createDynValue :: Dynamic -> Text -> Value
createDynValue dyn desc = ProvidedValue dyn (ValueDescription desc Nothing)

-- | Type representation of a 'Value'
valueDynTypeRep :: Value -> SomeTypeRep
valueDynTypeRep = dynTypeRep . valueDyn

-- | Dynamic representation of a 'Value'
valueDyn :: Value -> Dynamic
valueDyn (CreatedValue d _ _ _) = d
valueDyn (ProvidedValue d _) = d

-- | The description for a 'Value'
valDescription :: Value -> ValueDescription
valDescription (CreatedValue _ d _ _) = d
valDescription (ProvidedValue _ d) = d

-- | The dependencies for a 'Value'
valueDependencies :: Value -> Dependencies
valueDependencies (CreatedValue _ _ _ ds) = ds
valueDependencies (ProvidedValue _ _) = mempty

-- | A ValueDescription as 'Text'. If the actual content of the 'Value'
--   is provided display the type first then the content
valDescriptionToText :: ValueDescription -> Text
valDescriptionToText (ValueDescription t Nothing) = t
valDescriptionToText (ValueDescription t (Just v)) = t <> ": " <> v

--  | Return the context + specialization used when specializing a value
valueSpecializationContext :: Value -> Maybe SpecializationContext
valueSpecializationContext (CreatedValue _ _ sc _) = sc
valueSpecializationContext _ = Nothing

--  | Return the context used when specializing a value
valueContext :: Value -> Maybe Context
valueContext (CreatedValue _ _ sc _) = scContext <$> sc
valueContext _ = Nothing

-- | Return the specialization used when specializing a value
valueSpecialization :: Value -> Maybe Specialization
valueSpecialization (CreatedValue _ _ sc _) = scSpecialization <$> sc
valueSpecialization _ = Nothing

-- | Return True if a type is part of the specialization context of a Value
isInSpecializationContext :: SomeTypeRep -> Value -> Bool
isInSpecializationContext target value =
  case valueContext value of
    Just context -> target `elem` contextTypes context
    Nothing -> False

-- | Return True if a value has transitives dependencies which are
--   specialized values
hasSpecializedDependencies :: Specializations -> Value -> Bool
hasSpecializedDependencies (Specializations ss) v =
  let DependenciesTypes ds = dependenciesTypes $ valueDependencies v
      targetTypes = specializationTargetType <$> ss
   in not . P.null $ targetTypes `intersect` ds

-- | A Function is the 'Dynamic' representation of a Haskell function + its description
data Function = Function Dynamic FunctionDescription deriving (Show)

-- | Create a 'Function' value from a Haskell function
createFunction :: (Typeable f) => f -> Function
createFunction f =
  let dynType = toDyn f
   in Function dynType (describeFunction f)

-- | Description of a 'Function' with input types and output type
data FunctionDescription = FunctionDescription
  { _inputTypes :: [Text],
    _outputType :: Text
  }
  deriving (Eq, Show)

-- | Describe a 'Function' (which doesn't have a 'Show' instance)
--   that can be put in the Registry
describeFunction :: Typeable a => a -> FunctionDescription
describeFunction = uncurry FunctionDescription . showFullFunctionType

-- | Show a Function as 'Text' using its Description
showFunction :: Function -> Text
showFunction = funDescriptionToText . funDescription

-- | The Description of a 'Function'
funDescription :: Function -> FunctionDescription
funDescription (Function _ t) = t

-- | Dynamic representation of a 'Function'
funDyn :: Function -> Dynamic
funDyn (Function d _) = d

-- | Type representation of a 'Function'
funDynTypeRep :: Function -> SomeTypeRep
funDynTypeRep = dynTypeRep . funDyn

-- | Type representation of the output of a  'Function'
funDynOutTypeRep :: Function -> SomeTypeRep
funDynOutTypeRep f =
  go (funDynTypeRep f)
  where
    go (SomeTypeRep (Fun _ out)) = go (SomeTypeRep out)
    go (SomeTypeRep out) = SomeTypeRep out

-- | A 'FunctionDescription' as 'Text'
funDescriptionToText :: FunctionDescription -> Text
funDescriptionToText (FunctionDescription ins out) = T.intercalate " -> " (ins <> [out])

-- | Return True if a 'Function' has some input parameters
hasParameters :: Function -> Bool
hasParameters = isFunction . funDynTypeRep

-- | A Typed value or function can be added to a Registry
--   It is either a value, having both 'Show' and 'Typeable' information
--   or a function having just 'Typeable' information
data Typed a
  = TypedValue Value
  | TypedFunction Function

-- | A Untyped is used for storing either a value or a function
--   in the registry
data Untyped
  = UntypedValue Value
  | UntypedFunction Function
  deriving (Show)

-- | Drop the type variable
untype :: Typed a -> Untyped
untype (TypedValue v) = UntypedValue v
untype (TypedFunction f) = UntypedFunction f

-- | Return the output type of an untyped entry
outTypeRep :: Untyped -> SomeTypeRep
outTypeRep (UntypedValue v) = valueDynTypeRep v
outTypeRep (UntypedFunction f) = funDynOutTypeRep f

-- | Dynamic representation of a 'Function'
untypedDyn :: Untyped -> Dynamic
untypedDyn (UntypedFunction f) = funDyn f
untypedDyn (UntypedValue v) = valueDyn v

-- | This is a list of entries in the registry available for constructing values
--   They are sorted by output type and if there are several available functions or values
--   for a given type the first one in the list has the highest priority
newtype Entries = Entries
  { unFunctions :: MultiMap SomeTypeRep Untyped
  }
  deriving (Show, Semigroup, Monoid)

-- | Create a Entries data structure from a list of untyped entries
fromUntyped :: [Untyped] -> Entries
fromUntyped us = Entries (MM.fromList $ (\u -> (outTypeRep u, u)) <$> us)

-- | Create a list of functions from the Entries data structure
toFunctions :: Entries -> [Function]
toFunctions (Entries es) = mapMaybe (getFunction . snd) (MM.toList es)
  where
    getFunction = \case
      UntypedFunction f -> Just f
      _ -> Nothing

-- | Create a list of values from the Entries data structure
toValues :: Entries -> [Value]
toValues (Entries es) = mapMaybe (getValue . snd) (MM.toList es)
  where
    getValue = \case
      UntypedValue v -> Just v
      _ -> Nothing

-- | Display a list of constructors
describeFunctions :: Entries -> Text
describeFunctions entries@(Entries es) =
  if MM.null es
    then ""
    else unlines (funDescriptionToText . funDescription <$> toFunctions entries)

-- | Display a list of values
describeValues :: Entries -> Text
describeValues entries@(Entries es) =
  if MM.null es
    then ""
    else unlines (valDescriptionToText . valDescription <$> toValues entries)

-- | Add one more Function to the list of Entries.
--   It gets the highest priority for functions with the same output type
addUntyped :: Untyped -> Entries -> Entries
addUntyped e (Entries es) = Entries (MM.insert (outTypeRep e) e es)

-- | Add an entry to the list of Entries.
--   It gets the highest priority for functions with the same output type
addEntry :: Typed a -> Entries -> Entries
addEntry e = addUntyped (untype e)

-- | Add one more untyped entry to the list of Entries
--   It gets the lowest priority for functions with the same output type
--   This is not a very efficient because it requires a full recreation of the map
appendUntyped :: Untyped -> Entries -> Entries
appendUntyped u (Entries es) = Entries (MM.fromList $ MM.toList es <> [(outTypeRep u, u)])

-- | Add one more untyped entry to the list of Entries
--   It gets the lowest priority for functions with the same output type
--   This is not a very efficient because it requires a full recreation of the map
appendEntry :: Typed a -> Entries -> Entries
appendEntry e = appendUntyped (untype e)

-- | Find a function or value returning a target type
--   from a list of entries
findUntyped :: SomeTypeRep -> Entries -> Maybe Untyped
findUntyped target (Entries es) = P.head $ MM.lookup target es

-- | The types of values that we are trying to build at a given moment
--   of the resolution algorithm.
--   We also store the function requiring a given value type to provide
--   better error messages
--   IMPORTANT: this is a *stack*, the deepest elements in the value
--   graph are first in the list
newtype Context = Context
  { _contextStack :: [(SomeTypeRep, Maybe SomeTypeRep)]
  }
  deriving (Eq, Show)

instance Semigroup Context where
  Context c1 <> Context c2 = Context (c1 <> c2)

instance Monoid Context where
  mempty = Context mempty
  mappend = (<>)

-- | Return the target types for a given context
contextTypes :: Context -> [SomeTypeRep]
contextTypes (Context cs) = fmap fst cs

-- | The values that a value depends on
newtype Dependencies = Dependencies
  { unDependencies :: [Value]
  }
  deriving (Eq, Show, Semigroup, Monoid)

-- | The values types that a value depends on
newtype DependenciesTypes = DependenciesTypes
  { unDependenciesTypes :: [SomeTypeRep]
  }
  deriving (Eq, Show, Semigroup, Monoid)

-- | Return the types of all the dependencies
dependenciesTypes :: Dependencies -> DependenciesTypes
dependenciesTypes (Dependencies ds) = DependenciesTypes (valueDynTypeRep <$> ds)

-- | The dependencies of a value + the value itself
dependenciesOf :: Value -> Dependencies
dependenciesOf value = Dependencies $ value : (unDependencies . valueDependencies $ value)

-- | Specification of values which become available for
--   construction when a corresponding type comes in context
newtype Specializations = Specializations
  { unSpecializations :: [Specialization]
  }
  deriving (Show, Semigroup, Monoid)

-- | A specialization is defined by
--   a path of types, from top to bottom in the
--    value graph and a target value, which is the
--   value to use when we need a value of that type
--   on that path.
--   For example:
--      specializationPath = [App, PaymentEngine, TransactionRepository]
--      specializationValue = DatabaseConfig "localhost" 5432
--   This means that need to use this DatabaseConfig whenever
--   trying to find inputs needed to create a TransactionRepository
--   if that repository is necessary to create a PaymentEngine, itself
--   involved in the creation of the App
data Specialization = Specialization
  { _specializationPath :: SpecializationPath,
    _specializationValue :: Untyped
  }
  deriving (Show)

-- | List of consecutive types used when making a specific values
--   See the comments on 'Specialization'
type SpecializationPath = NonEmpty SomeTypeRep

-- | For each dependency of the value
--   Return the specialization context of the value if
--     - that dependency value is specialized
--     - the current value is part of the context stack and part of a context path
specializedContexts :: Value -> [SpecializationContext]
specializedContexts v = do
  let contexts = mapMaybe valueSpecializationContext (unDependencies $ dependenciesOf v)
  P.filter isCurrentValueSpecialized contexts
  where
    isCurrentValueSpecialized (SpecializationContext (Context stack) (Specialization path _)) = do
      let stackTypes = fst <$> stack
      let topSpecializedType = NonEmpty.head path
      let specializedTypes = P.takeWhile (/= topSpecializedType) stackTypes
      valueDynTypeRep v `elem` (topSpecializedType : specializedTypes)

-- | First type of a specialization
specializationStart :: Specialization -> SomeTypeRep
specializationStart = NonEmpty.head . _specializationPath

-- | Last type of a specialization
specializationEnd :: Specialization -> SomeTypeRep
specializationEnd = NonEmpty.last . _specializationPath

-- | Return the type of the replaced value in a specialization
specializationTargetType :: Specialization -> SomeTypeRep
specializationTargetType s =
  case _specializationValue s of
    UntypedValue v -> valueDynTypeRep v
    UntypedFunction f -> funDynOutTypeRep f

-- | This represents the full context in which a value has been specialized
--   Context is the full list of types leading to the creation of that value
--   and Specialization is a sub path describing under which types the value must be specialized
--   For example, when creating a FilePath used by a Logger the context could be: App -> Database -> Sql -> Logger
--   and the Specialization just Database -> Logger
--   to specify that the file path must have a specific value in that case
data SpecializationContext = SpecializationContext {scContext :: Context, scSpecialization :: Specialization} deriving (Show)

-- | A specialization is applicable to a context if all its types
--   are part of that context, in the right order
isContextApplicable :: Context -> Specialization -> Bool
isContextApplicable context (Specialization specializationPath _value) =
  P.all (`elem` contextTypes context) specializationPath

-- | Return the specializations valid in a given context
--   Those are the specializations which path is a subpath of the current context
applicableTo :: Specializations -> Context -> Specializations
applicableTo (Specializations ss) context =
  Specializations (P.filter (isContextApplicable context) ss)

-- | The depth of a specialization in a context is the
--   the index of the "deepest" type of that specialization
--   in the stack of types of that context
--   is the one having its "deepest" type (in the value graph)
--     the "deepest" in the current context
--   If there is a tie we take the "highest" highest type of each
specializationRange :: Context -> Specialization -> SpecializationRange
specializationRange context specialization =
  SpecializationRange
    (specializationStart specialization `elemIndex` contextTypes context)
    (specializationEnd specialization `elemIndex` contextTypes context)

-- | For a given context this represents the position of a specialization path
--   in that context. startRange is the index of the start type of the specialization
--   endRange is the index of the last type.
data SpecializationRange = SpecializationRange
  { _startRange :: Maybe Int,
    _endRange :: Maybe Int
  }
  deriving (Eq, Show)

-- | A specialization range is preferable to another one if its types
--   are more specific (or "deepest" in the value graph) than the other
--   If a path is limited to just one type then a path ending with the same
--   type but specifying other types will take precedence
--   See TypesSpec for some concrete examples.
instance Ord SpecializationRange where
  SpecializationRange s1 e1 <= SpecializationRange s2 e2
    | e1 /= s1 && e2 /= s2 = e1 < e2 || (e1 == e2 && s1 <= s2)
    | e1 == s1 && e2 /= s2 = e1 < e2
    | otherwise = e1 <= e2

-- | In a given context, create a value as specified by a specialization
--   the full context is necessary since the specificationPath is
--   only a subpath of a given creation context
--   Note: there are no dependencies for this value since it has been directly
--   provided by a Specialization
createValueFromSpecialization :: Context -> Specialization -> Untyped
createValueFromSpecialization context specialization@(Specialization _ (UntypedValue (ProvidedValue d desc))) =
  -- the creation context for that value
  UntypedValue $ CreatedValue d desc (Just (SpecializationContext context specialization)) mempty
-- the other case is when we have a specialization function
createValueFromSpecialization _ s = _specializationValue s

-- | Display a list of specializations for the Registry, just showing the
--   context (a type) in which a value must be selected
describeSpecializations :: Specializations -> Text
describeSpecializations (Specializations ss) =
  if P.null ss
    then ""
    else "specializations\n" <> unlines (P.show <$> ss)

-- | List of functions modifying some values right after they have been
--   built. This enables "tweaking" the creation process with slightly
--   different results. Here SomeTypeRep is the target value type a and
newtype Modifiers = Modifiers [(SomeTypeRep, ModifierFunction)] deriving (Semigroup, Monoid)

-- | A ModifierFunction modifies an already created value
--   If that value has been created as the result of a specialization
--   then the specialization path is also passed to the function
--   This is used for memoizing actions using a cache so that we
--   cache each specialized value separately.
type ModifierFunction = [SpecializationContext] -> Function

-- | Create a 'ModifierFunction' value from a Haskell function
--   The application of that function does not depend on the fact
--   that we are trying to apply it to a specialized value
createConstModifierFunction :: (Typeable f) => f -> ModifierFunction
createConstModifierFunction f = const (createFunction f)

-- | Create a 'ModifierFunction' value from a Haskell function
--   that will only act on unspecialized values
createUnspecializedModifierFunction :: forall a f. (Typeable f, Typeable a, Typeable (a -> a)) => f -> ModifierFunction
createUnspecializedModifierFunction f = \case
    [] -> createFunction f
    _ -> createFunction @(a -> a) identity

instance Show Modifiers where
  show = toS . describeModifiers

-- | Display a list of modifiers for the Registry, just showing the
--   type of the modified value
describeModifiers :: Modifiers -> Text
describeModifiers (Modifiers ms) =
  if P.null ms
    then ""
    else "modifiers for types\n" <> unlines (P.show . fst <$> ms)

-- * VALUES

-- | List of values available which can be used as parameters to
--   constructors for building other values
newtype Values = Values {unValues :: MultiMap SomeTypeRep Value} deriving (Show, Semigroup, Monoid)

-- | Create a Values data structure from a list of values
fromValues :: [Value] -> Values
fromValues vs = Values (MM.fromList $ (\v -> (valueDynTypeRep v, v)) <$> vs)

-- | Return values as a list
listValues :: Values -> [Value]
listValues (Values vs) = snd <$> MM.toList vs

-- | Add one more Value to the list of Values
addValue :: Value -> Values -> Values
addValue v (Values vs) = Values (MM.insert (valueDynTypeRep v) v vs)

-- | Add one more Value to the list of Values
--   It gets the lowest priority for values with the same type
--   This is not a very efficient because it requires a full recreation of the map
appendValue :: Value -> Values -> Values
appendValue v (Values vs) = Values (MM.fromList $ MM.toList vs <> [(valueDynTypeRep v, v)])

-- | Find all the values with a specific type
--   from a list of values
findValues :: SomeTypeRep -> Values -> [Value]
findValues target (Values vs) = MM.lookup target vs

-- | Find the first value with a specific type
--   from a list of values
findValue :: SomeTypeRep -> Values -> Maybe Value
findValue target = P.head . findValues target
