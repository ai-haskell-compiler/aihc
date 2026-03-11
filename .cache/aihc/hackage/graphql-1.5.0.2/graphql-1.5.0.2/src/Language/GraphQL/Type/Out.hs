{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Output types and values, monad transformer stack used by the @GraphQL@
-- resolvers.
--
-- This module is intended to be imported qualified, to avoid name clashes
-- with 'Language.GraphQL.Type.In'.
module Language.GraphQL.Type.Out
    ( Context(..)
    , Field(..)
    , InterfaceType(..)
    , ObjectType(..)
    , Resolve
    , Subscribe
    , Resolver(..)
    , SourceEventStream
    , Type(..)
    , UnionType(..)
    , argument
    , isNonNullType
    , pattern EnumBaseType
    , pattern InterfaceBaseType
    , pattern ListBaseType
    , pattern ObjectBaseType
    , pattern ScalarBaseType
    , pattern UnionBaseType
    ) where

import Conduit
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST (Name)
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.In as In

-- | Object type definition.
--
-- Almost all of the GraphQL types you define will be object types. Object
-- types have a name, but most importantly describe their fields.
data ObjectType m = ObjectType
    Name (Maybe Text) [InterfaceType m] (HashMap Name (Resolver m))

instance forall a. Eq (ObjectType a) where
    (ObjectType this _ _ _) == (ObjectType that _ _ _) = this == that

instance forall a. Show (ObjectType a) where
    show (ObjectType typeName _ _ _) = Text.unpack typeName

-- | Interface Type Definition.
--
-- When a field can return one of a heterogeneous set of types, a Interface type
-- is used to describe what types are possible, and what fields are in common
-- across all types.
data InterfaceType m = InterfaceType
    Name (Maybe Text) [InterfaceType m] (HashMap Name (Field m))

instance forall a. Eq (InterfaceType a) where
    (InterfaceType this _ _ _) == (InterfaceType that _ _ _) = this == that

instance forall a. Show (InterfaceType a) where
    show (InterfaceType typeName _ _ _) = Text.unpack typeName

-- | Union Type Definition.
--
-- When a field can return one of a heterogeneous set of types, a Union type is
-- used to describe what types are possible.
data UnionType m = UnionType Name (Maybe Text) [ObjectType m]

instance forall a. Eq (UnionType a) where
    (UnionType this _ _) == (UnionType that _ _) = this == that

instance forall a. Show (UnionType a) where
    show (UnionType typeName _ _) = Text.unpack typeName

-- | Output object field definition.
data Field m = Field
    (Maybe Text) -- ^ Description.
    (Type m) -- ^ Field type.
    In.Arguments -- ^ Arguments.

-- | These types may be used as output types as the result of fields.
--
-- GraphQL distinguishes between "wrapping" and "named" types. Each wrapping
-- type can wrap other wrapping or named types. Wrapping types are lists and
-- Non-Null types (named types are nullable by default).
data Type m
    = NamedScalarType ScalarType
    | NamedEnumType EnumType
    | NamedObjectType (ObjectType m)
    | NamedInterfaceType (InterfaceType m)
    | NamedUnionType (UnionType m)
    | ListType (Type m)
    | NonNullScalarType ScalarType
    | NonNullEnumType EnumType
    | NonNullObjectType (ObjectType m)
    | NonNullInterfaceType (InterfaceType m)
    | NonNullUnionType (UnionType m)
    | NonNullListType (Type m)
    deriving Eq

instance forall a. Show (Type a) where
    show (NamedScalarType scalarType) = show scalarType
    show (NamedEnumType enumType) = show enumType
    show (NamedObjectType inputObjectType) = show inputObjectType
    show (NamedInterfaceType interfaceType) = show interfaceType
    show (NamedUnionType unionType) = show unionType
    show (ListType baseType) = concat ["[", show baseType, "]"]
    show (NonNullScalarType scalarType) = showNonNullType scalarType
    show (NonNullEnumType enumType) = showNonNullType enumType
    show (NonNullObjectType inputObjectType) = showNonNullType inputObjectType
    show (NonNullInterfaceType interfaceType) = showNonNullType interfaceType
    show (NonNullUnionType unionType) = showNonNullType unionType
    show (NonNullListType baseType) = showNonNullListType baseType

-- | Matches either 'NamedScalarType' or 'NonNullScalarType'.
pattern ScalarBaseType :: forall m. ScalarType -> Type m
pattern ScalarBaseType scalarType <- (isScalarType -> Just scalarType)

-- | Matches either 'NamedEnumType' or 'NonNullEnumType'.
pattern EnumBaseType :: forall m. EnumType -> Type m
pattern EnumBaseType enumType <- (isEnumType -> Just enumType)

-- | Matches either 'NamedObjectType' or 'NonNullObjectType'.
pattern ObjectBaseType :: forall m. ObjectType m -> Type m
pattern ObjectBaseType objectType <- (isObjectType -> Just objectType)

-- | Matches either 'NamedInterfaceType' or 'NonNullInterfaceType'.
pattern InterfaceBaseType :: forall m. InterfaceType m -> Type m
pattern InterfaceBaseType interfaceType <-
    (isInterfaceType -> Just interfaceType)

-- | Matches either 'NamedUnionType' or 'NonNullUnionType'.
pattern UnionBaseType :: forall m. UnionType m -> Type m
pattern UnionBaseType unionType <- (isUnionType -> Just unionType)

-- | Matches either 'ListType' or 'NonNullListType'.
pattern ListBaseType :: forall m. Type m -> Type m
pattern ListBaseType listType <- (isListType -> Just listType)

{-# COMPLETE ScalarBaseType
    , EnumBaseType
    , ObjectBaseType
    , ListBaseType
    , InterfaceBaseType
    , UnionBaseType
    #-}

isScalarType :: forall m. Type m -> Maybe ScalarType
isScalarType (NamedScalarType outputType) = Just outputType
isScalarType (NonNullScalarType outputType) = Just outputType
isScalarType _ = Nothing

isObjectType :: forall m. Type m -> Maybe (ObjectType m)
isObjectType (NamedObjectType outputType) = Just outputType
isObjectType (NonNullObjectType outputType) = Just outputType
isObjectType _ = Nothing

isEnumType :: forall m. Type m -> Maybe EnumType
isEnumType (NamedEnumType outputType) = Just outputType
isEnumType (NonNullEnumType outputType) = Just outputType
isEnumType _ = Nothing

isInterfaceType :: forall m. Type m -> Maybe (InterfaceType m)
isInterfaceType (NamedInterfaceType interfaceType) = Just interfaceType
isInterfaceType (NonNullInterfaceType interfaceType) = Just interfaceType
isInterfaceType _ = Nothing

isUnionType :: forall m. Type m -> Maybe (UnionType m)
isUnionType (NamedUnionType unionType) = Just unionType
isUnionType (NonNullUnionType unionType) = Just unionType
isUnionType _ = Nothing

isListType :: forall m. Type m -> Maybe (Type m)
isListType (ListType outputType) = Just outputType
isListType (NonNullListType outputType) = Just outputType
isListType _ = Nothing

-- | Checks whether the given output type is a non-null type.
isNonNullType :: forall m. Type m -> Bool
isNonNullType (NonNullScalarType _) = True
isNonNullType (NonNullEnumType _) = True
isNonNullType (NonNullObjectType _) = True
isNonNullType (NonNullInterfaceType _) = True
isNonNullType (NonNullUnionType _) = True
isNonNullType (NonNullListType _) = True
isNonNullType _ = False

-- | Resolution context holds resolver arguments and the root value.
data Context = Context
    { arguments :: Arguments
    , values :: Value
    }

-- | Monad transformer stack used by the resolvers for determining the resolved
-- value of a field.
type Resolve m = ReaderT Context m Value

-- | Monad transformer stack used by the resolvers for determining the resolved
-- event stream of a subscription field.
type Subscribe m = ReaderT Context m (SourceEventStream m)

-- | A source stream represents the sequence of events, each of which will
-- trigger a GraphQL execution corresponding to that event.
type SourceEventStream m = ConduitT () Value m ()

-- | 'Resolver' associates some function(s) with each 'Field'. 'ValueResolver'
-- resolves a 'Field' into a 'Value'. 'EventStreamResolver' resolves
-- additionally a 'Field' into a 'SourceEventStream' if it is the field of a
-- root subscription type.
--
-- The resolvers aren't part of the 'Field' itself because not all fields
-- have resolvers (interface fields don't have an implementation).
data Resolver m
    = ValueResolver (Field m) (Resolve m)
    | EventStreamResolver (Field m) (Resolve m) (Subscribe m)

-- | Retrieves an argument by its name. If the argument with this name couldn't
-- be found, returns 'Null' (i.e. the argument is assumed to
-- be optional then).
argument :: Monad m => Name -> Resolve m
argument argumentName = do
    argumentValue <- asks $ lookupArgument . arguments
    pure $ fromMaybe Null argumentValue
  where
    lookupArgument (Arguments argumentMap) =
        HashMap.lookup argumentName argumentMap
