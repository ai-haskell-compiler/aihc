{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

module Language.GraphQL.Type.Internal
    ( AbstractType(..)
    , CompositeType(..)
    , Directive(..)
    , Directives
    , Schema(..)
    , Type(..)
    , description
    , directives
    , doesFragmentTypeApply
    , implementations
    , instanceOf
    , lookupCompositeField
    , lookupInputType
    , lookupTypeCondition
    , lookupTypeField
    , mutation
    , outToComposite
    , subscription
    , query
    , types
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Language.GraphQL.AST as Full
import Language.GraphQL.AST.DirectiveLocation (DirectiveLocation)
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out

-- | These are all of the possible kinds of types.
data Type m
    = ScalarType Definition.ScalarType
    | EnumType Definition.EnumType
    | ObjectType (Out.ObjectType m)
    | InputObjectType In.InputObjectType
    | InterfaceType (Out.InterfaceType m)
    | UnionType (Out.UnionType m)
    deriving Eq

-- | Directive definition.
--
-- A definition consists of an optional description, arguments, whether the
-- directive is repeatable, and the allowed directive locations.
data Directive = Directive (Maybe Text) In.Arguments Bool [DirectiveLocation]
    deriving Eq

-- | Directive definitions.
type Directives = HashMap Full.Name Directive

-- | A Schema is created by supplying the root types of each type of operation,
--   query and mutation (optional). A schema definition is then supplied to the
--   validator and executor.
data Schema m = Schema
    (Maybe Text) -- ^ Description.
    (Out.ObjectType m) -- ^ Query.
    (Maybe (Out.ObjectType m)) -- ^ Mutation.
    (Maybe (Out.ObjectType m)) -- ^ Subscription.
    Directives -- ^ Directives
    (HashMap Full.Name (Type m)) -- ^ Types.
    -- Interface implementations (used only for faster access).
    (HashMap Full.Name [Type m])

-- | Schema description.
description :: forall m. Schema m -> Maybe Text
description (Schema description' _ _ _ _ _ _) = description'

-- | Schema query type.
query :: forall m. Schema m -> Out.ObjectType m
query (Schema _ query' _ _ _ _ _) = query'

-- | Schema mutation type.
mutation :: forall m. Schema m -> Maybe (Out.ObjectType m)
mutation (Schema _ _ mutation' _ _ _ _) = mutation'

-- | Schema subscription type.
subscription :: forall m. Schema m -> Maybe (Out.ObjectType m)
subscription (Schema _ _ _ subscription' _ _ _) = subscription'

-- | Schema directive definitions.
directives :: forall m. Schema m -> Directives
directives (Schema _ _ _ _ directives' _ _) = directives'

-- | Types referenced by the schema.
types :: forall m. Schema m -> HashMap Full.Name (Type m)
types (Schema _ _ _ _ _ types' _) = types'

-- | Interface implementations.
implementations :: forall m. Schema m -> HashMap Full.Name [Type m]
implementations (Schema _ _ _ _ _ _ implementations') = implementations'

-- | These types may describe the parent context of a selection set.
data CompositeType m
    = CompositeUnionType (Out.UnionType m)
    | CompositeObjectType (Out.ObjectType m)
    | CompositeInterfaceType (Out.InterfaceType m)
    deriving Eq

-- | These types may describe the parent context of a selection set.
data AbstractType m
    = AbstractUnionType (Out.UnionType m)
    | AbstractInterfaceType (Out.InterfaceType m)
    deriving Eq

doesFragmentTypeApply :: forall m
    . CompositeType m
    -> Out.ObjectType m
    -> Bool
doesFragmentTypeApply (CompositeObjectType fragmentType) objectType =
    fragmentType == objectType
doesFragmentTypeApply (CompositeInterfaceType fragmentType) objectType =
    instanceOf objectType $ AbstractInterfaceType fragmentType
doesFragmentTypeApply (CompositeUnionType fragmentType) objectType =
    instanceOf objectType $ AbstractUnionType fragmentType

instanceOf :: forall m. Out.ObjectType m -> AbstractType m -> Bool
instanceOf objectType (AbstractInterfaceType interfaceType) =
    let Out.ObjectType _ _ interfaces _ = objectType
     in foldr go False interfaces
  where
    go objectInterfaceType@(Out.InterfaceType _ _ interfaces _) acc =
        acc || foldr go (interfaceType == objectInterfaceType) interfaces
instanceOf objectType (AbstractUnionType unionType) =
    let Out.UnionType _ _ members = unionType
     in foldr go False members
  where
    go unionMemberType acc = acc || objectType == unionMemberType

lookupTypeCondition :: forall m
    . Full.Name
    -> HashMap Full.Name (Type m)
    -> Maybe (CompositeType m)
lookupTypeCondition type' types' =
    case HashMap.lookup type' types' of
        Just (ObjectType objectType) ->
            Just $ CompositeObjectType objectType
        Just (UnionType unionType) -> Just $ CompositeUnionType unionType
        Just (InterfaceType interfaceType) ->
            Just $ CompositeInterfaceType interfaceType
        _ -> Nothing

lookupInputType :: Full.Type -> HashMap Full.Name (Type m) -> Maybe In.Type
lookupInputType (Full.TypeNamed name) types' =
    case HashMap.lookup name types' of
        Just (ScalarType scalarType) ->
            Just $ In.NamedScalarType scalarType
        Just (EnumType enumType) ->
            Just $ In.NamedEnumType enumType
        Just (InputObjectType objectType) ->
            Just $ In.NamedInputObjectType objectType
        _ -> Nothing
lookupInputType (Full.TypeList list) types'
    = In.ListType
    <$> lookupInputType list types'
lookupInputType (Full.TypeNonNull (Full.NonNullTypeNamed nonNull)) types' =
    case HashMap.lookup nonNull types' of
        Just (ScalarType scalarType) ->
            Just $ In.NonNullScalarType scalarType
        Just (EnumType enumType) ->
            Just $ In.NonNullEnumType enumType
        Just (InputObjectType objectType) ->
            Just $ In.NonNullInputObjectType objectType
        _ -> Nothing
lookupInputType (Full.TypeNonNull (Full.NonNullTypeList nonNull)) types'
    = In.NonNullListType
    <$> lookupInputType nonNull types'

lookupTypeField :: forall a. Full.Name -> Out.Type a -> Maybe (Out.Field a)
lookupTypeField fieldName outputType =
    outToComposite outputType >>= lookupCompositeField fieldName

lookupCompositeField :: forall a
    . Full.Name
    -> CompositeType a
    -> Maybe (Out.Field a)
lookupCompositeField fieldName = \case
    CompositeObjectType objectType -> objectChild objectType
    CompositeInterfaceType interfaceType -> interfaceChild interfaceType
    _ -> Nothing
  where
    objectChild (Out.ObjectType _ _ _ resolvers) =
        resolverType <$> HashMap.lookup fieldName resolvers
    interfaceChild (Out.InterfaceType _ _ _ fields) =
        HashMap.lookup fieldName fields
    resolverType (Out.ValueResolver objectField _) = objectField
    resolverType (Out.EventStreamResolver objectField _ _) = objectField

outToComposite :: forall a. Out.Type a -> Maybe (CompositeType a)
outToComposite = \case
    Out.ObjectBaseType objectType -> Just $ CompositeObjectType objectType
    Out.InterfaceBaseType interfaceType ->
        Just $ CompositeInterfaceType interfaceType
    Out.UnionBaseType unionType -> Just $ CompositeUnionType unionType
    Out.ListBaseType listType -> outToComposite listType
    _ -> Nothing
