{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

-- | Various parts of a GraphQL document can be annotated with directives.
-- This module describes locations in a document where directives can appear.
module Language.GraphQL.AST.DirectiveLocation
    ( DirectiveLocation(..)
    , ExecutableDirectiveLocation(..)
    , TypeSystemDirectiveLocation(..)
    ) where

-- | All directives can be splitted in two groups: directives used to annotate
-- various parts of executable definitions and the ones used in the schema
-- definition.
data DirectiveLocation
    = ExecutableDirectiveLocation ExecutableDirectiveLocation
    | TypeSystemDirectiveLocation TypeSystemDirectiveLocation
    deriving Eq

instance Show DirectiveLocation where
    show (ExecutableDirectiveLocation directiveLocation) =
        show directiveLocation
    show (TypeSystemDirectiveLocation directiveLocation) =
        show directiveLocation

-- | Where directives can appear in an executable definition, like a query.
data ExecutableDirectiveLocation
    = Query
    | Mutation
    | Subscription
    | Field
    | FragmentDefinition
    | FragmentSpread
    | InlineFragment
    deriving Eq

instance Show ExecutableDirectiveLocation where
    show Query = "QUERY"
    show Mutation = "MUTATION"
    show Subscription = "SUBSCRIPTION"
    show Field = "FIELD"
    show FragmentDefinition = "FRAGMENT_DEFINITION"
    show FragmentSpread = "FRAGMENT_SPREAD"
    show InlineFragment = "INLINE_FRAGMENT"

-- | Where directives can appear in a type system definition.
data TypeSystemDirectiveLocation
    = Schema
    | Scalar
    | Object
    | FieldDefinition
    | ArgumentDefinition
    | Interface
    | Union
    | Enum
    | EnumValue
    | InputObject
    | InputFieldDefinition
    deriving Eq

instance Show TypeSystemDirectiveLocation where
    show Schema = "SCHEMA"
    show Scalar = "SCALAR"
    show Object = "OBJECT"
    show FieldDefinition = "FIELD_DEFINITION"
    show ArgumentDefinition = "ARGUMENT_DEFINITION"
    show Interface = "INTERFACE"
    show Union = "UNION"
    show Enum = "ENUM"
    show EnumValue = "ENUM_VALUE"
    show InputObject = "INPUT_OBJECT"
    show InputFieldDefinition = "INPUT_FIELD_DEFINITION"
