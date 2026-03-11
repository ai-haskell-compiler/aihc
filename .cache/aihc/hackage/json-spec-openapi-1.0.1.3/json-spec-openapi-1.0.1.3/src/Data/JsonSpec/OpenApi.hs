{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  This module provides tools for integrating the type-level JSON
  'Specification' with the "openapi" package.

  You can use 'toOpenApiSchema' as a low-level tool to transform @json-spec@
  'Specification's into @openapi3@ 'Schema's directly, irrespective of any
  particular business data type.

  More likely you will want to use @-XDerivingVia@ along with 'EncodingSchema'
  or 'DecodingSchema' to derive 'ToSchema' instances for your data types.

  Example, given this data type:

  > data User = User
  >   {      name :: Text
  >   , lastLogin :: Maybe UTCTime
  >   }
  >   deriving ToSchema via (EncodingSchema User) -- <-- ToSchema instance defined here
  > instance HasJsonEncodingSpec User where
  >   type EncodingSpec User =
  >     JsonObject
  >       '[ Required "name" JsonString
  >        , Optional "last-login" JsonDateTime
  >        ]
  >   toJSONStructure user =
  >     (Field @"name" (name user),
  >     (fmap (Field @"last-login") (lastLogin user),
  >     ()))

  Calling @'Data.Aeson.encode' ('Data.OpenApi3.toSchema' ('Proxy' :: 'Proxy' User))@
  will produce the following Schema:

  > {
  >   "additionalProperties": false,
  >   "properties": {
  >     "last-login": {
  >       "format": "date-time",
  >       "type": "string"
  >     },
  >     "name": {
  >       "type": "string"
  >     }
  >   },
  >   "required": [
  >     "name"
  >   ],
  >   "type": "object"
  > }

  If you needed more control over the content of the schema you might also
  consider doing something like this, e.g. in the case where you would like to
  allow additional properties:

  > data User = User
  >   {      name :: Text
  >   , lastLogin :: Maybe UTCTime
  >   }
  > instance HasJsonEncodingSpec User where
  >   type EncodingSpec User =
  >     JsonObject
  >       '[ Required "name" JsonString
  >        , Optional "last-login" JsonDateTime
  >        ]
  >   toJSONStructure user =
  >     (Field @"name" (name user),
  >     (fmap (Field @"last-login") (lastLogin user),
  >     ()))
  > instance ToSchema User where
  >   declareNamedSchema _proxy =
  >       pure $
  >         NamedSchema
  >           Nothing
  >           (
  >             toOpenApiSchema (EncodingSpec User)
  >               & set
  >                   additionalProperties
  >                   (Just (AdditionalPropertiesAllowed True))
  >           )

-}
module Data.JsonSpec.OpenApi (
  toOpenApiSchema,
  Schemaable,
  EncodingSchema(..),
  DecodingSchema(..),
  Rename,
) where


import Control.Lens (At(at), (&), over, set)
import Data.Aeson (ToJSON(toJSON))
import Data.Functor.Identity (Identity(runIdentity))
import Data.JsonSpec
  ( FieldSpec(Optional, Required), HasJsonDecodingSpec(DecodingSpec)
  , HasJsonEncodingSpec(EncodingSpec)
  , Specification
    ( JsonArray, JsonBool, JsonDateTime, JsonEither, JsonInt, JsonLet
    , JsonNullable, JsonNum, JsonObject, JsonRaw, JsonRef, JsonString, JsonTag
    )
  )
import Data.JsonSpec.OpenApi.Rename (Rename)
import Data.OpenApi
  ( AdditionalProperties(AdditionalPropertiesAllowed)
  , HasAdditionalProperties(additionalProperties), HasEnum(enum_)
  , HasFormat(format), HasItems(items), HasOneOf(oneOf)
  , HasProperties(properties), HasRequired(required), HasType(type_)
  , NamedSchema(NamedSchema), OpenApiItems(OpenApiItemsObject)
  , OpenApiType
    ( OpenApiArray, OpenApiBoolean, OpenApiInteger, OpenApiNull, OpenApiNumber
    , OpenApiObject, OpenApiString
    )
  , Reference(Reference), Referenced(Inline), ToSchema(declareNamedSchema)
  , Definitions, Schema
  )
import Data.OpenApi.Declare (DeclareT(runDeclareT), MonadDeclare(declare))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Typeable (Proxy(Proxy), Typeable)
import GHC.TypeError (ErrorMessage((:$$:), (:<>:)), Unsatisfiable, unsatisfiable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude
  ( Applicative(pure), Bool(False), Functor(fmap), Maybe(Just, Nothing)
  , Monoid(mempty), ($), (.)
  )
import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Data.OpenApi as OA
import qualified GHC.TypeError as TE


{-|
  Convert a 'Specification' into an OpenApi 'Schema'. The type class
  'Schemaable' is an internal and opaque implementation detail and not
  something you should have to worry about.

  It should already have an instance for every 'Specification' that can
  be turned into a 'Schema'. If it does not, then that is a bug. Please
  report it! :-)

  The limitations of this function are:

  * It behaves in a possibly unexpected way when given a top level schema
    of the form:

      > JsonLet '[
      >   '("foo", ...)
      > ] (
      >   JsonRef "foo"
      > )

      'toOpenApiSchema' returns a 'Schema', not a @'Referenced' 'Schema'@.
      Therefore, if the \"top level\" of the 'Specification' is a 'JsonRef',
      then we will try to dereference and inline the referenced schema. In
      other words,

      > toOpenApiSchema (Proxy @(
      >     JsonLet
      >       '[ '("foo", JsonString) ]
      >       (JsonRef "foo")
      >   ))

      will behave as if you had called

      > toOpenApiSchema (Proxy @(
      >     JsonLet
      >       '[ '("foo", JsonString) ]
      >       JsonString
      >   ))

      However, if the reference is undefined, then you will get a custom
      type error explaining what the problem is.

  * With the exception of the above point, we do not check to make sure
    that every referenced used in the returned 'Schema' actually contains
    a definition. So for instance this will \"work\":

      > let
      >   (defs, schema) =
      >     toOpenApiSchema
      >       (Proxy @(
      >         JsonObject '[
      >           ("bar", JsonRef "not-defined")
      >         ]
      >       ))
      > in
      >   ...

      This will compile, and will not throw any runtime errors directly,
      but depending on how you /use/ @defs@ and @schema@ (like, for
      instance, generating an OpenApi specification) you will probably
      encounter a runtime error complaining that "not-defined" hasn't
      been defined.
-}
toOpenApiSchema
  :: forall spec.
     (Schemaable spec)
  => Proxy (spec :: Specification)
  -> (Definitions Schema, Schema)
toOpenApiSchema Proxy =
  runIdentity (runDeclareT (schemaable @spec) mempty)


{-|
  Specifications that can be turned into OpenApi 'Schema's.

  This is intended to be an opaque implementation detail. The only
  reason it is exported is because there are some cases where you might
  need to be able to spell this constraint in code that builds off of
  this package.
-}
class Schemaable (spec :: Specification) where
  schemaable
    :: (MonadDeclare (Definitions Schema) m)
    => m Schema

instance (Inlineable '[] spec) => Schemaable spec where
  schemaable = inlineable @'[] @spec

class
    Inlineable
      (defs :: [ (Symbol, Specification) ])
      (spec :: Specification)
  where
    inlineable
      :: (MonadDeclare (Definitions Schema) m)
      => m Schema
instance (KnownSymbol tag) => Inlineable defs ('JsonTag tag) where
  inlineable =
    pure $
      mempty & set enum_ (Just [toJSON (sym @tag :: Text)])
instance Inlineable defs JsonString where
  inlineable =
    pure $
      mempty & set type_ (Just OpenApiString)
instance {- Inlineable defs ('JsonEither left right) -}
    ( Refable defs left
    , Refable defs right
    )
  =>
    Inlineable defs ('JsonEither left right)
  where
    inlineable = do
      schemaLeft <- refable @defs @left
      schemaRight <- refable @defs @right
      pure $
        mempty
          & set oneOf (Just
              [ schemaLeft
              , schemaRight
              ]
          )
instance Inlineable defs JsonNum where
  inlineable =
    pure $
      mempty & set type_ (Just OpenApiNumber)
instance Inlineable defs JsonInt where
  inlineable =
    pure $
      mempty & set type_ (Just OpenApiInteger)
instance Inlineable defs (JsonObject '[]) where
  inlineable =
    pure $
      mempty
        & set type_ (Just OpenApiObject)
        & set additionalProperties (Just (AdditionalPropertiesAllowed False))
instance {- Inlineable defs (JsonObject ( Optional key spec : more )) -}
    ( Inlineable defs (JsonObject more)
    , Refable defs spec
    , KnownSymbol key
    )
  =>
    Inlineable defs (JsonObject ( Optional key spec : more ))
  where
    inlineable = do
      propertySchema <- refable @defs @spec
      more <- inlineable @defs @('JsonObject more)
      pure $
        more
          & over
              properties
              (set (at (sym @key)) (Just propertySchema))
instance {- Inlineable defs (JsonObject ( Required key spec : more )) -}
    ( Inlineable defs (JsonObject ( Optional key spec : more ))
    , KnownSymbol key
    )
  =>
    Inlineable defs (JsonObject ( Required key spec : more ))
  where
    inlineable = do
      schema <- inlineable @defs @(JsonObject ( Optional key spec : more ))
      pure $
        schema
          & over required (sym @key:)
instance {- Inlineable defs (JsonArray spec) -}
    (Refable defs spec)
  =>
    Inlineable defs (JsonArray spec)
  where
    inlineable = do
      elementSchema <- refable @defs @spec
      pure $
        mempty
          & set type_ (Just OpenApiArray)
          & set items (Just (OpenApiItemsObject elementSchema))
instance Inlineable defs JsonBool where
  inlineable =
    pure $
      mempty & set type_ (Just OpenApiBoolean)
instance Inlineable defs JsonDateTime where
  inlineable =
    pure $
      mempty
        & set type_ (Just OpenApiString)
        & set format (Just "date-time")
instance (Refable defs spec) => Inlineable defs (JsonNullable spec) where
  inlineable = do
    schema <- refable @defs @spec
    pure $
      mempty
        & set oneOf (Just
            [ Inline (mempty & set type_ (Just OpenApiNull))
            , schema
            ]
        )
instance Inlineable defs JsonRaw where
  inlineable =
    pure $
      mempty
      & set type_ (Just OpenApiObject)
instance {- Inlineable defs (JsonLet newDefs spec) -}
    ( Inlineable (Concat newDefs defs) spec
    , Defs (Concat newDefs defs) newDefs
    )
  =>
    Inlineable defs (JsonLet newDefs spec)
  where
    inlineable = do
      mkDefs @(Concat newDefs defs) @newDefs
      inlineable @(Concat newDefs defs) @spec
instance {- Inlineable defs (JsonRef target) -}
    ( Deref defs defs target
    )
  =>
    Inlineable defs (JsonRef target)
  where
    inlineable = deref @defs @defs @target


{-|
  Specifications that can be turned into OpenApi Schemas or a reference
  to a schema.
-}
class
    Refable
      (defs :: [(Symbol, Specification)])
      (spec :: Specification)
  where
    refable
      :: (MonadDeclare (Definitions Schema) m)
      => m (Referenced Schema)
instance {-# overlappable #-} (Inlineable defs a) => Refable defs a where
  refable = fmap Inline (inlineable @defs @a)
instance
    ( Defs newDefs newDefs
    , Refable (Concat newDefs defs) spec
    )
  =>
    Refable defs (JsonLet newDefs spec)
  where
    refable = do
      mkDefs @newDefs @newDefs
      refable @(Concat newDefs defs) @spec
instance (KnownSymbol name) => Refable defs (JsonRef name) where
  refable =
    pure (ref (sym @name))


class
    Deref
      (defs   :: [(Symbol, Specification)])
      (search :: [(Symbol, Specification)])
      (target :: Symbol)
  where
    deref
      :: MonadDeclare (Definitions Schema) m
      => m Schema
instance (NotDereferenceable defs target) => Deref defs '[] target where
  deref = unsatisfiable
instance {- Deref defs ( '(target, spec) ': more) target -}
    ( Inlineable defs spec
    )
  =>
    Deref defs ( '(target, spec) ': more) target
  where
    deref = inlineable @defs @spec
instance {- Deref defs ( '(miss, spec) ': more) target -}
    {-# overlaps #-}
    (Deref defs more target)
  =>
    Deref defs ( '(miss, spec) ': more) target
  where
    deref =
      deref @defs @more @target
type NotDereferenceable defs target =
  Unsatisfiable (
    TE.Text
      "Symbol not defined (in a position \
      \where we must dereference the referenced schema)."
      :$$: TE.Text "The symbol is: " :<>: TE.ShowType target
      :$$: TE.Text "The definitions in scope are: " :<>: TE.ShowType defs
  )


{-| Go through and make a declaration for each item in a JsonLet.  -}
class
    Defs
      (allDefs :: [(Symbol, Specification)])
      (defs :: [(Symbol, Specification)])
  where
    mkDefs
      :: (MonadDeclare (Definitions Schema) m)
      => m ()
instance Defs defs '[] where
  mkDefs = pure ()
instance {- Defs defs ( '(name, spec) ': more) -}
    ( Defs defs more
    , Inlineable defs spec
    , KnownSymbol name
    )
  =>
    Defs defs ( '(name, spec) ': more)
  where
    mkDefs = do
      schema <- inlineable @defs @spec
      declare (HMI.singleton (sym @name) schema)
      mkDefs @defs @more


{-|
  Helper for defining `ToSchema` instances based on `HasJsonEncodingSpec`
  using @deriving via@.

  Example:

  > data MyType = ...
  >   deriving ToSchema via (EncodingSchema MyType)
  > instance HasJsonEncodingSchema MyType where
  >   ...
-}
newtype EncodingSchema a =
  EncodingSchema {unEncodingSchema :: a}
instance
    ( Schemaable (EncodingSpec a)
    , Typeable a
    )
  =>
    ToSchema (EncodingSchema a)
  where
    declareNamedSchema _ = do
      let (declarations, schema) = toOpenApiSchema (Proxy @(EncodingSpec a))
      declare declarations
      pure (NamedSchema Nothing schema)


{-|
  Helper for defining `ToSchema` instances based on `HasJsonDecodingSpec`
  using @deriving via@.

  Example:

  > data MyType = ...
  >   deriving ToSchema via (DecodingSchema MyType)
  > instance HasJsonDecodingSchema MyType where
  >   ...
-}
newtype DecodingSchema a =
  DecodingSchema {unDecodingSchema :: a}
instance
    ( Schemaable (DecodingSpec a)
    , Typeable a
    )
  =>
    ToSchema (DecodingSchema a)
  where
    declareNamedSchema _ = do
      let (declarations, schema) = toOpenApiSchema (Proxy @(DecodingSpec a))
      declare declarations
      pure (NamedSchema Nothing schema)


{- | Shorthand for demoting type-level strings.  -}
sym
  :: forall a b.
     ( IsString b
     , KnownSymbol a
     )
  => b
sym = fromString $ symbolVal (Proxy @a)


ref :: Text -> Referenced a
ref = OA.Ref . Reference


type family
    Concat
      (a :: [(Symbol, Specification)])
      (b :: [(Symbol, Specification)])
      :: [(Symbol, Specification)]
  where
    Concat '[] b = b
    Concat (a : more) b = a : Concat more b


