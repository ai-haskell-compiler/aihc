{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Types and functions used for input and result coercion.
module Language.GraphQL.Execute.Coerce
    ( Output(..)
    , Serialize(..)
    , VariableValue(..)
    , coerceInputLiteral
    , matchFieldValues
    ) where

import Data.Int (Int32)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder
import Language.GraphQL.AST (Name)
import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Execute.OrderedMap as OrderedMap
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out

-- | Since variables are passed separately from the query, in an independent
-- format, they should be first coerced to the internal representation used by
-- this implementation.
class VariableValue a where
    -- | Only a basic, format-specific, coercion must be done here. Type
    -- correctness or nullability shouldn't be validated here, they will be
    -- validated later. The type information is provided only as a hint.
    --
    -- For example @GraphQL@ prohibits the coercion from a 't:Float' to an
    -- 't:Int', but @JSON@ doesn't have integers, so whole numbers should be
    -- coerced to 't:Int` when receiving variables as a JSON object. The same
    -- holds for 't:Enum'. There are formats that support enumerations, @JSON@
    -- doesn't, so the type information is given and 'coerceVariableValue' can
    -- check that an 't:Enum' is expected and treat the given value
    -- appropriately. Even checking whether this value is a proper member of the
    -- corresponding 't:Enum' type isn't required here, since this can be
    -- checked independently.
    --
    -- Another example is an @ID@. @GraphQL@ explicitly allows to coerce
    -- integers and strings to @ID@s, so if an @ID@ is received as an integer,
    -- it can be left as is and will be coerced later.
    --
    -- If a value cannot be coerced without losing information, 'Nothing' should
    -- be returned, the coercion will fail then and the query won't be executed.
    coerceVariableValue
        :: In.Type -- ^ Expected type (variable type given in the query).
        -> a -- ^ Variable value being coerced.
        -> Maybe Type.Value -- ^ Coerced value on success, 'Nothing' otherwise.

instance VariableValue Type.Value where
    coerceVariableValue _ Type.Null = Just Type.Null
    coerceVariableValue (In.ScalarBaseType _) value = Just value
    coerceVariableValue (In.EnumBaseType _) (Type.Enum stringValue) =
        Just $ Type.Enum stringValue
    coerceVariableValue (In.InputObjectBaseType objectType) value
        | (Type.Object objectValue) <- value = do
            let (In.InputObjectType _ _ inputFields) = objectType
            (newObjectValue, resultMap) <- foldWithKey objectValue inputFields
            if HashMap.null newObjectValue
                then Just $ Type.Object resultMap
                else Nothing
      where
        foldWithKey objectValue = HashMap.foldrWithKey matchFieldValues'
            $ Just (objectValue, HashMap.empty)
        matchFieldValues' _ _ Nothing = Nothing
        matchFieldValues' fieldName inputField (Just (objectValue, resultMap)) =
            let (In.InputField _ fieldType _) = inputField
                insert = flip (HashMap.insert fieldName) resultMap
                newObjectValue = HashMap.delete fieldName objectValue
             in case HashMap.lookup fieldName objectValue of
                    Just variableValue -> do
                        coerced <- coerceVariableValue fieldType variableValue
                        pure (newObjectValue, insert coerced)
                    Nothing -> Just (objectValue, resultMap)
    coerceVariableValue (In.ListBaseType listType) value
        | (Type.List arrayValue) <- value =
            Type.List <$> traverse (coerceVariableValue listType) arrayValue
        | otherwise = coerceVariableValue listType value
    coerceVariableValue _ _ = Nothing

-- | Looks up a value by name in the given map, coerces it and inserts into the
-- result map. If the coercion fails, returns 'Nothing'. If the value isn't
-- given, but a default value is known, inserts the default value into the
-- result map. Otherwise it fails with 'Nothing' if the Input Type is a
-- Non-Nullable type, or returns the unchanged, original map.
matchFieldValues :: forall a
    . (In.Type -> a -> Maybe Type.Value)
    -> HashMap Name a
    -> Name
    -> In.Type
    -> Maybe Type.Value
    -> Maybe (HashMap Name Type.Value)
    -> Maybe (HashMap Name Type.Value)
matchFieldValues coerce values' fieldName type' defaultValue resultMap =
    case HashMap.lookup fieldName values' of
        Just variableValue -> coerceRuntimeValue $ coerce type' variableValue
        Nothing
            | Just value <- defaultValue ->
                HashMap.insert fieldName value <$> resultMap
            | Nothing <- defaultValue
            , In.isNonNullType type' -> Nothing
            | otherwise -> resultMap
  where
    coerceRuntimeValue (Just Type.Null)
        | In.isNonNullType type' = Nothing
    coerceRuntimeValue coercedValue =
        HashMap.insert fieldName <$> coercedValue <*> resultMap

-- | Coerces operation arguments according to the input coercion rules for the
-- corresponding types.
coerceInputLiteral :: In.Type -> Type.Value -> Maybe Type.Value
coerceInputLiteral (In.isNonNullType -> False) Type.Null = Just Type.Null
coerceInputLiteral (In.ScalarBaseType type') value
    | (Type.String stringValue) <- value
    , (Type.ScalarType "String" _) <- type' = Just $ Type.String stringValue
    | (Type.Boolean booleanValue) <- value
    , (Type.ScalarType "Boolean" _) <- type' = Just $ Type.Boolean booleanValue
    | (Type.Int intValue) <- value
    , (Type.ScalarType "Int" _) <- type' = Just $ Type.Int intValue
    | (Type.Float floatValue) <- value
    , (Type.ScalarType "Float" _) <- type' = Just $ Type.Float floatValue
    | (Type.Int intValue) <- value
    , (Type.ScalarType "Float" _) <- type' =
        Just $ Type.Float $ fromIntegral intValue
    | (Type.String stringValue) <- value
    , (Type.ScalarType "ID" _) <- type' = Just $ Type.String stringValue
    | (Type.Int intValue) <- value
    , (Type.ScalarType "ID" _) <- type' = Just $ decimal intValue
  where
    decimal = Type.String
        . Text.Lazy.toStrict
        . Text.Builder.toLazyText
        . Text.Builder.decimal
coerceInputLiteral (In.EnumBaseType type') (Type.Enum enumValue)
    | member enumValue type' = Just $ Type.Enum enumValue
  where
    member value (Type.EnumType _ _ members) = HashMap.member value members
coerceInputLiteral (In.InputObjectBaseType type') (Type.Object values) =
    let (In.InputObjectType _ _ inputFields) = type'
     in Type.Object
            <$> HashMap.foldrWithKey (matchFieldValues' values) (Just HashMap.empty) inputFields
  where
    matchFieldValues' values' fieldName (In.InputField _ inputFieldType defaultValue) =
        matchFieldValues coerceInputLiteral values' fieldName inputFieldType defaultValue
coerceInputLiteral (In.ListBaseType listType) (Type.List list) =
    Type.List <$> traverse (coerceInputLiteral listType) list
coerceInputLiteral (In.ListBaseType listType) singleton =
    wrapSingleton listType singleton
  where
      wrapSingleton (In.ListBaseType listType') singleton' =
          Type.List <$> sequence [wrapSingleton listType' singleton']
      wrapSingleton listType' singleton' =
          Type.List <$> sequence [coerceInputLiteral listType' singleton']
coerceInputLiteral _ _ = Nothing

-- | 'Serialize' describes how a @GraphQL@ value should be serialized.
class Serialize a where
    -- | Serializes a @GraphQL@ value according to the given serialization
    -- format.
    --
    -- Type infomration is given as a hint, e.g. if you need to know what type
    -- is being serialized to serialize it properly. Don't do any validation for
    -- @GraphQL@ built-in types here.
    --
    -- If the value cannot be serialized without losing information, return
    -- 'Nothing' — it will cause a field error.
    serialize :: forall m
        . Out.Type m -- ^ Expected output type.
        -> Output a -- ^ The value to be serialized.
        -> Maybe a -- ^ Serialized value on success or 'Nothing'.
    -- | __null__ representation in the given serialization format.
    null :: a

-- | Intermediate type used to serialize a @GraphQL@ value.
--
-- The serialization is done during the execution, and 'Output' contains
-- already serialized data (in 'List' and 'Object') as well as the new layer
-- that has to be serialized in the current step. So 'Output' is parameterized
-- by the serialization format.
data Output a
    = Int Int32
    | Float Double
    | String Text
    | Boolean Bool
    | Enum Name
    | List [a]
    | Object (OrderedMap a)
    deriving (Eq, Show)

instance forall a. IsString (Output a) where
    fromString = String . fromString

instance Serialize Type.Value where
    null = Type.Null
    serialize (Out.ScalarBaseType scalarType) value
        | Type.ScalarType "Int" _ <- scalarType
        , Int int <- value = Just $ Type.Int int
        | Type.ScalarType "Float" _ <- scalarType
        , Float float <- value = Just $ Type.Float float
        | Type.ScalarType "String" _ <- scalarType
        , String string <- value = Just $ Type.String string
        | Type.ScalarType "ID" _ <- scalarType
        , String string <- value = Just $ Type.String string
        | Type.ScalarType "Boolean" _ <- scalarType
        , Boolean boolean <- value = Just $ Type.Boolean boolean
    serialize _ (Enum enum) = Just $ Type.Enum enum
    serialize _ (List list) = Just $ Type.List list
    serialize _ (Object object) = Just
        $ Type.Object
        $ HashMap.fromList
        $ OrderedMap.toList object
    serialize _ _ = Nothing
