{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | JSON serialization.
module Language.GraphQL.JSON
    ( JSON(..)
    , graphql
    ) where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Language.GraphQL as GraphQL
import Language.GraphQL.AST (Location(..), Name)
import Language.GraphQL.Error
import Language.GraphQL.Type.Schema (Schema)
import Data.Bifunctor (Bifunctor(..))
import qualified Conduit
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Language.GraphQL.Execute.Coerce
import qualified Language.GraphQL.Execute.OrderedMap as OrderedMap
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import qualified Language.GraphQL.Type as Type

-- | Wraps an aeson value.
newtype JSON = JSON Aeson.Value

instance Aeson.ToJSON JSON where
    toJSON (JSON value) = value

instance Aeson.FromJSON JSON where
    parseJSON = pure . JSON

instance Serialize JSON where
    serialize (Out.ScalarBaseType scalarType) value
        | Type.ScalarType "Int" _ <- scalarType
        , Int int <- value = Just $ JSON $ Aeson.Number $ fromIntegral int
        | Type.ScalarType "Float" _ <- scalarType
        , Float float <- value = Just $ JSON $ Aeson.toJSON float
        | Type.ScalarType "String" _ <- scalarType
        , String string <- value = Just $ JSON $ Aeson.String string
        | Type.ScalarType "ID" _ <- scalarType
        , String string <- value = Just $ JSON $ Aeson.String string
        | Type.ScalarType "Boolean" _ <- scalarType
        , Boolean boolean <- value = Just $ JSON $ Aeson.Bool boolean
    serialize _ (Enum enum) = Just $ JSON $ Aeson.String enum
    serialize _ (List list) = Just $ JSON $ Aeson.toJSON list
    serialize _ (Object object) = Just
        $ JSON
        $ Aeson.object
        $ toJSONKeyValue <$> OrderedMap.toList object
      where
        toJSONKeyValue (key, value) = (Aeson.Key.fromText key, Aeson.toJSON value)
    serialize _ _ = Nothing
    null = JSON Aeson.Null

instance VariableValue JSON where
    coerceVariableValue _ (JSON Aeson.Null) = Just Type.Null
    coerceVariableValue (In.ScalarBaseType scalarType) (JSON value)
        | (Aeson.String stringValue) <- value = Just $ Type.String stringValue
        | (Aeson.Bool booleanValue) <- value = Just $ Type.Boolean booleanValue
        | (Aeson.Number numberValue) <- value
        , (Type.ScalarType "Float" _) <- scalarType =
            Just $ Type.Float $ toRealFloat numberValue
        | (Aeson.Number numberValue) <- value = -- ID or Int
            Type.Int <$> toBoundedInteger numberValue
    coerceVariableValue (In.EnumBaseType _) (JSON (Aeson.String stringValue)) =
        Just $ Type.Enum stringValue
    coerceVariableValue (In.InputObjectBaseType objectType) (JSON value)
        | (Aeson.Object objectValue) <- value = do
            let (In.InputObjectType _ _ inputFields) = objectType
            (newObjectValue, resultMap) <- foldWithKey objectValue inputFields
            if KeyMap.null newObjectValue
                then Just $ Type.Object resultMap
                else Nothing
      where
        foldWithKey :: Aeson.Object
            -> HashMap Name In.InputField
            -> Maybe (Aeson.Object, HashMap Name Type.Value)
        foldWithKey objectValue = HashMap.foldrWithKey matchFieldValues'
            $ Just (objectValue, HashMap.empty)
        matchFieldValues' :: Text
            -> In.InputField
            -> Maybe (Aeson.Object, HashMap Name Type.Value)
            -> Maybe (Aeson.Object, HashMap Name Type.Value)
        matchFieldValues' _ _ Nothing = Nothing
        matchFieldValues' fieldName inputField (Just (objectValue, resultMap)) =
            let fieldKey = Aeson.Key.fromText fieldName
                In.InputField _ fieldType _ = inputField
                insert = flip (HashMap.insert fieldName) resultMap
                newObjectValue = KeyMap.delete fieldKey objectValue
             in case KeyMap.lookup fieldKey objectValue of
                    Just variableValue -> do
                        coerced <- coerceVariableValue fieldType
                            $ JSON variableValue
                        pure (newObjectValue, insert coerced)
                    Nothing -> Just (objectValue, resultMap)
    coerceVariableValue (In.ListBaseType listType) (JSON value)
        | (Aeson.Array arrayValue) <- value =
            Type.List <$> foldr foldVector (Just []) arrayValue
        | otherwise = coerceVariableValue listType $ JSON value
      where
        foldVector _ Nothing = Nothing
        foldVector variableValue (Just list) = do
            coerced <- coerceVariableValue listType $ JSON variableValue
            pure $ coerced : list
    coerceVariableValue _ _ = Nothing

-- | If the text parses correctly as a @GraphQL@ query the query is
-- executed using the given 'Schema'.
graphql :: MonadCatch m
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> Aeson.Object -- ^ Variables.
    -> Text -- ^ Text representing a @GraphQL@ request document.
    -> m (Either (ResponseEventStream m Aeson.Value) Aeson.Object) -- ^ Response.
graphql schema operationName variableValues = fmap (bimap stream formatResponse)
    . GraphQL.graphql schema operationName jsonVariables
  where
    jsonVariables = JSON <$> KeyMap.toHashMapText variableValues
    -- stream :: ResponseEventStream m JSON -> ResponseEventStream m Aeson.Value
    stream = Conduit.mapOutput mapResponse
    mapResponse response@Response{ data' = JSON json } =
        response{ data' = json }
    formatResponse :: Response JSON -> Aeson.Object
    formatResponse Response{ errors, data' = JSON json } =
        let dataResponse = KeyMap.singleton "data" json
         in case errors of
              Seq.Empty -> dataResponse
              _ -> flip (KeyMap.insert "errors") dataResponse
                  $ Aeson.Array $ foldr fromError mempty errors
    fromError :: Error -> Vector Aeson.Value -> Vector Aeson.Value
    fromError Error{..} = Vector.cons $ Aeson.object $ catMaybes
        [ Just ("message", Aeson.String message)
        , toMaybe fromLocation "locations" locations
        , toMaybe fromPath "path" path
        ]
    fromPath (Segment segment) = Aeson.String segment
    fromPath (Index index) = Aeson.toJSON index
    fromLocation Location{..} = Aeson.object
        [ ("line", Aeson.toJSON line)
        , ("column", Aeson.toJSON column)
        ]
    toMaybe _ _ [] = Nothing
    toMaybe f key xs = Just (key, Aeson.listValue f xs)
