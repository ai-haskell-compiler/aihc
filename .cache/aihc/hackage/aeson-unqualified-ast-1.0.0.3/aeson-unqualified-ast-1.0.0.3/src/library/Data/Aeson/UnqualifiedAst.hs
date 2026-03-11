-- | Definitions of JSON AST in terms of the \"aeson\" package.
--
-- This module is intended to be imported unqualified.
-- It is obliged by the package policy to never pollute the namespace.
--
-- The naming policy that we use here:
--
-- - Constructors are to be suffixed with the type name. Read the name as answering the question of what instance of the type it is. E.g., 'ObjectJson', standing for an object kind of JSON.
-- - Subtypes are to be prefixed with the parent type. Read the name as a path. E.g., 'JsonObject' stands for @Json/Object@.
--
-- This policy is pretty universal and it has been applied successfully to disambiguate very large models.
module Data.Aeson.UnqualifiedAst where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Bool (Bool)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

-- * Json type

-- |
-- Alias to JSON AST representation in terms of the \"aeson\" package.
--
-- Together with the associated pattern synonym definitions that are defined in this module
-- this type provides virtually the same API as the following definition:
--
-- > data Json
-- >   = ObjectJson (KeyMap Json)
-- >   | ArrayJson (Vector Json)
-- >   | StringJson Text
-- >   | NumberJson Scientific
-- >   | BoolJson Bool
-- >   | NullJson
--
-- At the same time being just an alias it is completely interchangeable with the 'Aeson.Value' definition from \"aeson\" at zero runtime cost.
type Json = Aeson.Value

-- ** Json pattern synonyms

-- | Alias to the v'Aeson.Object' constructor and its pattern.
pattern ObjectJson :: JsonObject -> Json
pattern ObjectJson object = Aeson.Object object

-- | Alias to the v'Aeson.Array' constructor and its pattern.
pattern ArrayJson :: JsonArray -> Json
pattern ArrayJson array = Aeson.Array array

-- | Alias to the v'Aeson.String' constructor and its pattern.
pattern StringJson :: Text -> Json
pattern StringJson text = Aeson.String text

-- | Alias to the v'Aeson.Number' constructor and its pattern.
pattern NumberJson :: Scientific -> Json
pattern NumberJson scientific = Aeson.Number scientific

-- | Alias to the v'Aeson.Bool' constructor and its pattern.
pattern BoolJson :: Bool -> Json
pattern BoolJson bool = Aeson.Bool bool

-- | Alias to the v'Aeson.Null' constructor and its pattern.
pattern NullJson :: Json
pattern NullJson = Aeson.Null

{-# COMPLETE ObjectJson, ArrayJson, StringJson, NumberJson, BoolJson, NullJson #-}

-- * Other types

-- | Alias to JSON Object AST.
type JsonObject = AesonKeyMap.KeyMap Json

-- | Alias to key for 'JsonObject'.
type JsonObjectKey = AesonKeyMap.Key

-- | Alias to JSON Array AST.
type JsonArray = Vector Json
