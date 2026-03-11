{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}

module Schemas.HeroSchema (heroSchema) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (throwM)
import Language.GraphQL.Error (ResolverException (..))
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type as Type
import Language.GraphQL.Type.Schema (schemaWithTypes)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable (cast)
import qualified Language.GraphQL.Type.Out as Out

data HeroException = HeroException
    deriving Show

instance Exception HeroException where
    toException = toException. ResolverException
    fromException e = do
        ResolverException resolverException <- fromException e
        cast resolverException

heroSchema :: Type.Schema IO
heroSchema =
    schemaWithTypes Nothing queryType Nothing Nothing [] mempty

type ObjectType = Out.ObjectType IO

queryType :: ObjectType
queryType = Out.ObjectType "Query" Nothing []
    $ HashMap.fromList
        [ ("hero", Out.ValueResolver heroField heroResolver)
        ]
  where
    heroField = Out.Field Nothing (Out.NamedObjectType heroType)
        $ HashMap.singleton "id"
        $ In.Argument Nothing (In.NamedScalarType Type.id) Nothing
    heroResolver = pure $ Type.Object mempty

stringField :: Out.Field IO
stringField = Out.Field Nothing (Out.NonNullScalarType Type.string) HashMap.empty

heroType :: ObjectType
heroType = Out.ObjectType "Hero" Nothing [] $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("id", Out.ValueResolver stringField (pure $ Type.String "4111"))
        , ("name", Out.ValueResolver stringField (pure $ Type.String "R2D2"))
        , ("friends", Out.ValueResolver friendsField (pure $ Type.List [luke]))
        ]
    friendsField = Out.Field Nothing (Out.ListType $ Out.NonNullObjectType lukeType) HashMap.empty
    -- This list values are ignored because of current realisation (types and resolvers are the same entity)
    -- The values from lukeType will be used
    luke = Type.Object $ HashMap.fromList
        [ ("id", "dfdfdf")
        , ("name", "dfdfdff")
        ]

lukeType :: ObjectType
lukeType = Out.ObjectType "Luke" Nothing [] $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("id", Out.ValueResolver stringField (pure $ Type.String "1000"))
        , ("name", Out.ValueResolver stringField (throwM HeroException))
        ]
