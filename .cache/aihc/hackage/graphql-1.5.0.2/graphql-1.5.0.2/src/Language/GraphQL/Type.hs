{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

-- | Reexports non-conflicting type system and schema definitions.
module Language.GraphQL.Type
    ( In.InputField(..)
    , In.InputObjectType(..)
    , Out.Context(..)
    , Out.Field(..)
    , Out.InterfaceType(..)
    , Out.ObjectType(..)
    , Out.Resolve
    , Out.Resolver(..)
    , Out.SourceEventStream
    , Out.Subscribe
    , Out.UnionType(..)
    , Out.argument
    , module Language.GraphQL.Type.Definition
    , module Language.GraphQL.Type.Schema
    ) where

import Language.GraphQL.Type.Definition
import Language.GraphQL.Type.Schema (Schema, schema, schemaWithTypes)
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
