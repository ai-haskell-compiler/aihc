{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions and exceptions to write resolvers.
module Language.GraphQL.Resolver
    ( ServerException(..)
    , argument
    , defaultResolver
    ) where

import Control.Monad.Catch (Exception(..), MonadCatch(..), MonadThrow(..))
import Control.Monad.Trans.Reader (ReaderT, asks)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (cast)
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Error
import qualified Language.GraphQL.Type as Type
import Language.GraphQL.Class (FromGraphQL(..))

-- | Exceptions thrown by the functions in this module.
data ServerException
    = FieldNotResolvedException !Text
    | ErroneousArgumentTypeException !Text

instance Show ServerException where
    show (FieldNotResolvedException fieldName) =
        Text.unpack $ Text.unwords ["Field", fieldName, "not resolved."]
    show (ErroneousArgumentTypeException argumentName) =
        Text.unpack $ Text.unwords
            [ "Unable to convert the argument"
            , argumentName
            , "to a user-defined type."
            ]

instance Exception ServerException where
    toException = toException . ResolverException
    fromException x = do
        ResolverException a <- fromException x
        cast a

-- | Default resolver expects that the field value is returned by the parent
-- object. If the parent is not an object or it doesn't contain the requested
-- field name, an error is thrown.
defaultResolver :: MonadCatch m => Name -> Type.Resolve m
defaultResolver fieldName = do
    values' <- asks Type.values
    case values' of
        Type.Object objectValue
            | Just result <- HashMap.lookup fieldName objectValue -> pure result
        _nonObject -> throwM $ FieldNotResolvedException fieldName

-- | Takes an argument name, validates that the argument exists, and optionally
-- converts it to a user-defined type.
argument :: (MonadCatch m, FromGraphQL a) => Name -> ReaderT Type.Context m a
argument argumentName =
    Type.argument argumentName >>= maybe throwError pure . fromGraphQL
  where
    throwError = throwM $ ErroneousArgumentTypeException argumentName
