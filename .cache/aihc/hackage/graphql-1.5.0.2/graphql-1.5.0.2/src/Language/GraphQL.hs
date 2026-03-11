{-# LANGUAGE RecordWildCards #-}

-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Language.GraphQL
    ( graphql
    ) where

import Control.Monad.Catch (MonadCatch)
import Data.HashMap.Strict (HashMap)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.AST as Full
import Language.GraphQL.Error
import Language.GraphQL.Execute
import qualified Language.GraphQL.Validate as Validate
import Language.GraphQL.Type.Schema (Schema)
import Prelude hiding (null)
import Text.Megaparsec (parse)

-- | If the text parses correctly as a @GraphQL@ query the query is
-- executed using the given 'Schema'.
--
-- An operation name can be given if the document contains multiple operations.
graphql :: (MonadCatch m, VariableValue a, Serialize b)
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> HashMap Full.Name a -- ^ Variable substitution function.
    -> Text -- ^ Text representing a @GraphQL@ request document.
    -> m (Either (ResponseEventStream m b) (Response b)) -- ^ Response.
graphql schema operationName variableValues document' =
    case parse Full.document "" document' of
        Left errorBundle -> pure  <$> parseError errorBundle
        Right parsed ->
            case validate parsed of
                Seq.Empty -> execute schema operationName variableValues parsed
                errors -> pure $ pure
                    $ Response null
                    $ fromValidationError <$> errors
  where
    validate = Validate.document schema Validate.specifiedRules
    fromValidationError Validate.Error{..} = Error
        { message = Text.pack message
        , locations = locations
        , path = []
        }
