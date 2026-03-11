{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

-- | Error handling.
module Language.GraphQL.Error
    ( Error(..)
    , Path(..)
    , ResolverException(..)
    , Response(..)
    , ResponseEventStream
    , parseError
    ) where

import Conduit
import Control.Exception (Exception(..))
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST (Location(..))
import Language.GraphQL.Execute.Coerce
import Prelude hiding (null)
import Text.Megaparsec
    ( ParseErrorBundle(..)
    , PosState(..)
    , SourcePos(..)
    , errorOffset
    , parseErrorTextPretty
    , reachOffset
    , unPos
    )

-- | Wraps a parse error into a list of errors.
parseError :: (Applicative f, Serialize a)
    => ParseErrorBundle Text Void
    -> f (Response a)
parseError ParseErrorBundle{..}  =
    pure $ Response null $ fst
        $ foldl go (Seq.empty, bundlePosState) bundleErrors
  where
    errorObject s SourcePos{..} = Error
        { message = Text.pack $ init $ parseErrorTextPretty s
        , locations = [Location (unPos' sourceLine) (unPos' sourceColumn)]
        , path = []
        }
    unPos' = fromIntegral . unPos
    go (result, state) x =
        let (_, newState) = reachOffset (errorOffset x) state
            sourcePosition = pstateSourcePos newState
         in (result |> errorObject x sourcePosition, newState)

-- | If an error can be associated to a particular field in the GraphQL result,
-- it must contain an entry with the key path that details the path of the
-- response field which experienced the error. This allows clients to identify
-- whether a null result is intentional or caused by a runtime error.
data Path
    = Segment Text -- ^ Field name.
    | Index Int -- ^ List index if a field returned a list.
    deriving (Eq, Show)

-- | @GraphQL@ error.
data Error = Error
    { message :: Text
    , locations :: [Location]
    , path :: [Path]
    } deriving (Eq, Show)

-- | The server\'s response describes the result of executing the requested
-- operation if successful, and describes any errors encountered during the
-- request.
data Response a = Response
    { data' :: a
    , errors :: Seq Error
    } deriving (Eq, Show)

-- | Each event in the underlying Source Stream triggers execution of the
-- subscription selection set. The results of the execution generate a Response
-- Stream.
type ResponseEventStream m a = ConduitT () (Response a) m ()

-- | Only exceptions that inherit from 'ResolverException' a cought by the
-- executor.
data ResolverException = forall e. Exception e => ResolverException e

instance Show ResolverException where
    show (ResolverException e) = show e

instance Exception ResolverException
