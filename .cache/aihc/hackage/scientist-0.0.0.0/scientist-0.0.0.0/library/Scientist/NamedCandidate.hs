module Scientist.NamedCandidate
  ( NamedCandidate
  , namedCandidate
  , namedCandidateName
  , runNamedCandidate
  ) where

import Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Scientist.Candidate
import UnliftIO.Exception (SomeException, tryAny)

data NamedCandidate m a = NamedCandidate Text (m (Candidate a))

namedCandidate :: Text -> m (Candidate a) -> NamedCandidate m a
namedCandidate = NamedCandidate

namedCandidateName :: NamedCandidate m a -> Text
namedCandidateName (NamedCandidate x _) = x

runNamedCandidate
  :: MonadUnliftIO m => NamedCandidate m a -> m (Either SomeException a)
runNamedCandidate (NamedCandidate _ f) = tryAny $ unCandidate <$> f
