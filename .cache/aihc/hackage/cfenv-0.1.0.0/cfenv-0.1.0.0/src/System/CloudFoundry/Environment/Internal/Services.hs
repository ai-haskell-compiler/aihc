module System.CloudFoundry.Environment.Internal.Services
  ( withLabel
  , withName
  , withTag
  ) where

import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)

import System.CloudFoundry.Environment.Internal.Types

-- |Get all services which have the provided tag.
withTag :: String    -- ^ The tag to search for
        -> Services  -- ^ All services bound to the application
        -> [Service] -- ^ A list of matching services
withTag searchTag = filter (elem searchTag . tags) . allServices

-- |Get the service by name.
withName :: String        -- ^ The name of the service to be found
         -> Services      -- ^ All services bound to the application
         -> Maybe Service -- ^ The service if it is found
withName searchName = listToMaybe . filter ((== searchName) . name) . allServices

-- |Get the services by label.
withLabel :: String    -- ^ The label to search for
          -> Services  -- ^ All services bound to the application
          -> [Service] -- ^ A list of matching services
withLabel searchLabel (Services svcs) =
    fromMaybe [] $ Map.lookup searchLabel svcs

allServices :: Services -> [Service]
allServices (Services svcs) = join $ Map.elems svcs