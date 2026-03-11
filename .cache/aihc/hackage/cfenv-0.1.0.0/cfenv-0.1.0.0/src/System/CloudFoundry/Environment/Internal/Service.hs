module System.CloudFoundry.Environment.Internal.Service where

import qualified Data.Map.Strict as Map
import System.CloudFoundry.Environment.Internal.Types

-- |Get a credential string from a service.
credentialString :: String       -- ^ The key to each for in the credentials section
                 -> Service      -- ^ The service to get the value from
                 -> Maybe String -- ^ The value of that credential string if it is found
credentialString key = Map.lookup key . credentials