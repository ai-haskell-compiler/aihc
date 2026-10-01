-- | Re-exports the class and the type of @provider@ without their instance
-- module being a dependency of the package that imports this one.
module Middle (Describe (..), Token, token) where

import Provider (Describe (..), Token (..))

token :: Token
token = Token
