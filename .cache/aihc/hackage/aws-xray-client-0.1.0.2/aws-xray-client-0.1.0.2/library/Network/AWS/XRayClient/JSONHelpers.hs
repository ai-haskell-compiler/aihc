module Network.AWS.XRayClient.JSONHelpers
  ( xrayAesonOptions
  ) where

import Prelude

import Data.Aeson
import qualified Data.Char as C
import Data.List (isPrefixOf)

-- | Aeson 'Options' to remove prefix and apply snake_case.
xrayAesonOptions :: String -> Options
xrayAesonOptions prefix = defaultOptions
  { fieldLabelModifier = snakeCaseify . unCapitalize . dropPrefix prefix
  , omitNothingFields = True
  }

-- | Lower-case and insert an underscore before upper-case letters
--
-- >>> snakeCaseify "levelSubtraction"
-- "level_subtraction"
--
snakeCaseify :: String -> String
snakeCaseify [] = []
snakeCaseify (c : cs)
  | C.isLower c || C.isNumber c = c : snakeCaseify cs
  | otherwise = '_' : C.toLower c : snakeCaseify cs

-- | Lower-case leading character
--
-- >>> unCapitalize "Capped"
-- "capped"
--
unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c : cs) = C.toLower c : cs

dropPrefix :: String -> String -> String
dropPrefix prefix x = if prefix `isPrefixOf` x
  then drop (length prefix) x
  else error $ "dropPrefix: " ++ show prefix ++ " is not a prefix of " ++ show x
