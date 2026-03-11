-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | An NV type contains the name and version of a package only.
module Data.RPM.NV (
  NV(..),
  showNV,
  readNV,
  eitherNV,
  maybeNV
  )
where

import Data.Either.Extra
import Data.List.Extra

-- | Package name-version type
data NV = NV {name :: String,
              version :: String}
  deriving (Eq)

-- | Render an name-version
showNV :: NV -> String
showNV (NV nm ver) = nm ++ "-" ++ ver

-- | Read a package name-version or return a failure string
eitherNV :: String -> Either String NV
eitherNV "" = Left "NV cannot be empty string"
eitherNV s =
  case stripInfixEnd "-" s of
    Just (n,v) ->
      if null n || null v
      then Left $ "NV must not start or end with '-': " ++ s
      else Right (NV n v)
    Nothing -> Left $ "NV must contain '-': " ++ s

-- | Maybe read an package name-version
maybeNV :: String -> Maybe NV
maybeNV = eitherToMaybe . eitherNV

-- | Read an NV
--
-- Errors if not of the form "name-version"
readNV :: String -> NV
readNV =  either error id . eitherNV
