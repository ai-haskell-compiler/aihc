-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | A type for name-version-release of an RPM package
module Data.RPM.NVR (
  NVR(..),
  showNVR,
  readNVR,
  eitherNVR,
  maybeNVR,
  VerRel(..),
  )
where

import Data.Either.Extra
import Data.List.Extra

import Data.RPM.VerRel

-- | An rpm package name-version-release
data NVR = NVR {nvrName :: String,
                nvrVerRel :: VerRel}
  deriving Eq

-- | render an name-version-release
showNVR :: NVR -> String
showNVR (NVR nm verrel) = nm ++ "-" ++ showVerRel verrel

-- | Either read a package name-version-release or return a failure string
eitherNVR :: String -> Either String NVR
eitherNVR s =
  case reverse (splitOn "-" s) of
    rel:ver:emaN ->
      if null rel || null ver || null emaN
      then Left $ "NVR cannot start or end with '-'s: " ++ s
      else Right (NVR (intercalate "-" $ reverse emaN) (VerRel ver rel))
    _ ->
      Left $ "malformed NVR string: '" ++ s ++ "'"

-- | Maybe read a package name-version-release string
maybeNVR :: String -> Maybe NVR
maybeNVR = eitherToMaybe . eitherNVR

-- | read an NVR
--
-- Errors if not of the form "name-version-release"
readNVR :: String -> NVR
readNVR = either error id . eitherNVR
