-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | A type for carrying the version and release of an rpm package.
module Data.RPM.VerRel (
  VerRel(..),
  showVerRel,
  readVerRel,
  eitherVerRel,
  maybeVerRel
  )
where

import Data.Either.Extra
import Data.List.Extra

import Data.RPM.VerCmp

-- | The version-release of an (rpm) package, which is ordered by
-- @rpmVerCompare@ for version and release.
--
-- FIXME: note currently rpmVerCompare is not used for Eq (like codec-rpm).
data VerRel = VerRel {vrVersion :: String,
                      vrRelease :: String}
  deriving (Eq)

-- | Display a VerRel
showVerRel :: VerRel -> String
showVerRel (VerRel ver rel) = ver ++ "-" ++ rel

-- | Either read a package version-release or return an failure string
eitherVerRel :: String -> Either String VerRel
eitherVerRel s =
  case stripInfixEnd "-" s of
    Just (v,r) ->
      if null v || null r
      then error $ "VerRel cannot start or end with '-': " ++ s
      else Right (VerRel v r)
    Nothing -> Left $ "VerRel must contain a '-': " ++ s

-- | Maybe read a package version-release
maybeVerRel :: String -> Maybe VerRel
maybeVerRel = eitherToMaybe . eitherVerRel

-- | Read a version-release
--
-- Errors if malformed
readVerRel :: String -> VerRel
readVerRel = either error id . eitherVerRel

instance Ord VerRel where
  compare (VerRel v1 r1) (VerRel v2 r2) =
    case rpmVerCompare v1 v2 of
      EQ -> rpmVerCompare r1 r2
      o -> o

-- appendRelease :: VerRel -> String -> VerRel
-- appendRelease (VerRel v r) d = VerRel v (r ++ d)
