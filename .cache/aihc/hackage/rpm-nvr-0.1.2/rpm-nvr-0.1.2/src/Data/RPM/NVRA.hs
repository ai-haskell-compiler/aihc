{-# LANGUAGE CPP #-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | A type for a (binary) RPM package.
module Data.RPM.NVRA (
  NVRA(..),
  readNVRA,
  eitherNVRA,
  maybeNVRA,
  showNVRA,
  showPkgIdent,
  showPkgVerRel,
  )
where

import Data.Either.Extra
import Data.List.Extra
#if !MIN_VERSION_extra(1,6,4)
import Data.Maybe (fromMaybe)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR
import Data.RPM.VerRel
import System.FilePath (takeFileName)

-- | RPM package with name, version-release, and architecture
--
-- If arch is not needed use NVR instead.
--
-- FIXME: add epoch field
data NVRA = NVRA {rpmName :: String,
                  rpmVerRel :: VerRel,
                  rpmArch :: String}
  deriving (Eq, Ord)

-- | Render an RpmPackage
showNVRA :: NVRA -> String
showNVRA (NVRA n vr a) = n <> "-" <> showVerRel vr <> "." <> a

-- | Either read a name-version-release.arch or return a failure string
--
-- Strips off ".rpm" extension and any directory path prefix.
eitherNVRA :: String -> Either String NVRA
eitherNVRA "" = Left "NVRA string cannot be empty"
eitherNVRA s@('-':_) = Left $ "NVRA cannot start with '-': " ++ s
eitherNVRA s =
  let nvra = dropSuffix ".rpm" $ takeFileName s
  in
    case reverse (splitOn "-" nvra) of
      relarch:ver:emaN ->
        if null relarch || null ver || null emaN
        then Left $ "Bad NVRA string: " ++ s
        else
          case breakOnEnd "." relarch of
            ("",_) -> Left $ "No arch suffix for " ++ s
            (_,"") -> Left $ "Package release should not end in '.' " ++ s
            (reldot,arch) ->
              Right $ NVRA (intercalate "-" $ reverse emaN) (VerRel ver (dropEnd 1 reldot)) arch
      _ -> Left $ "NVRA string must have form 'name-version-release.arch': " ++ s

-- | Maybe read an NVRA
maybeNVRA :: String -> Maybe NVRA
maybeNVRA = eitherToMaybe . eitherNVRA

-- | Parse an NVRA with arch suffix
--
-- Errors if not of the form "name-version-release[.arch]"
readNVRA :: String -> NVRA
readNVRA = either error id . eitherNVRA

-- | Render the version-release of an NVRA
showPkgVerRel :: NVRA -> String
showPkgVerRel = showVerRel . rpmVerRel

-- | Identifier for an RPM package identified by name and arch
showPkgIdent :: NVRA -> String
showPkgIdent p = rpmName p <> "." <> rpmArch p

#if !MIN_VERSION_extra(1,6,4)
dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix a b = fromMaybe b $ stripSuffix a b
#endif
