{-|
The module provides a Branch type for Fedora and EPEL for active Release's.
-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Distribution.Fedora.Branch
  ( Branch(..)
  , readBranch
  , showBranch
  , eitherBranch
  , readActiveBranch
  , eitherActiveBranch
  , newerBranch
  , getActiveBranches
  , getActiveBranched
  , getLatestFedoraBranch
  , branchDestTag
  , branchDistTag
  , branchRelease
  , partitionBranches
  )
where

import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List.Extra (delete, elemIndex, replace, sortBy, spanEnd)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Down(Down))
import Data.Tuple (swap)
import Numeric.Natural (Natural)
import Safe (headDef, readMay)

import Distribution.Fedora.Release

-- | Branch datatype
--
-- Branch can be rawhide, or a fedora or epel branch
data Branch = EPELMinor !Natural !Natural | EPEL !Natural | EPELNext !Natural | Fedora !Natural | Rawhide
  deriving (Eq)

-- | defined such that: EPELNext 9 < EPEL 10 < Fedora 41 < Rawhide
instance Ord Branch where
  compare Rawhide Rawhide = EQ
  compare (Fedora m) (Fedora n) = compare m n
  compare (EPELNext m) (EPELNext n) = compare m n
  compare (EPEL m) (EPEL n) = compare m n
  compare (EPELMinor m1 n1) (EPELMinor m2 n2) =
    case compare m1 m2 of
      LT -> LT
      GT -> GT
      EQ -> compare n1 n2
  compare Rawhide _ = GT
  compare _ Rawhide = LT
  compare (Fedora _) _ = GT
  compare _ (Fedora _) = LT
  compare (EPEL m) (EPELNext n) = if m == n then LT else compare m n
  compare (EPELNext m) (EPEL n) = if m == n then GT else compare m n
  compare (EPEL m) (EPELMinor maj _) = if m >= maj then GT else LT
  compare (EPELMinor maj _) (EPEL n) = if maj <= n then LT else GT
  compare (EPELMinor _ _) (EPELNext _) = GT
  compare (EPELNext _) (EPELMinor _ _) = LT

-- | Read a Fedora Branch name, otherwise return branch string
eitherBranch :: String -> Either String Branch
eitherBranch str =
  case str of
    "" -> Left str -- error or NonEmpty?
    "rawhide" -> Right Rawhide
    "epel8-next" -> Right $ EPELNext 8
    "epel9-next" -> Right $ EPELNext 9
    _ ->
      case spanEnd isDigit str of
        (pre,ns) ->
          case readMay ns of
            Nothing -> Left str
            Just num ->
              case pre of
                   "f" -> Right $ Fedora num
                   "epel" -> Right $ EPEL num
                   "epel10." -> Right $ EPELMinor 10 num
                   "el" -> Right $ EPEL num
                   _ -> Left str

-- -- | Read a Fedora Branch name, otherwise return an error message
-- eitherBranch' :: String -> Either String Branch
-- eitherBranch' cs = case eitherBranch cs of
--   Right br -> Right br
--   Left xs -> Left $ xs ++ " is not a known Fedora/EPEL branch"

-- | Read a Fedora Branch name
readBranch :: String -> Maybe Branch
readBranch bs =
  case eitherBranch bs of
    Left _ -> Nothing
    Right br -> Just br

-- | Read a Branch name (one of the list of active branches)
--
-- Provides error strings for inactive or unknown branches.
eitherActiveBranch :: [Branch] -> String -> Either String Branch
eitherActiveBranch active bs =
  case eitherBranch bs of
    Left e -> Left e
    Right br -> if br `elem` active
                then Right br
                else Left bs

-- | Read a Branch name (one of the list of active branches)
--
-- Similar to eitherActiveBranch but ignores any error string
readActiveBranch :: [Branch] -> String -> Maybe Branch
readActiveBranch active cs =
  case eitherActiveBranch active cs of
    Left _ -> Nothing
    Right br -> Just br

-- | render Branch to String
showBranch :: Branch -> String
showBranch Rawhide = "rawhide"
showBranch (Fedora n) = "f" ++ show n
showBranch (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
showBranch (EPELNext n) = "epel" ++ show n ++ "-next"
showBranch (EPELMinor m n) = "epel" ++ show m ++ '.' : show n

-- | Get Release associated with release Branch
--
-- Fails if given an inactive branch
branchRelease :: Branch -> IO Release
branchRelease = getBranchRelease . showBranch

-- | Map Branch to Koji destination tag (candidate tag)
branchDestTag :: Branch -> IO String
branchDestTag br = releaseCandidateTag <$> branchRelease br

-- | Converts koji dist tag  to rpm %dist tag for branch
--
-- f41 -> .fc41
--
-- epel10.0 -> .el10_0
branchDistTag :: Branch -> IO String
branchDistTag br = do
  dist <- releaseDistTag <$> branchRelease br
  -- f41 -> .fc41
  -- epel10.0 -> .el10_0
  return $ '.' : (distroFix br . replace "." "_") dist
  where
    distroFix (EPEL _) = replace "epel" "el"
    distroFix (EPELNext _) = replace "epel" "el"
    distroFix (EPELMinor _ _) = replace "epel" "el"
    distroFix _ = replace "f" "fc"

-- | Returns newer branch than given one from supplied active branches.
--
-- Branches should be in descending order, eg from getFedoraBranches
newerBranch :: Branch -> [Branch] -> Maybe Branch
newerBranch Rawhide _ = Nothing
newerBranch br branches =
  case elemIndex br branches of
    Just i | i > 0 -> Just $ branches !! (i - 1)
    _ -> Nothing


--olderBranch :: Branch -> Branch
--olderBranch Rawhide = latestBranch
--olderBranch (Fedora n) = Fedora (n-1)

-- | Returns descending list of active Fedora branches, including rawhide and EPEL
getActiveBranches :: IO [Branch]
getActiveBranches =
  reverseSort . mapMaybe (readBranch . releaseBranch) <$> getActiveReleases

-- | Returns list of active Fedora branches, excluding rawhide
getActiveBranched :: IO [Branch]
getActiveBranched = delete Rawhide <$> getActiveBranches

-- from simple-cmd
error' :: String -> a
error' = errorWithoutStackTrace

-- | separate fedora branches from rest of args
partitionBranches :: [String] -> ([Branch],[String])
partitionBranches args =
  swap . partitionEithers $ map eitherBranch args

-- | get newest Fedora branched Release
getLatestFedoraBranch :: IO Branch
getLatestFedoraBranch =
  headDef (error' "no active branched!") <$> getActiveBranched

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy (comparing Down)
