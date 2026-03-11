{-# LANGUAGE OverloadedStrings #-}

module Types (
  Mirror(..),
  Natural,
  RepoSource(..),
  Verbosity(..),
  Release(..),
  eitherRelease
  ) where

import Data.Char (isDigit)
import Data.List.Extra (lower, unsnoc)
import Numeric.Natural

--import Distribution.Fedora.Repoquery

data Verbosity = Quiet | Normal | Verbose
  deriving Eq

data Mirror = DownloadFpo | DlFpo | Mirror String
  deriving Eq

-- FIXME: True for koji make into type
data RepoSource = RepoSource Bool Mirror
  deriving Eq

data Release = EPEL10Dot Natural | EPEL Natural | EPELNext Natural
             | Centos Natural | Fedora Natural
             | ELN | Rawhide | System
  deriving (Eq, Ord)

-- FIXME determine via EPEL or Centos
elnVersion :: Natural
elnVersion = 11

-- FIXME error if short/release-like
-- | Read a Release name
eitherRelease :: String -> Either String Release
eitherRelease rel =
  case lower rel of
    "rawhide" -> Right Rawhide
    -- FIXME add proper parsing:
    "epel8-next" -> Right $ EPELNext 8
    "epel9-next" -> Right $ EPELNext 9
    ('e':'p':'e':'l':'1':'0':'.':n@(_:_)) | all isDigit n -> let br = EPEL10Dot (read n)
                                                 in Right br
    ('e':'p':'e':'l':n@(_:_)) | all isDigit n -> let br = EPEL (read n)
                                                 in Right br
    ('c':n@(_:_)) ->
      let ver =
            case unsnoc n of
              Just (v,'s') -> v
              _ -> n
      in if null ver
         then Left rel
         else
           if all isDigit ver
           then Right (Centos $ read ver)
           else Left rel
    "eln" -> Right ELN
    ('f':n@(_:_)) | all isDigit n -> let r = read n in Right (Fedora r)
    ns@(_:_) | all isDigit ns ->
               let r = read ns in
                 Right $
                 case compare r elnVersion of
                   LT -> Centos r
                   EQ -> ELN
                   GT -> Fedora r
    _ -> Left rel

instance Show Release where
  show Rawhide = "rawhide"
  show (Fedora n) = "f" ++ show n
  show (EPEL10Dot n) = "epel10." ++ show n
  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
  show (EPELNext n) = "epel" ++ show n ++ "-next"
  show ELN = "eln"
  show (Centos n) = 'c' : show n ++ "s"
  show System = "system"
