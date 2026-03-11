{-# LANGUAGE CPP #-}

-- | A library for pkgtreediff for comparing trees of rpm packages
module Distribution.RPM.PackageTreeDiff
  (RPMPkgDiff(..),
   Ignore(..),
   diffPkgs,
   -- * from rpm-nvr
   NVRA(..),
   readNVRA
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.RPM.VerRel
import Data.RPM.NVRA
import Data.RPM.VerCmp (rpmVerCompare)

-- | Ignore describes how comparison is done
data Ignore = IgnoreNone    -- ^ do not ignore version or release
            | IgnoreRelease -- ^ ignore differences in release
            | IgnoreVersion -- ^ ignore differences in version
  deriving Eq

-- | RPMPkgDiff type encodes how a particular rpm package differs between trees
data RPMPkgDiff = PkgUpdate NVRA NVRA
                | PkgDowngrade NVRA NVRA
                | PkgAdd NVRA
                | PkgDel NVRA
                | PkgArch NVRA NVRA
  deriving Eq

-- | Compare two lists of packages NVRAs
diffPkgs :: Ignore -> [NVRA] -> [NVRA] -> [RPMPkgDiff]
diffPkgs _ [] [] = []
diffPkgs ignore (p:ps) [] = PkgDel p : diffPkgs ignore ps []
diffPkgs ignore [] (p:ps) = PkgAdd p : diffPkgs ignore [] ps
diffPkgs ignore (p1:ps1) (p2:ps2) =
  case compare (rpmName p1) (rpmName p2) of
    LT -> PkgDel p1 : diffPkgs ignore ps1 (p2:ps2)
    EQ -> case diffPkg of
            Just diff -> (diff :)
            Nothing -> id
          $ diffPkgs ignore ps1 ps2
    GT -> PkgAdd p2 : diffPkgs ignore (p1:ps1) ps2
  where
    diffPkg :: Maybe RPMPkgDiff
    diffPkg =
      if rpmArch p1 == rpmArch p2
      then case cmpVR ignore (rpmVerRel p1) (rpmVerRel p2) of
             LT -> Just $ PkgUpdate p1 p2
             EQ -> Nothing
             GT -> Just $ PkgDowngrade p1 p2
      else Just $ PkgArch p1 p2

-- cmpVR True ignore release
cmpVR :: Ignore -> VerRel -> VerRel -> Ordering
cmpVR IgnoreNone vr vr' = compare vr vr'
cmpVR IgnoreRelease (VerRel v _) (VerRel v' _) = rpmVerCompare v v'
cmpVR IgnoreVersion _ _ = EQ
