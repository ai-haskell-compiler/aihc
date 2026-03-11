{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wall #-}
-- | Discover the GHC version via the package database. Requirements:
--
--     * the package database must be compatible, which is usually not the case
--       across major ghc versions.
--
--     * the 'ghc' package is registered, which is not always the case.
module GHC.Check.PackageDb
  ( PackageVersion(abi), version,
    getPackageVersion,
    fromVersionString
   )
where

import Control.Monad.Trans.Class as Monad (MonadTrans (lift))
import Data.String (IsString (fromString))
import Data.Version (Version)
import Language.Haskell.TH.Syntax (Lift)
import Data.Foldable (find)
import Control.Applicative (Alternative((<|>)))
#if MIN_VERSION_ghc(9,2,0)
import GHC
  (Ghc,
    getSession,
  )
import GHC.Data.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified GHC.Data.ShortText as ShortText
import GHC.Driver.Env (hsc_unit_env, )
import GHC.Unit.Info (PackageName (PackageName))
import GHC.Unit.Env (ue_units)
import GHC.Unit.State
  (lookupUnit, explicitUnits,  lookupUnitId,
    lookupPackageName, GenericUnitInfo (..),
    UnitInfo, unitPackageNameString)
#if !MIN_VERSION_ghc(9,4,0)
import GHC.Unit.Types (indefUnit)
#endif
#elif MIN_VERSION_ghc(9,0,1)
import GHC
  (unitState,  Ghc,
    getSessionDynFlags,
  )
import GHC.Data.Maybe (MaybeT (MaybeT), runMaybeT)
import GHC.Unit.Info (PackageName (PackageName))
import GHC.Unit.State
  (lookupUnit, explicitUnits,  lookupUnitId,
    lookupPackageName, GenericUnitInfo (..),
    UnitInfo, unitPackageNameString)
import GHC.Unit.Types (indefUnit)
#else
import GHC
  (pkgState,  Ghc,
    getSessionDynFlags,
  )
import Maybes (MaybeT (MaybeT), runMaybeT)
import Module (componentIdToInstalledUnitId)
import PackageConfig (PackageName (PackageName))
import Packages
  (lookupPackage, explicitPackages,  lookupInstalledPackage,
    lookupPackageName
  )
import Packages (InstalledPackageInfo (packageVersion, abiHash))
import Packages (PackageConfig)
import Packages (packageNameString)
#endif
import GHC.Stack (HasCallStack)

import GHC.Check.Util

data PackageVersion
  = PackageVersion
      { myVersion :: !MyVersion,
        abi :: Maybe String
      }
  deriving (Eq, Lift, Show)

version :: PackageVersion -> Version
version PackageVersion{ myVersion = MyVersion v} = v

#if MIN_VERSION_ghc(9,4,0)
indefUnit :: a -> a
indefUnit = id
#endif

#if MIN_VERSION_ghc(9,2,0)
-- | @getPackageVersion p@ returns the version of package @p@ that will be used in the Ghc session.
getPackageVersion :: String -> Ghc (Maybe PackageVersion)
getPackageVersion pName = runMaybeT $ do
  hsc_env <- Monad.lift getSession
  let pkgst   = ue_units $ hsc_unit_env hsc_env
      depends =
#if MIN_VERSION_ghc(9,4,0)
        map fst $
#endif
        explicitUnits pkgst

  let explicit = do
        pkgs <- traverse (MaybeT . return . lookupUnit pkgst) depends
        MaybeT $ return $ find (\p -> unitPackageNameString p == pName ) pkgs

      notExplicit = do
        component <- MaybeT $ return $ lookupPackageName pkgst $ PackageName $ fromString pName
        MaybeT $ return $ lookupUnitId pkgst (indefUnit component)

  p <- explicit <|> notExplicit

  return $ fromPackageConfig p

fromPackageConfig :: UnitInfo -> PackageVersion
fromPackageConfig p = PackageVersion (MyVersion $ unitPackageVersion p) (Just $ ShortText.unpack $ unitAbiHash p)

#elif MIN_VERSION_ghc(9,0,1)
-- | @getPackageVersion p@ returns the version of package @p@ that will be used in the Ghc session.
getPackageVersion :: String -> Ghc (Maybe PackageVersion)
getPackageVersion pName = runMaybeT $ do
  dflags <- Monad.lift getSessionDynFlags
  let pkgst   = unitState dflags
      depends = explicitUnits pkgst

  let explicit = do
        pkgs <- traverse (MaybeT . return . lookupUnit pkgst) depends
        MaybeT $ return $ find (\p -> unitPackageNameString p == pName ) pkgs

      notExplicit = do
        component <- MaybeT $ return $ lookupPackageName pkgst $ PackageName $ fromString pName
        MaybeT $ return $ lookupUnitId pkgst (indefUnit component)

  p <- explicit <|> notExplicit

  return $ fromPackageConfig p

fromPackageConfig :: UnitInfo -> PackageVersion
fromPackageConfig p = PackageVersion (MyVersion $ unitPackageVersion p) (Just $ unitAbiHash p)

#else

-- | @getPackageVersion p@ returns the version of package @p@ that will be used in the Ghc session.
getPackageVersion :: String -> Ghc (Maybe PackageVersion)
getPackageVersion pName = runMaybeT $ do
  dflags <- Monad.lift getSessionDynFlags
  let pkgst   = pkgState dflags
      depends = explicitPackages pkgst

  let explicit = do
        pkgs <- traverse (MaybeT . return . lookupPackage dflags) depends
        MaybeT $ return $ find (\p -> packageNameString p == pName ) pkgs

      notExplicit = do
        component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString pName
        MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)

  p <- explicit <|> notExplicit

  return $ fromPackageConfig p

fromPackageConfig :: PackageConfig -> PackageVersion
fromPackageConfig p = PackageVersion (MyVersion $ packageVersion p) (Just $ abiHash p)
#endif

fromVersionString :: HasCallStack => String -> PackageVersion
fromVersionString v = PackageVersion (read v) Nothing
