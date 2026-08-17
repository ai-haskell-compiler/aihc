{-# LANGUAGE OverloadedStrings #-}

-- | Wired GHC.Types names for System FC 2.
module Aihc.Fc2.Wired
  ( wiredGhcTypes,
    typeSynonym,
    typeConstructor,
    runtimeRepConstructor,
    levityConstructor,
    constraintName,
    liftedRepName,
    unliftedRepName,
    boxedRepName,
    liftedName,
    unliftedName,
    ghcTypesModule,
    isGhcTypesOrigin,
    primPackageFromScopes,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Resolve (PackageId (..))
import Data.Maybe (listToMaybe)
import Data.Text (Text)

ghcTypesModule :: Text
ghcTypesModule = "GHC.Types"

wiredGhcTypes :: PackageId -> Text -> Sort -> Name
wiredGhcTypes package name sort =
  Name name sort (OriginTop package ghcTypesModule)

typeSynonym :: PackageId -> Type
typeSynonym package =
  TyCon (wiredGhcTypes package "Type" SortSynonym)

typeConstructor :: PackageId -> Name
typeConstructor package =
  wiredGhcTypes package "TYPE" SortTypeConstructor

runtimeRepConstructor :: PackageId -> Name
runtimeRepConstructor package =
  wiredGhcTypes package "RuntimeRep" SortTypeConstructor

levityConstructor :: PackageId -> Name
levityConstructor package =
  wiredGhcTypes package "Levity" SortTypeConstructor

constraintName :: PackageId -> Name
constraintName package =
  wiredGhcTypes package "Constraint" SortTypeConstructor

liftedRepName :: PackageId -> Name
liftedRepName package =
  wiredGhcTypes package "LiftedRep" SortSynonym

unliftedRepName :: PackageId -> Name
unliftedRepName package =
  wiredGhcTypes package "UnliftedRep" SortSynonym

boxedRepName :: PackageId -> Name
boxedRepName package =
  wiredGhcTypes package "BoxedRep" SortDataConstructor

liftedName :: PackageId -> Name
liftedName package =
  wiredGhcTypes package "Lifted" SortDataConstructor

unliftedName :: PackageId -> Name
unliftedName package =
  wiredGhcTypes package "Unlifted" SortDataConstructor

isGhcTypesOrigin :: PackageId -> Name -> Bool
isGhcTypesOrigin package name =
  case nameOrigin name of
    OriginTop originPackage moduleName ->
      originPackage == package && moduleName == ghcTypesModule
    OriginLocal {} -> False

-- | The package identity of the GHC.Types scope, if the table has one.
primPackageFromScopes :: ScopeTable -> Maybe PackageId
primPackageFromScopes table =
  listToMaybe
    [ package
    | (_, package, moduleName) <- scopeEntries table,
      moduleName == ghcTypesModule
    ]
