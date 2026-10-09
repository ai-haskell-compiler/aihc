{-# LANGUAGE OverloadedStrings #-}

-- | Join documentation to export identities from aihc-resolve.
module Aihc.Haddock.Resolve (resolveDocumentation) where

import Aihc.Haddock.Interface
import Aihc.Haddock.Model
import Aihc.Resolve qualified as R
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set

resolveDocumentation :: R.Package -> [PackageDoc] -> [(R.ModuleUnit, ModuleDoc)] -> [ModuleDoc]
resolveDocumentation package dependencies inputs = map complete local
  where
    publicModules =
      Map.fromList
        ( [(R.ModuleKey package (moduleDocName modu), modu) | (_, modu) <- inputs, moduleDocExposed modu]
            <> [(R.ModuleKey (dependencyPackage dep) (moduleDocName modu), modu) | dep <- dependencies, modu <- packageDocModules dep, moduleDocExposed modu]
        )
    dependencyExports =
      R.moduleExportsFromList
        [ (R.ModuleKey (dependencyPackage dep) (moduleDocName modu), unDocInterface (moduleDocInterface modu))
        | dep <- dependencies,
          modu <- packageDocModules dep,
          moduleDocExposed modu
        ]
    dependencyPackage dep = R.Package (packageDocName dep) (R.PackageId (packageDocName dep <> "-" <> packageDocVersion dep))
    ownExports = R.collectModuleExportsWithDeps dependencyExports (map fst inputs)
    visibleExports = ownExports <> dependencyExports
    local = [(unit, identify unit modu) | (unit, modu) <- inputs]
    identify unit modu = modu {moduleDocDecls = map attach (moduleDocDecls modu)}
      where
        declarations = R.moduleDeclarationExports visibleExports unit
        attach decl =
          decl
            { declIdentity = DocName <$> lookupIdentity declarations decl,
              declSubordinates = map attach (declSubordinates decl)
            }
    lookupIdentity declarations decl =
      case Map.lookup (declName decl) (case declNamespace decl of NamespaceType -> R.exportedTypes declarations; NamespaceValue -> R.exportedTerms declarations) of
        Just (R.EntityGlobal name) -> Just name
        _ -> Nothing
    exportDocs (ExportResolvedItem decl) = [decl]
    exportDocs (ExportResolvedModuleItem _ _ decls) = decls
    exportDocs _ = []
    allDocs =
      concatMap (moduleDocDecls . snd) local
        <> concatMap (concatMap moduleDocDecls . packageDocModules) dependencies
        <> [decl | dep <- dependencies, modu <- packageDocModules dep, item <- moduleDocResolvedExports modu, decl <- exportDocs item]
    flatten decl = decl : concatMap flatten (declSubordinates decl)
    docs = Map.fromListWith mergeDeclaration [(identity, decl) | decl <- concatMap flatten allDocs, Just identity <- [declIdentity decl]]
    -- A dependency can export only part of a declaration from another package.
    -- Combine these parts before selection, with the original source order.
    mergeDeclaration new old = old {declSubordinates = mergeChildren (declSubordinates old <> declSubordinates new)}
    mergeChildren children =
      sortOn
        declLocation
        ( Map.elems (Map.fromListWith mergeDeclaration [(identity, child) | child <- children, Just identity <- [declIdentity child]])
            <> filter (isNothing . declIdentity) children
        )
    parents = Map.fromListWith (<>) (concatMap (ancestors []) allDocs)
    ancestors above decl =
      [(identity, above) | Just identity <- [declIdentity decl]]
        <> concatMap (ancestors (maybe above (: above) (declIdentity decl))) (declSubordinates decl)
    complete (unit, modu) =
      modu
        { moduleDocInterface = DocInterface interface,
          moduleDocResolvedExports = items,
          moduleDocDiagnostics = moduleDocDiagnostics modu <> missing
        }
      where
        interface = fromMaybe (R.exportsFromEntries []) (R.lookupModuleExport (R.ModuleKey package (moduleDocName modu)) ownExports)
        (items, missing) = case R.resolveExportList visibleExports unit of
          Nothing -> (map ExportResolvedItem (moduleDocDecls modu), [])
          Just exports -> merge (R.ModuleKey package (moduleDocName modu)) (fromMaybe [] (moduleDocExports modu)) exports
    merge _ [] _ = ([], [])
    merge current (item : rest) scopes = case item of
      ExportDeclItem {} -> next
      ExportModuleItem {} -> next
      _ -> let (items, diagnostics) = merge current rest scopes in (item : items, diagnostics)
      where
        next = case scopes of
          scope : remaining ->
            let (docs', diagnostics) = materializeExport current scope
                (items, more) = merge current rest remaining
             in (docs' <> items, diagnostics <> more)
          [] -> ([], ["No resolver result for an export item"])
    materializeExport current resolved =
      let interface = R.resolvedExportScope resolved
          (_, diagnostics) = materialize interface
          references = [key | key <- R.resolvedExportModules resolved, key /= current, Map.member key publicModules]
          referenceInterface key = fromMaybe (R.exportsFromEntries []) (R.lookupModuleExport key visibleExports)
          linked = Set.fromList [DocName name | key <- references, name <- interfaceNames (referenceInterface key)]
          links =
            [ ExportResolvedModuleItem
                (R.packageIdText (R.packageId (R.moduleKeyPackage key)))
                (R.moduleKeyName key)
                [decl | ExportResolvedItem decl <- fst (materialize (referenceInterface key))]
            | key <- references
            ]
          (remaining, _) = materializeNames [name | global <- interfaceNames interface, let name = DocName global, Set.notMember name linked]
       in (links <> remaining, diagnostics)
    materialize = materializeNames . map DocName . interfaceNames
    materializeNames names =
      ( [ExportResolvedItem (prune decl) | name <- names, all (`Set.notMember` selected) (Map.findWithDefault [] name parents), Just decl <- [Map.lookup name docs]],
        ["No documentation declaration for " <> R.globalNameModule global <> "." <> R.globalNameText global | name@(DocName global) <- names, Map.notMember name docs]
      )
      where
        selected = Set.fromList names
        prune decl = decl {declSubordinates = concatMap keep (declSubordinates decl)}
        keep decl
          | maybe False (`Set.member` selected) (declIdentity decl) = [prune decl]
          | otherwise = concatMap keep (declSubordinates decl)
