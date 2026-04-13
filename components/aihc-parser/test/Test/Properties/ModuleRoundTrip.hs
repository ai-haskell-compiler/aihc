{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Module ()
import Test.Properties.ExprHelpers (normalizeDecl, span0)
import Test.QuickCheck

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
      (errs, reparsed) = parseModule moduleConfig source
   in counterexample (T.unpack source) $
        case errs of
          [] ->
            let expected = normalizeModule modu
                actual = normalizeModule reparsed
             in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
          _ ->
            counterexample (formatParseErrors "<quickcheck>" (Just source) errs) False

moduleConfig :: ParserConfig
moduleConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, Arrows, UnboxedTuples, UnboxedSums, TemplateHaskell, UnicodeSyntax, LambdaCase, QuasiQuotes, ExplicitNamespaces, PatternSynonyms]
    }

-- Module normalization
normalizeModule :: Module -> Module
normalizeModule modu =
  Module
    { moduleSpan = span0,
      moduleHead = fmap normalizeModuleHead (moduleHead modu),
      moduleLanguagePragmas = moduleLanguagePragmas modu,
      moduleImports = map normalizeImportDecl (moduleImports modu),
      moduleDecls = map normalizeDecl (moduleDecls modu)
    }

normalizeModuleHead :: ModuleHead -> ModuleHead
normalizeModuleHead head' =
  ModuleHead
    { moduleHeadSpan = span0,
      moduleHeadName = moduleHeadName head',
      moduleHeadWarningText = fmap normalizeWarningText (moduleHeadWarningText head'),
      moduleHeadExports = fmap (map normalizeExportSpec) (moduleHeadExports head')
    }

normalizeExportSpec :: ExportSpec -> ExportSpec
normalizeExportSpec spec =
  case spec of
    ExportModule _ mWarning modName -> ExportModule span0 (normalizeWarningText <$> mWarning) modName
    ExportVar _ mWarning namespace name -> ExportVar span0 (normalizeWarningText <$> mWarning) namespace name
    ExportAbs _ mWarning namespace name -> ExportAbs span0 (normalizeWarningText <$> mWarning) namespace name
    ExportAll _ mWarning namespace name -> ExportAll span0 (normalizeWarningText <$> mWarning) namespace name
    ExportWith _ mWarning namespace name members -> ExportWith span0 (normalizeWarningText <$> mWarning) namespace name (map normalizeExportMember members)

normalizeExportMember :: IEBundledMember -> IEBundledMember
normalizeExportMember (IEBundledMember namespace name) = IEBundledMember namespace name

normalizeWarningText :: WarningText -> WarningText
normalizeWarningText warningText =
  case warningText of
    DeprText _ msg -> DeprText span0 msg
    WarnText _ msg -> WarnText span0 msg

normalizeImportDecl :: ImportDecl -> ImportDecl
normalizeImportDecl decl =
  ImportDecl
    { importDeclSpan = span0,
      importDeclLevel = importDeclLevel decl,
      importDeclPackage = importDeclPackage decl,
      importDeclSource = importDeclSource decl,
      importDeclSafe = importDeclSafe decl,
      importDeclQualified = importDeclQualified decl,
      importDeclQualifiedPost = importDeclQualifiedPost decl,
      importDeclModule = importDeclModule decl,
      importDeclAs = importDeclAs decl,
      importDeclSpec = fmap normalizeImportSpec (importDeclSpec decl)
    }

normalizeImportSpec :: ImportSpec -> ImportSpec
normalizeImportSpec spec =
  ImportSpec
    { importSpecSpan = span0,
      importSpecHiding = importSpecHiding spec,
      importSpecItems = map normalizeImportItem (importSpecItems spec)
    }

normalizeImportItem :: ImportItem -> ImportItem
normalizeImportItem item =
  case item of
    ImportItemVar _ namespace name -> ImportItemVar span0 namespace name
    ImportItemAbs _ namespace name -> ImportItemAbs span0 namespace name
    ImportItemAll _ namespace name -> ImportItemAll span0 namespace name
    ImportItemWith _ namespace name members -> ImportItemWith span0 namespace name (map normalizeExportMember members)
