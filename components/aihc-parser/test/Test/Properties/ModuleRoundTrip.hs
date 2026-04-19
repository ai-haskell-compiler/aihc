{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Parens (addModuleParens)
import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Module ()
import Test.Properties.ExprHelpers (normalizeDecl)
import Test.QuickCheck

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
      (errs, reparsed) = parseModule moduleConfig source
      reparsedSource = renderStrict (layoutPretty defaultLayoutOptions (pretty reparsed))
   in counterexample ("Original source:\n" <> T.unpack source) $
        counterexample ("Reparsed source:\n" <> T.unpack reparsedSource) $
          case errs of
            [] ->
              let expected = normalizeModule (addModuleParens modu)
                  actual = normalizeModule reparsed
               in counterexample ("Original AST:\n" <> show (shorthand expected) <> "\nActual AST:\n" <> show (shorthand actual)) (expected == actual)
            _ ->
              counterexample (formatParseErrors "<quickcheck>" (Just source) errs) False

moduleConfig :: ParserConfig
moduleConfig =
  defaultConfig
    { parserExtensions = effectiveExtensions GHC2024Edition [EnableExtension BlockArguments, EnableExtension Arrows, EnableExtension UnboxedTuples, EnableExtension UnboxedSums, EnableExtension TemplateHaskell, EnableExtension UnicodeSyntax, EnableExtension QuasiQuotes, EnableExtension PatternSynonyms, EnableExtension MagicHash, EnableExtension OverloadedLabels, EnableExtension MultiWayIf, EnableExtension RecursiveDo, EnableExtension CApiFFI, EnableExtension ImplicitParams, EnableExtension TypeAbstractions, EnableExtension RequiredTypeArguments]
    }

-- Module normalization
normalizeModule :: Module -> Module
normalizeModule modu =
  Module
    { moduleAnns = [],
      moduleHead = fmap normalizeModuleHead (moduleHead modu),
      moduleLanguagePragmas = [],
      moduleImports = map normalizeImportDecl (moduleImports modu),
      moduleDecls = map normalizeDecl (moduleDecls modu)
    }

normalizeModuleHead :: ModuleHead -> ModuleHead
normalizeModuleHead head' =
  ModuleHead
    { moduleHeadAnns = [],
      moduleHeadName = moduleHeadName head',
      moduleHeadWarningText = fmap normalizeWarningText (moduleHeadWarningText head'),
      moduleHeadExports = fmap (map normalizeExportSpec) (moduleHeadExports head')
    }

normalizeExportSpec :: ExportSpec -> ExportSpec
normalizeExportSpec spec =
  case spec of
    ExportAnn _ sub -> normalizeExportSpec sub
    ExportModule mWarning modName -> ExportModule (normalizeWarningText <$> mWarning) modName
    ExportVar mWarning namespace name -> ExportVar (normalizeWarningText <$> mWarning) namespace name
    ExportAbs mWarning namespace name -> ExportAbs (normalizeWarningText <$> mWarning) namespace name
    ExportAll mWarning namespace name -> ExportAll (normalizeWarningText <$> mWarning) namespace name
    ExportWith mWarning namespace name members -> ExportWith (normalizeWarningText <$> mWarning) namespace name (map normalizeExportMember members)
    ExportWithAll mWarning namespace name wildcardIndex members -> ExportWithAll (normalizeWarningText <$> mWarning) namespace name wildcardIndex (map normalizeExportMember members)

normalizeExportMember :: IEBundledMember -> IEBundledMember
normalizeExportMember (IEBundledMember namespace name) = IEBundledMember namespace name

normalizeWarningText :: WarningText -> WarningText
normalizeWarningText warningText =
  case warningText of
    DeprText msg -> DeprText msg
    WarnText msg -> WarnText msg
    WarningTextAnn _ sub -> normalizeWarningText sub

normalizeImportDecl :: ImportDecl -> ImportDecl
normalizeImportDecl decl =
  ImportDecl
    { importDeclAnns = [],
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
    { importSpecAnns = [],
      importSpecHiding = importSpecHiding spec,
      importSpecItems = map normalizeImportItem (importSpecItems spec)
    }

normalizeImportItem :: ImportItem -> ImportItem
normalizeImportItem item =
  case item of
    ImportAnn _ sub -> normalizeImportItem sub
    ImportItemVar namespace name -> ImportItemVar namespace name
    ImportItemAbs namespace name -> ImportItemAbs namespace name
    ImportItemAll namespace name -> ImportItemAll namespace name
    ImportItemWith namespace name members -> ImportItemWith namespace name (map normalizeExportMember members)
    ImportItemAllWith namespace name wildcardIndex members -> ImportItemAllWith namespace name wildcardIndex (map normalizeExportMember members)
