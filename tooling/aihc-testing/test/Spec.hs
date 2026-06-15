{-# LANGUAGE OverloadedStrings #-}

import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Annotation,
    Decl (..),
    Expr (..),
    Module (..),
    ModuleHead (..),
    Name (..),
    NameType (..),
    Rhs (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    stripAnnotations,
  )
import Aihc.Testing.AnnotatedModule (renderAnnotatedModule)
import Aihc.Testing.AnnotatedModule qualified as AnnotatedModule
import Control.Exception (SomeException, evaluate, try)
import Data.Text qualified as Text
import Prettyprinter (Doc, hardline, pretty)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

newtype TestAnnotation = TestAnnotation String
  deriving (Eq, Show)

main :: IO ()
main =
  defaultMain $
    testGroup
      "aihc-testing"
      [ testCase "reconstructs spans for generated modules" testGeneratedModuleSpans,
        testCase "renders a single annotation" testSingleAnnotation,
        testCase "stacks overlapping annotations deterministically" testOverlappingAnnotations,
        testCase "packs non-overlapping annotations on one line" testPackedAnnotations,
        testCase "throws on pretty/reparse parse failure" testParseFailure,
        testCase "throws on stripped AST mismatch" testShapeMismatch,
        testCase "throws on multiline annotation labels" testMultilineLabel,
        testCase "throws on renderable annotation without span" testMissingSpan,
        testCase "renders source text without pretty-printing" testSourceTextRendering,
        testCase "throws on source/module count mismatch" testSourceModuleCountMismatch,
        testProperty "accepts repository QuickCheck options" True
      ]

testGeneratedModuleSpans :: IO ()
testGeneratedModuleSpans = do
  let modu = annotateFirstDecl "generated" (stripAnnotations (parseModuleOrFail "module Main where\nx = 1"))
  assertEqual
    "rendered annotation"
    "module Main where\nx = 1\n\9492\9472 generated"
    (renderAnnotatedModule testConfig testAnnotationDoc modu)

testSingleAnnotation :: IO ()
testSingleAnnotation = do
  let modu = annotateFirstDecl "decl" (parseModuleOrFail "x = 1")
  assertEqual
    "rendered annotation"
    "x = 1\n\9492\9472 decl"
    (renderAnnotatedModule testConfig testAnnotationDoc modu)

testOverlappingAnnotations :: IO ()
testOverlappingAnnotations = do
  let modu = annotateFirstDecl "outer" (annotateFirstDecl "inner" (parseModuleOrFail "x = 1"))
  assertEqual
    "stacked annotations"
    "x = 1\n\9492\9472 inner\n\9492\9472 outer"
    (renderAnnotatedModule testConfig testAnnotationDoc modu)

testPackedAnnotations :: IO ()
testPackedAnnotations = do
  let modu = annotateRhsName "rhs" (annotateFirstDecl "lhs" (stripAnnotations (parseModuleOrFail "longname = other")))
  assertEqual
    "packed annotations"
    "longname = other\n\9492\9472 lhs     \9492\9472 rhs"
    (renderAnnotatedModule testConfig testAnnotationDoc modu)

testParseFailure :: IO ()
testParseFailure = do
  result <- throws (renderAnnotatedModule testConfig testAnnotationDoc invalidModule)
  assertBool "expected parse failure exception" result

testShapeMismatch :: IO ()
testShapeMismatch = do
  result <- throws (renderAnnotatedModule testConfig testAnnotationDoc emptyFunctionModule)
  assertBool "expected shape mismatch exception" result

testMultilineLabel :: IO ()
testMultilineLabel = do
  let modu = annotateFirstDecl "unused" (parseModuleOrFail "x = 1")
  result <- throws (renderAnnotatedModule testConfig multilineAnnotationDoc modu)
  assertBool "expected multiline label exception" result

testMissingSpan :: IO ()
testMissingSpan = do
  let modu = annotateFirstDecl "missing-span" (stripAnnotations (parseModuleOrFail "x = 1"))
  result <- throws (AnnotatedModule.renderAnnotatedModuleSource testAnnotationDoc "x = 1" modu)
  assertBool "expected missing span exception" result

testSourceTextRendering :: IO ()
testSourceTextRendering = do
  let source = "x = f y z"
      modu = annotateRhsName "arg" (parseModuleOrFail source)
  assertEqual
    "source text is not reflowed"
    "x = f y z\n        \9492\9472 arg"
    (AnnotatedModule.renderAnnotatedModuleSource testAnnotationDoc source modu)

testSourceModuleCountMismatch :: IO ()
testSourceModuleCountMismatch = do
  let modu = parseModuleOrFail "x = 1"
  result <- throws (show (AnnotatedModule.renderAnnotatedModuleSources testAnnotationDoc [] [modu]))
  assertBool "expected count mismatch exception" result

testConfig :: ParserConfig
testConfig = defaultConfig {parserSourceName = "<test>"}

parseModuleOrFail :: Text.Text -> Module
parseModuleOrFail input =
  case parseModule testConfig input of
    ([], modu) -> modu
    (errs, _) -> error ("parseModuleOrFail: " <> show errs)

annotateFirstDecl :: String -> Module -> Module
annotateFirstDecl label modu =
  case moduleDecls modu of
    decl : rest -> modu {moduleDecls = DeclAnn (mkAnnotation (TestAnnotation label)) decl : rest}
    [] -> error "annotateFirstDecl: module has no declarations"

annotateRhsName :: String -> Module -> Module
annotateRhsName label modu =
  case moduleDecls modu of
    [DeclAnn ann (DeclValue valueDecl)] ->
      modu {moduleDecls = [DeclAnn ann (DeclValue (annotateValueDecl valueDecl))]}
    [DeclValue valueDecl] ->
      modu {moduleDecls = [DeclValue (annotateValueDecl valueDecl)]}
    _ -> error "annotateRhsName: unexpected module shape"
  where
    annotateValueDecl valueDecl =
      case valueDecl of
        PatternBind multiplicity pat (UnguardedRhs anns expr whereDecls) ->
          PatternBind multiplicity pat (UnguardedRhs anns (annotateRightmostName label expr) whereDecls)
        _ -> error "annotateRhsName: unexpected value declaration"

annotateRightmostName :: String -> Expr -> Expr
annotateRightmostName label expr =
  case expr of
    EAnn ann inner -> EAnn ann (annotateRightmostName label inner)
    EVar name -> EVar (annotateName label name)
    EApp fun arg -> EApp fun (annotateRightmostName label arg)
    _ -> error "annotateRightmostName: unexpected expression"

annotateName :: String -> Name -> Name
annotateName label name =
  name {nameAnns = nameAnns name <> [mkAnnotation (TestAnnotation label)]}

testAnnotationDoc :: Annotation -> Maybe (Doc ann)
testAnnotationDoc annotation = do
  TestAnnotation label <- fromAnnotation annotation
  pure (pretty label)

multilineAnnotationDoc :: Annotation -> Maybe (Doc ann)
multilineAnnotationDoc annotation = do
  TestAnnotation _ <- fromAnnotation annotation
  pure ("first" <> hardline <> "second")

throws :: String -> IO Bool
throws value = do
  result <- try (evaluate (length value)) :: IO (Either SomeException Int)
  pure $
    case result of
      Left _ -> True
      Right _ -> False

invalidModule :: Module
invalidModule =
  Module
    { moduleAnns = [],
      moduleHead = Just (ModuleHead [] "Bad Name" Nothing Nothing),
      moduleLanguagePragmas = [],
      moduleImports = [],
      moduleDecls = []
    }

emptyFunctionModule :: Module
emptyFunctionModule =
  Module
    { moduleAnns = [],
      moduleHead = Nothing,
      moduleLanguagePragmas = [],
      moduleImports = [],
      moduleDecls = [DeclValue (FunctionBind (UnqualifiedName NameVarId "f" []) [])]
    }
