{-# LANGUAGE OverloadedStrings #-}

module Test.Compat.Decl
  ( declCompatTests,
  )
where

import Aihc.Parser qualified as Aihc
import Aihc.Parser.Compat (toGhcHsDecl)
import Aihc.Parser.Compat.Internal.Ghc
  ( compatGhcExtensions,
    normalizeGhcAst,
    parseGhcLocatedDecl,
  )
import Aihc.Parser.Parens (addDeclParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
  ( Decl,
    Extension (..),
  )
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (GhcPs)
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Decls (HsDecl)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Compat.Expr (comparisonDump, renderGhc, sameStructural)
import Test.Properties.Arb.Decl ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

declCompatTests :: TestTree
declCompatTests =
  testGroup
    "decl"
    [ testGroup
        "examples"
        [ example "value binding" "x = 1",
          example "function binding" "f x = x",
          example "type signature" "f :: a -> a",
          example "fixity" "infixr 5 <++>",
          example "data" "data T a = MkT a",
          example "newtype" "newtype N a = MkN a",
          example "gadt" "data G a where MkG :: a -> G a",
          example "record constructor" "data R = R { field :: Int }",
          example "deriving" "data D = D deriving stock Eq",
          example "type synonym" "type Pair a = (a, a)",
          example "standalone kind signature" "type K :: Type",
          example "standalone kind signature with forall body kind signature" "type (:+) :: forall (a :: _). (_ :: _)",
          example "type family" "type family F a",
          example "data family" "data family DF a",
          example "class" "class C a where method :: a -> a",
          example "class associated families" "class CF a where type AF a; data ADF a",
          example "instance" "instance C Int where method x = x",
          example "instance associated data" "instance CF Int where data ADF Int = ADFInt",
          example "standalone deriving" "deriving stock instance Eq D",
          example "default" "default (Int)",
          example "foreign import" "foreign import ccall \"math.h sin\" c_sin :: Double -> Double",
          example "foreign export" "foreign export ccall f :: Int -> Int",
          example "pattern synonym" "pattern P x = Just x",
          example "th splice" "$(deriveSomething ''T)",
          example "inline pragma" "{-# INLINE f #-}"
        ],
      QC.testProperty "generated decl converts to normalized GHC parsed AST" prop_declCompat
    ]

prop_declCompat :: Decl -> QC.Property
prop_declCompat decl0 =
  let decl = addDeclParens decl0
      source = renderDecl decl
   in QC.counterexample (T.unpack source) $
        case (Aihc.parseDecl compatParserConfig source, parseGhcLocatedDecl compatGhcExtensions source) of
          (Aihc.ParseOk aihcDecl, Right parsed) ->
            let expected = normalizeGhcAst (unLoc parsed)
                actual = toGhcHsDecl aihcDecl
             in QC.counterexample (declMismatch expected actual) $
                  sameStructural expected actual
          _ -> QC.discard

example :: TestName -> Text -> TestTree
example label source =
  testCase label $ do
    aihcDecl <-
      case Aihc.parseDecl compatParserConfig source of
        Aihc.ParseErr err -> assertFailure (Aihc.formatParseErrors "<compat-test>" (Just source) err)
        Aihc.ParseOk decl -> pure decl
    ghcDecl <-
      case parseGhcLocatedDecl compatGhcExtensions source of
        Left err -> assertFailure (T.unpack err <> "\nsource:\n" <> T.unpack source)
        Right parsed -> pure (normalizeGhcAst (unLoc parsed))
    let convertedDecl = toGhcHsDecl aihcDecl
    assertBool (T.unpack source <> "\n" <> declMismatch ghcDecl convertedDecl) $
      sameStructural ghcDecl convertedDecl

declMismatch :: HsDecl GhcPs -> HsDecl GhcPs -> String
declMismatch expected actual =
  unlines
    [ "expected pretty: " <> renderGhc expected,
      "actual pretty: " <> renderGhc actual,
      "expected comparison tree:",
      comparisonDump expected,
      "actual comparison tree:",
      comparisonDump actual
    ]

renderDecl :: Decl -> Text
renderDecl = renderStrict . layoutPretty defaultLayoutOptions . pretty

compatParserConfig :: Aihc.ParserConfig
compatParserConfig =
  Aihc.defaultConfig
    { Aihc.parserExtensions =
        [ Arrows,
          BlockArguments,
          CApiFFI,
          DataKinds,
          DefaultSignatures,
          DeriveAnyClass,
          DerivingStrategies,
          DerivingViaExtension,
          EmptyDataDecls,
          ExplicitForAll,
          ExplicitNamespaces,
          ForeignFunctionInterface,
          FunctionalDependencies,
          GADTs,
          ImplicitParams,
          InstanceSigs,
          JavaScriptFFI,
          LambdaCase,
          LinearTypes,
          MagicHash,
          MultiParamTypeClasses,
          MultiWayIf,
          OverloadedLabels,
          OverloadedRecordDot,
          ParallelListComp,
          PatternSynonyms,
          QualifiedDo,
          QuasiQuotes,
          RecursiveDo,
          RequiredTypeArguments,
          RoleAnnotations,
          StarIsType,
          StandaloneDeriving,
          StandaloneKindSignatures,
          TemplateHaskell,
          TransformListComp,
          TupleSections,
          TypeAbstractions,
          TypeApplications,
          TypeData,
          TypeFamilies,
          UnboxedSums,
          UnboxedTuples,
          UnicodeSyntax,
          ViewPatterns
        ]
    }
