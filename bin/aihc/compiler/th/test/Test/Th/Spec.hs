{-# LANGUAGE OverloadedStrings #-}

-- | The result of a splice: the message decoder and the conversion to the
-- surface syntax.
module Test.Th.Spec (tests) where

import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Aihc.Resolve.Types (GlobalName (..), PackageId (..), ResolutionNamespace (..), exactTerm, exactType)
import Aihc.Th.Convert (declarationsFromTree, expressionFromTree, typeResultFromTree)
import Aihc.Th.Request (Request (..), reportedFailure, requestFromTree)
import Aihc.Th.Splices (Splice (..), SpliceRequest (..), SpliceResult (..), moduleSplices, replaceModuleSplices, spliceModuleText)
import Aihc.Th.Wire (Tree (..), decodeTree)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc-th"
    [ testCase "decodes the message of a splice" decodeTest,
      testCase "rejects a message that is not one complete tree" rejectTest,
      testCase "converts expressions to the surface syntax" convertTest,
      testCase "gives a package-bound name its term or its type" exactNameTest,
      testCase "converts a type to the surface syntax" typeTest,
      testCase "converts declarations to the surface syntax" declarationsTest,
      testCase "converts an instance declaration to the surface syntax" instanceTest,
      testCase "converts a forall type and leaves out a pragma" forallTest,
      testCase "names a construct that it does not support" unsupportedTest,
      testCase "decodes the requests of a splice" requestTest,
      testCase "finds and replaces the splices of a module" splicesTest,
      testCase "writes the splice module" spliceModuleTest
    ]

-- | The message that the guest sent for @appE (varE (mkName "negate"))
-- (litE (integerL (-7)))@.
applicationMessage :: BS.ByteString
applicationMessage =
  "\0\4AppE\2\0\4VarE\1\0\4Name\2\0\7OccName\1\3\6negate\0\5NameS\0\0\4LitE\1\0\8IntegerL\1\1\2-7"

applicationTree :: Tree
applicationTree =
  TreeNode
    "AppE"
    [ TreeNode "VarE" [name "negate"],
      TreeNode "LitE" [TreeNode "IntegerL" [TreeInteger (-7)]]
    ]

decodeTest :: IO ()
decodeTest = do
  assertEqual "an application" (Right applicationTree) (decodeTree applicationMessage)
  -- The count 300 has two bytes, and the character is U+00E9.
  assertEqual "a text of 300 bytes" (Right (TreeString (T.replicate 300 "a"))) (decodeTree ("\3\172\2" <> BS.replicate 300 97))
  assertEqual "a character" (Right (TreeChar '\233')) (decodeTree "\2\233\1")
  assertEqual "a ratio" (Right (TreeRatio 0.75)) (decodeTree "\4\1\&3\1\&4")

rejectTest :: IO ()
rejectTest = do
  assertBool "a message without its last byte" (isLeft (decodeTree (BS.init applicationMessage)))
  assertBool "a message with one more byte" (isLeft (decodeTree (applicationMessage <> "\0")))
  assertBool "a constructor with more fields than bytes" (isLeft (decodeTree "\0\1A\255\255\255\127"))
  assertBool "a ratio with the denominator zero" (isLeft (decodeTree "\4\1\&1\1\&0"))
  assertBool "an empty message" (isLeft (decodeTree ""))

convertTest :: IO ()
convertTest = do
  assertEqual
    "a negative integer has parentheses"
    (Right (EApp (EVar "negate") (EParen (ENegate (EInt 7 TInteger "7")))))
    (expressionFromTree applicationTree)
  assertEqual
    "an application in an argument has parentheses"
    (Right (EApp (EVar "f") (EParen (EApp (EVar "g") (EVar "x")))))
    (expressionFromTree (TreeNode "AppE" [variable "f", TreeNode "AppE" [variable "g", variable "x"]]))
  -- Template Haskell gives (a + b) * c with no fixity. The parentheses keep
  -- that structure when name resolution applies the fixity of each operator.
  assertEqual
    "an infix operand keeps its structure"
    (Right (EInfix (EParen (EInfix (EVar "a") "+" (EVar "b"))) "*" (EVar "c")))
    (expressionFromTree (infixTree (Just (infixTree (Just (variable "a")) "+" (Just (variable "b")))) "*" (Just (variable "c"))))
  assertEqual
    "a section has parentheses"
    (Right (EParen (ESectionR "+" (EVar "x"))))
    (expressionFromTree (infixTree Nothing "+" (Just (variable "x"))))
  assertEqual
    "a lambda"
    (Right (ELambdaPats [PVar (mkUnqualifiedName NameVarId "x"), PWildcard] (EVar "x")))
    (expressionFromTree (TreeNode "LamE" [list [TreeNode "VarP" [name "x"], TreeNode "WildP" []], variable "x"]))
  assertEqual
    "a tuple with a string, a character, and a qualified constructor"
    (Right (ETuple Boxed [Just (EString "h\233llo" "\"h\\233llo\""), Just (EChar 'x' "'x'"), Just (EVar (mkName (Just "Data.Maybe") NameConId "Nothing"))]))
    ( expressionFromTree
        ( TreeNode
            "TupE"
            [ list
                [ TreeNode "Just" [TreeNode "LitE" [TreeNode "StringL" [TreeString "h\233llo"]]],
                  TreeNode "Just" [TreeNode "LitE" [TreeNode "CharL" [TreeChar 'x']]],
                  TreeNode "Just" [TreeNode "ConE" [TreeNode "Name" [TreeNode "OccName" [TreeString "Nothing"], TreeNode "NameQ" [TreeNode "ModName" [TreeString "Data.Maybe"]]]]]
                ]
            ]
        )
    )

typeTest :: IO ()
typeTest =
  assertEqual
    "a promoted constructor applied to a type-level literal"
    (Right (TApp (TCon "Just" Promoted) (TTypeLit (TypeLitInteger 255 "255"))))
    (typeResultFromTree (TreeNode "AppT" [TreeNode "PromotedT" [name "Just"], TreeNode "LitT" [TreeNode "NumTyLit" [TreeInteger 255]]]))

-- | The declarations for:
--
-- > swap :: (a, Int) -> [Maybe a]
-- > swap (x, 0) = [Just x]
-- > swap _ = none
-- >   where
-- >     none = []
declarationsTest :: IO ()
declarationsTest = do
  let binder = mkUnqualifiedName NameVarId
      constructor text = TreeNode "ConT" [name text]
      apply function argument = TreeNode "AppT" [function, argument]
      tree =
        list
          [ TreeNode
              "SigD"
              [ name "swap",
                apply
                  (apply (TreeNode "ArrowT" []) (apply (apply (TreeNode "TupleT" [TreeInteger 2]) (TreeNode "VarT" [name "a"])) (constructor "Int")))
                  (apply (TreeNode "ListT" []) (apply (constructor "Maybe") (TreeNode "VarT" [name "a"])))
              ],
            TreeNode
              "FunD"
              [ name "swap",
                list
                  [ TreeNode
                      "Clause"
                      [ list [TreeNode "TupP" [list [TreeNode "VarP" [name "x"], TreeNode "LitP" [TreeNode "IntegerL" [TreeInteger 0]]]]],
                        TreeNode "NormalB" [TreeNode "ListE" [list [TreeNode "AppE" [TreeNode "ConE" [name "Just"], variable "x"]]]],
                        list []
                      ],
                    TreeNode
                      "Clause"
                      [ list [TreeNode "WildP" []],
                        TreeNode "NormalB" [variable "none"],
                        list [TreeNode "ValD" [TreeNode "VarP" [name "none"], TreeNode "NormalB" [TreeNode "ListE" [list []]], list []]]
                      ]
                  ]
              ]
          ]
  assertEqual
    "a signature and a function with two clauses"
    ( Right
        [ DeclTypeSig
            [binder "swap"]
            ( TFun
                ArrowUnrestricted
                (TTuple Boxed Unpromoted [TVar (binder "a"), TCon "Int" Unpromoted])
                (TList Unpromoted [TApp (TCon "Maybe" Unpromoted) (TVar (binder "a"))])
            ),
          DeclValue
            ( FunctionBind
                (binder "swap")
                [ Match [] MatchHeadPrefix [PTuple Boxed [PVar (binder "x"), PLit (LitInt 0 TInteger "0")]] (UnguardedRhs [] (EList [EApp (EVar "Just") (EVar "x")]) Nothing),
                  Match
                    []
                    MatchHeadPrefix
                    [PWildcard]
                    (UnguardedRhs [] (EVar "none") (Just [DeclValue (FunctionBind (binder "none") [Match [] MatchHeadPrefix [] (UnguardedRhs [] (EList []) Nothing)])]))
                ]
            )
        ]
    )
    (declarationsFromTree tree)
  assertEqual
    "a data declaration"
    (Left "The compiler does not support the declaration DataD in a splice result.")
    (declarationsFromTree (list [TreeNode "DataD" []]))

-- | A name that a quote bound to its package and module keeps that term in
-- each scope.
exactNameTest :: IO ()
exactNameTest = do
  let tree =
        TreeNode
          "VarE"
          [ TreeNode
              "Name"
              [ TreeNode "OccName" [TreeString "length"],
                TreeNode "NameG" [TreeNode "VarName" [], TreeNode "PkgName" [TreeString "aihc-base-4.21.2.0"], TreeNode "ModName" [TreeString "GHC.Internal.Foldable"]]
              ]
          ]
  case expressionFromTree tree of
    Right (EVar variableName) -> do
      assertEqual "the text of the name" "length" (nameText variableName)
      assertEqual
        "the term of the name"
        (Just (GlobalName "length" (PackageId "aihc-base-4.21.2.0") "GHC.Internal.Foldable" ResolutionNamespaceTerm))
        (exactTerm variableName)
    other -> assertFailure ("expected a variable, got " <> show other)
  let typeTree =
        list
          [ TreeNode
              "SigD"
              [ name "x",
                TreeNode
                  "ConT"
                  [ TreeNode
                      "Name"
                      [ TreeNode "OccName" [TreeString "Int"],
                        TreeNode "NameG" [TreeNode "TcClsName" [], TreeNode "PkgName" [TreeString "aihc-prim-0.13.0"], TreeNode "ModName" [TreeString "GHC.Types"]]
                      ]
                  ]
              ]
          ]
  case declarationsFromTree typeTree of
    Right [DeclTypeSig _ (TCon typeName Unpromoted)] ->
      assertEqual
        "the type of the name"
        (Just (GlobalName "Int" (PackageId "aihc-prim-0.13.0") "GHC.Types" ResolutionNamespaceType))
        (exactType typeName)
    other -> assertFailure ("expected a signature with a type constructor, got " <> show other)

-- | A report is a request, and a report of an error makes the splice fail.
requestTest :: IO ()
requestTest = do
  let report isError message = TreeNode "Report" [TreeNode isError [], TreeString message]
  assertEqual "a warning" (Right (RequestReport False "w")) (requestFromTree (report "False" "w"))
  assertEqual "an error" (Right (RequestReport True "e")) (requestFromTree (report "True" "e"))
  assertEqual "an unknown request" (Left "The compiler does not know the splice request Lookup.") (requestFromTree (TreeNode "Lookup" []))
  let reify flavour = TreeNode "Reify" [TreeNode "Name" [TreeNode "OccName" [TreeString "T"], flavour]]
      bound space = TreeNode "NameG" [TreeNode space [], TreeNode "PkgName" [TreeString "p-1"], TreeNode "ModName" [TreeString "M"]]
  assertEqual
    "the facts of a type"
    (Right (RequestReify "T" (Just (GlobalName "T" (PackageId "p-1") "M" ResolutionNamespaceType))))
    (requestFromTree (reify (bound "TcClsName")))
  assertEqual
    "the facts of a data constructor"
    (Right (RequestReify "T" (Just (GlobalName "T" (PackageId "p-1") "M" ResolutionNamespaceTerm))))
    (requestFromTree (reify (bound "DataName")))
  assertEqual "the facts of a name with no module" (Right (RequestReify "T" Nothing)) (requestFromTree (reify (TreeNode "NameS" [])))
  assertEqual "only warnings" Nothing (reportedFailure [RequestReport False "w"])
  assertEqual
    "two errors and a warning"
    (Just "first\nsecond")
    (reportedFailure [RequestReport True "first", RequestReport False "w", RequestReport True "second"])

-- | An instance as a derived instance of a library gives it: a method with
-- a case expression, a record pattern, a guard, and a primitive literal,
-- and an @INLINE@ pragma that the conversion leaves out.
instanceTest :: IO ()
instanceTest = do
  let apply function argument = TreeNode "AppT" [function, argument]
      applyE function argument = TreeNode "AppE" [function, argument]
      clause patterns body = TreeNode "Clause" [list patterns, body, list []]
      match pattern' body = TreeNode "Match" [pattern', body, list []]
      tree =
        list
          [ TreeNode
              "InstanceD"
              [ TreeNode "Nothing" [],
                list [apply (TreeNode "ConT" [name "Eq"]) (TreeNode "VarT" [name "a"])],
                apply (TreeNode "ConT" [name "Eq"]) (apply (TreeNode "ConT" [name "Box"]) (TreeNode "VarT" [name "a"])),
                list
                  [ TreeNode "PragmaD" [TreeNode "InlineP" [name "==", TreeNode "Inline" [], TreeNode "FunLike" [], TreeNode "AllPhases" []]],
                    TreeNode
                      "FunD"
                      [ name "==",
                        list
                          [ clause
                              [TreeNode "RecP" [name "Box", list [TreeNode "(,)" [name "unBox", TreeNode "VarP" [name "x"]]]], TreeNode "VarP" [name "y"]]
                              ( TreeNode
                                  "NormalB"
                                  [ TreeNode
                                      "CaseE"
                                      [ variable "y",
                                        list
                                          [ match
                                              (TreeNode "ConP" [name "Box", list [], list [TreeNode "VarP" [name "z"]]])
                                              (TreeNode "GuardedB" [list [TreeNode "(,)" [TreeNode "NormalG" [applyE (applyE (variable "==") (variable "x")) (variable "z")], TreeNode "ConE" [name "True"]]]]),
                                            match (TreeNode "WildP" []) (TreeNode "NormalB" [TreeNode "ConE" [name "False"]])
                                          ]
                                      ]
                                  ]
                              )
                          ]
                      ]
                  ]
              ],
            TreeNode "ValD" [TreeNode "VarP" [name "zero"], TreeNode "NormalB" [TreeNode "LitE" [TreeNode "IntPrimL" [TreeInteger 0]]], list []]
          ]
  assertEqual
    "an instance with a case expression, a record pattern, and a guard"
    (Right "instance Eq a => Eq (Box a) where (==) Box {unBox = x} y = case y of Box z | (==) x z -> True _ -> False zero = 0#")
    (T.unwords . map render <$> declarationsFromTree tree)
  assertEqual
    "an instance with an overlap pragma"
    (Left "The compiler does not support the overlap pragma Just in a splice result.")
    (declarationsFromTree (list [TreeNode "InstanceD" [TreeNode "Just" [TreeNode "Overlapping" []], list [], TreeNode "ConT" [name "C"], list []]]))

-- | A signature with an explicit forall, a binder with a kind, and a
-- context, as a library that makes a function signature gives it. A
-- top-level @INLINE@ pragma does not change the meaning, and the
-- conversion leaves it out.
forallTest :: IO ()
forallTest = do
  let apply function argument = TreeNode "AppT" [function, argument]
      tree =
        list
          [ TreeNode "PragmaD" [TreeNode "InlineP" [name "f", TreeNode "Inline" [], TreeNode "FunLike" [], TreeNode "AllPhases" []]],
            TreeNode
              "SigD"
              [ name "f",
                TreeNode
                  "ForallT"
                  [ list [TreeNode "PlainTV" [name "a", TreeNode "SpecifiedSpec" []], TreeNode "KindedTV" [name "m", TreeNode "SpecifiedSpec" [], apply (apply (TreeNode "ArrowT" []) (TreeNode "ConT" [name "Type"])) (TreeNode "ConT" [name "Type"])]],
                    list [apply (TreeNode "ConT" [name "Show"]) (TreeNode "VarT" [name "a"])],
                    apply (apply (TreeNode "ArrowT" []) (TreeNode "VarT" [name "a"])) (apply (TreeNode "VarT" [name "m"]) (TreeNode "ConT" [name "String"]))
                  ]
              ]
          ]
  assertEqual
    "a signature with a forall and a context"
    (Right "f :: forall a (m :: Type -> Type). Show a => a -> m String")
    (T.unwords . map render <$> declarationsFromTree tree)

unsupportedTest :: IO ()
unsupportedTest = do
  -- A unique name has its number in its text, so two names with the same
  -- text from two calls of newName are different names.
  assertEqual
    "a name that Template Haskell made unique"
    (Right (EVar "x#u7"))
    (expressionFromTree (TreeNode "VarE" [TreeNode "Name" [TreeNode "OccName" [TreeString "x"], TreeNode "NameU" [TreeInteger 7]]]))
  assertEqual
    "a name that is bound to a position in the source"
    (Left "The compiler does not support the name flavour NameL in a splice result.")
    (expressionFromTree (TreeNode "VarE" [TreeNode "Name" [TreeNode "OccName" [TreeString "x"], TreeNode "NameL" [TreeInteger 1]]]))
  assertEqual
    "a do expression"
    (Left "The compiler does not support the expression DoE in a splice result.")
    (expressionFromTree (TreeNode "DoE" [TreeNode "Nothing" [], list []]))

-- | A module with a splice in each position that matters: with and
-- without parentheses, in a quote, in the body of a splice, and in the
-- position of a declaration, with and without the splice syntax.
spliceSource :: Text
spliceSource =
  T.unlines
    [ "module M where",
      "import Data.List (sort)",
      "import Own.Module",
      "x = $(a 1) + f $b [| $(c) |]",
      "$(g 2)",
      "y = $(d $(e))",
      "z = 0 :: $(t)",
      "h"
    ]

parseSpliceSource :: IO Module
parseSpliceSource = do
  let (errors, modu) = parseModule defaultConfig {parserExtensions = [TemplateHaskell]} spliceSource
  assertEqual "parse errors" [] errors
  pure modu

-- | The text of a syntax tree with one space between its words. The
-- layout of the pretty-printer is not the subject of these tests.
render :: (Pretty syntax) => syntax -> Text
render = T.unwords . T.words . renderStrict . layoutPretty defaultLayoutOptions . pretty

splicesTest :: IO ()
splicesTest = do
  modu <- parseSpliceSource
  -- The splice in the quote and the splice in the body of a splice do not
  -- run for this module.
  assertEqual
    "the splices, in source order"
    [("expression", "(a 1)"), ("expression", "b"), ("declaration", "(g 2)"), ("expression", "(d $(e))"), ("type", "(t)"), ("declaration", "h")]
    (map kindAndBody (moduleSplices modu))
  signature <- declarationOf "s :: T"
  value <- declarationOf "s = t"
  let replaced =
        replaceModuleSplices
          [ExpressionResult (EVar "p"), ExpressionResult (EVar "q"), DeclarationResult [signature, value], ExpressionResult (EVar "r"), TypeResult (TCon "Int" Unpromoted), DeclarationResult []]
          modu
  assertEqual "no splice is left outside the quote" [] (map kindAndBody (moduleSplices replaced))
  assertEqual
    "an expression and a type have parentheses, declarations take the position of their splice, and the quote keeps its splice"
    "module M where import Data.List (sort) import Own.Module x = (p) + f (q) [| $(c) |] s :: T s = t y = (r) z = 0 :: (Int)"
    (render replaced)
  where
    kindAndBody splice =
      case splice of
        ExpressionSplice body -> ("expression" :: Text, render body)
        TypeSplice body -> ("type", render body)
        DeclarationSplice body -> ("declaration", render body)
    declarationOf source =
      case parseModule defaultConfig (T.unlines ["module D where", source]) of
        ([], Module {moduleDecls = [decl]}) -> pure decl
        other -> assertFailure ("expected one declaration, got " <> show other)

spliceModuleTest :: IO ()
spliceModuleTest = do
  modu <- parseSpliceSource
  let request =
        SpliceRequest
          { spliceRequestPackage = "demo",
            spliceRequestModule = "M",
            spliceRequestStoreRoot = "store",
            spliceRequestBuildRoot = "build",
            spliceRequestDependencies = [("base", [4, 21, 2, 0])],
            spliceRequestExtensions = [],
            spliceRequestLanguage = Nothing,
            spliceRequestPragmas = [EnableExtension TemplateHaskell],
            spliceRequestImports = take 1 (moduleImports modu),
            spliceRequestSplices = moduleSplices modu
          }
  assertEqual
    "one closure for each splice"
    "{-# LANGUAGE TemplateHaskell #-} module AihcSplice where import Data.List (sort) import qualified Language.Haskell.TH.Lib as AihcQuoteLibrary import qualified Language.Haskell.TH.Syntax as AihcQuoteLibrary import qualified Aihc.TH.Splice aihcSplice0 = Aihc.TH.Splice.runExpressionSplice (a 1) aihcSplice1 = Aihc.TH.Splice.runExpressionSplice (b) aihcSplice2 = Aihc.TH.Splice.runDeclarationsSplice (g 2) aihcSplice3 = Aihc.TH.Splice.runExpressionSplice (d $(e)) aihcSplice4 = Aihc.TH.Splice.runTypeSplice (t) aihcSplice5 = Aihc.TH.Splice.runDeclarationsSplice (h)"
    (T.unwords (T.words (spliceModuleText request)))

name :: Text -> Tree
name occurrence = TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], TreeNode "NameS" []]

variable :: Text -> Tree
variable occurrence = TreeNode "VarE" [name occurrence]

infixTree :: Maybe Tree -> Text -> Maybe Tree -> Tree
infixTree left operator right = TreeNode "InfixE" [optional left, variable operator, optional right]
  where
    optional = maybe (TreeNode "Nothing" []) (\value -> TreeNode "Just" [value])

list :: [Tree] -> Tree
list = foldr (\first rest -> TreeNode "(:)" [first, rest]) (TreeNode "[]" [])
