{-# LANGUAGE OverloadedStrings #-}

-- | The result of a splice: the message decoder and the conversion to the
-- surface syntax.
module Test.Th.Spec (tests) where

import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Aihc.Resolve.Types (GlobalName (..), PackageId (..), ResolutionNamespace (..), exactTerm)
import Aihc.Th.Convert (expressionFromTree)
import Aihc.Th.Splices (SpliceRequest (..), moduleSplices, replaceModuleSplices, spliceModuleText)
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
      testCase "gives a package-bound name its term" exactNameTest,
      testCase "names a construct that it does not support" unsupportedTest,
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

unsupportedTest :: IO ()
unsupportedTest = do
  assertEqual
    "a name that Template Haskell made unique"
    (Left "The compiler does not support the name flavour NameU in a splice result.")
    (expressionFromTree (TreeNode "VarE" [TreeNode "Name" [TreeNode "OccName" [TreeString "x"], TreeNode "NameU" [TreeInteger 1]]]))
  assertEqual
    "a do expression"
    (Left "The compiler does not support the expression DoE in a splice result.")
    (expressionFromTree (TreeNode "DoE" [TreeNode "Nothing" [], list []]))

-- | A module with a splice in each position that matters: with and
-- without parentheses, in a quote, and in the body of a splice.
spliceSource :: Text
spliceSource =
  T.unlines
    [ "module M where",
      "import Data.List (sort)",
      "import Own.Module",
      "x = $(a 1) + f $b [| $(c) |]",
      "y = $(d $(e))"
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
  assertEqual "the splice bodies, in source order" ["(a 1)", "b", "(d $(e))"] (map render (moduleSplices modu))
  let replaced = replaceModuleSplices [EVar "p", EVar "q", EVar "r"] modu
  assertEqual "no splice is left outside the quote" [] (map render (moduleSplices replaced))
  assertEqual
    "each result has parentheses and the quote keeps its splice"
    "module M where import Data.List (sort) import Own.Module x = (p) + f (q) [| $(c) |] y = (r)"
    (render replaced)

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
            spliceRequestBodies = moduleSplices modu
          }
  assertEqual
    "one closure for each splice"
    "{-# LANGUAGE TemplateHaskell #-} module AihcSplice where import Data.List (sort) import qualified Aihc.TH.Splice aihcSplice0 = Aihc.TH.Splice.runExpressionSplice (a 1) aihcSplice1 = Aihc.TH.Splice.runExpressionSplice (b) aihcSplice2 = Aihc.TH.Splice.runExpressionSplice (d $(e))"
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
