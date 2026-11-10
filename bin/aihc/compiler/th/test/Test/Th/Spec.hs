{-# LANGUAGE OverloadedStrings #-}

-- | The result of a splice: the message decoder and the conversion to the
-- surface syntax.
module Test.Th.Spec (tests) where

import Aihc.Parser.Syntax
import Aihc.Th.Convert (expressionFromTree)
import Aihc.Th.Wire (Tree (..), decodeTree)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc-th"
    [ testCase "decodes the message of a splice" decodeTest,
      testCase "rejects a message that is not one complete tree" rejectTest,
      testCase "converts expressions to the surface syntax" convertTest,
      testCase "names a construct that it does not support" unsupportedTest
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
