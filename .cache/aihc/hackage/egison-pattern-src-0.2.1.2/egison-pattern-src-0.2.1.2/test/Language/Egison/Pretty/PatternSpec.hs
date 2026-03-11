module Language.Egison.Pretty.PatternSpec
  ( test_atom_patterns
  , test_primitive_pattern_operators
  , test_user_defined_pattern_operators
  )
where

import           TestImport

import           Data.Text                      ( Text )
import           Test.Tasty
import           Test.Tasty.HUnit


assertPrintExpr :: Expr Name Name ValueExpr -> Text -> Assertion
assertPrintExpr e expected = case prettyExpr testPrintMode e of
  Left  err -> fail $ show err
  Right got -> assertEqual ("while printing \"" ++ show e ++ "\"") expected got

test_atom_patterns :: [TestTree]
test_atom_patterns =
  [ testCase "wildcard pattern" $ assertPrintExpr Wildcard "_"
  , testCase "variable pattern" $ assertPrintExpr (Variable $ Name "x") "$x"
  , testCase "value pattern" $ assertPrintExpr (Value $ ValueExprInt 10) "#10"
  , testCase "predicate pattern"
    $ assertPrintExpr (Predicate $ ValueExprInt 10) "?10"
  , testCase "constructor pattern" $ assertPrintExpr
    (Pattern (Name "ctor") [Wildcard, Wildcard, Wildcard])
    "ctor _ _ _"
  , testCase "constructor pattern without arguments"
    $ assertPrintExpr (Pattern (Name "nil") []) "nil"
  , testCase "nested constructor pattern" $ assertPrintExpr
    (Pattern
      (Name "ctorA")
      [ Pattern (Name "ctorB") [Wildcard]
      , Pattern (Name "ctorC") [Wildcard, Pattern (Name "ctorD") [Wildcard]]
      , Wildcard
      , Pattern (Name "ctorE") []
      ]
    )
    "ctorA (ctorB _) (ctorC _ (ctorD _)) _ ctorE"
  , testCase "collection pattern"
    $ assertPrintExpr (Collection [Wildcard, Wildcard, Wildcard]) "[_, _, _]"
  , testCase "nested collection pattern" $ assertPrintExpr
    (Collection [Wildcard, Collection [Wildcard, Wildcard], Wildcard])
    "[_, [_, _], _]"
  , testCase "nil collection pattern" $ assertPrintExpr (Collection []) "[]"
  , testCase "tuple pattern"
    $ assertPrintExpr (Tuple [Wildcard, Wildcard, Wildcard]) "(_, _, _)"
  , testCase "nested tuple pattern" $ assertPrintExpr
    (Tuple [Wildcard, Tuple [Wildcard, Wildcard], Wildcard])
    "(_, (_, _), _)"
  , testCase "nil tuple pattern" $ assertPrintExpr (Tuple []) "()"
  , testCase "not pattern" $ assertPrintExpr (Not Wildcard) "!_"
  , testCase "not pattern in constructor arguments" $ assertPrintExpr
    (Pattern (Name "ctor") [Not Wildcard, Not Wildcard])
    "ctor !_ !_"
  , testCase "not pattern on constructor pattern" $ assertPrintExpr
    (Not (Pattern (Name "ctor") [Wildcard, Wildcard]))
    "!(ctor _ _)"
  , testCase "nested not patterns" $ assertPrintExpr (Not (Not Wildcard)) "!!_"
  , testCase "tuple pattern that contains value pattern" $ assertPrintExpr
    (Tuple [Value (ValueExprInt 1), Value (ValueExprInt 2)])
    "(#1, #2)"
  , testCase "collection pattern that contains value pattern" $ assertPrintExpr
    (Collection [Value (ValueExprInt 1), Value (ValueExprInt 2)])
    "[#1, #2]"
  ]

test_primitive_pattern_operators :: [TestTree]
test_primitive_pattern_operators =
  [ testCase "and pattern" $ assertPrintExpr (And Wildcard Wildcard) "_ & _"
  , testCase "or pattern" $ assertPrintExpr (Or Wildcard Wildcard) "_ | _"
  -- associativity
  , testCase "nested and pattern"
    $ assertPrintExpr (And Wildcard (And Wildcard Wildcard)) "_ & _ & _"
  , testCase "nested and pattern (with parentheses)"
    $ assertPrintExpr (And (And Wildcard Wildcard) Wildcard) "(_ & _) & _"
  , testCase "nested or pattern"
    $ assertPrintExpr (Or Wildcard (Or Wildcard Wildcard)) "_ | _ | _"
  , testCase "nested or pattern (with parentheses)"
    $ assertPrintExpr (Or (Or Wildcard Wildcard) Wildcard) "(_ | _) | _"
  -- precedence
  , testCase "nested and, or pattern" $ assertPrintExpr
    (Or (And Wildcard Wildcard) (And Wildcard Wildcard))
    "_ & _ | _ & _"
  , testCase "nested and, or pattern (with parentheses)" $ assertPrintExpr
    (And (Or Wildcard Wildcard) (Or Wildcard Wildcard))
    "(_ | _) & (_ | _)"
  , testCase "nested not, or pattern"
    $ assertPrintExpr (Or (Not Wildcard) Wildcard) "!_ | _"
  , testCase "nested not, and pattern"
    $ assertPrintExpr (Not (And Wildcard Wildcard)) "!(_ & _)"
  , testCase "constructor pattern in infix operands" $ assertPrintExpr
    (Or (And Wildcard (Pattern (Name "ctor") [Wildcard, Wildcard]))
        (Pattern (Name "ctor") [Wildcard, Wildcard])
    )
    "_ & ctor _ _ | ctor _ _"
  ]

test_user_defined_pattern_operators :: [TestTree]
test_user_defined_pattern_operators =
  [ testCase "cons pattern"
    $ assertPrintExpr (Infix (Name ":") Wildcard Wildcard) "_ : _"
  , testCase "join pattern"
    $ assertPrintExpr (Infix (Name "++") Wildcard Wildcard) "_ ++ _"
  -- associativity
  , testCase "nested cons pattern" $ assertPrintExpr
    (Infix (Name ":") Wildcard (Infix (Name ":") Wildcard Wildcard))
    "_ : _ : _"
  , testCase "nested join pattern" $ assertPrintExpr
    (Infix (Name "++") Wildcard (Infix (Name "++") Wildcard Wildcard))
    "_ ++ _ ++ _"
  , testCase "nested join, cons pattern" $ assertPrintExpr
    (Infix (Name "++") Wildcard (Infix (Name ":") Wildcard Wildcard))
    "_ ++ _ : _"
  , testCase "nested cons, join pattern" $ assertPrintExpr
    (Infix (Name ":") Wildcard (Infix (Name "++") Wildcard Wildcard))
    "_ : _ ++ _"
  , testCase "nested cons, join pattern (with parentheses)" $ assertPrintExpr
    (Infix (Name "++") (Infix (Name ":") Wildcard Wildcard) Wildcard)
    "(_ : _) ++ _"
  -- and more
  , testCase "precedence" $ assertPrintExpr
    (Infix (Name "<|") (Infix (Name ":") Wildcard Wildcard) Wildcard)
    "_ : _ <| _"
  , testCase "precedence (with parentheses)" $ assertPrintExpr
    (Infix (Name ":") Wildcard (Infix (Name "<|") Wildcard Wildcard))
    "_ : (_ <| _)"
  , testCase "associativity" $ assertPrintExpr
    (Infix (Name "|>") (Infix (Name "|>") Wildcard Wildcard) Wildcard)
    "_ |> _ |> _"
  , testCase "associativity (with parentheses)" $ assertPrintExpr
    (Infix (Name "|>") Wildcard (Infix (Name "|>") Wildcard Wildcard))
    "_ |> (_ |> _)"
  ]
