{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Anf (tests) where

import Aihc.Grin.Anf (normalizeGrinExpr)
import Aihc.Grin.Syntax
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "normalization"
    [ testCase "a nested bind is reassociated into the enclosing spine" $
        assertEqual
          "spine"
          ( GrinBind
              [var "y"]
              (eval (value "z"))
              (GrinBind [var "x"] (eval (value "y")) (store [value "x"]))
          )
          ( normalizeGrinExpr
              ( GrinBind
                  [var "x"]
                  (GrinBind [var "y"] (eval (value "z")) (eval (value "y")))
                  (store [value "x"])
              )
          ),
      testCase "a copy bind disappears and its value takes its place" $
        assertEqual
          "substituted"
          (store [intValue "rightInt"])
          ( normalizeGrinExpr
              (GrinBind [intVar "argument"] (GrinConstant [intValue "rightInt"]) (store [intValue "argument"]))
          ),
      testCase "a literal reaches the use the copy bind stood between" $
        assertEqual
          "substituted"
          (store [GrinLitValue (GrinLitInt IntRep 42)])
          ( normalizeGrinExpr
              ( GrinBind
                  [intVar "argument"]
                  (GrinConstant [GrinLitValue (GrinLitInt IntRep 42)])
                  (store [intValue "argument"])
              )
          ),
      testCase "a constant that ends a body is a result and stays" $
        assertEqual
          "result"
          (GrinBind [var "x"] (eval (value "z")) (GrinConstant [value "x"]))
          ( normalizeGrinExpr
              (GrinBind [var "x"] (eval (value "z")) (GrinConstant [value "x"]))
          ),
      -- Flattening is what brings the trailing constant of a value expression
      -- into view of the bind above it, so the two rules only compose in one
      -- pass.
      testCase "a copy bind ending a nested value expression disappears" $
        assertEqual
          "substituted"
          (GrinBind [var "y"] (eval (value "z")) (store [value "y"]))
          ( normalizeGrinExpr
              ( GrinBind
                  [var "x"]
                  (GrinBind [var "y"] (eval (value "z")) (GrinConstant [value "y"]))
                  (store [value "x"])
              )
          ),
      testCase "a bind whose representation differs is not a copy" $
        assertEqual
          "kept"
          (GrinBind [wordVar "argument"] (GrinConstant [intValue "source"]) (store [wordValue "argument"]))
          ( normalizeGrinExpr
              (GrinBind [wordVar "argument"] (GrinConstant [intValue "source"]) (store [wordValue "argument"]))
          ),
      testCase "a bind whose value count differs is not a copy" $
        assertEqual
          "kept"
          (GrinBind [intVar "a", intVar "b"] (GrinConstant [intValue "one"]) (store [intValue "a"]))
          ( normalizeGrinExpr
              (GrinBind [intVar "a", intVar "b"] (GrinConstant [intValue "one"]) (store [intValue "a"]))
          ),
      testCase "rebinding a substituted name ends its substitution" $
        assertEqual
          "shadowed"
          (GrinBind [var "x"] (eval (value "w")) (store [value "x"]))
          ( normalizeGrinExpr
              ( GrinBind
                  [var "x"]
                  (GrinConstant [value "y"])
                  (GrinBind [var "x"] (eval (value "w")) (store [value "x"]))
              )
          ),
      testCase "rebinding the name a substitution stands for ends it" $
        assertEqual
          "not captured"
          (GrinBind [var "y"] (eval (value "w")) (store [value "x"]))
          ( normalizeGrinExpr
              ( GrinBind
                  [var "x"]
                  (GrinConstant [value "y"])
                  (GrinBind [var "y"] (eval (value "w")) (store [value "x"]))
              )
          ),
      testCase "an alternative sees the substitutions in force around it" $
        assertEqual
          "substituted"
          (GrinCase (value "scrutinee") (var "_scrut") [alt (store [value "scrutinee"])])
          ( normalizeGrinExpr
              ( GrinBind
                  [var "x"]
                  (GrinConstant [value "scrutinee"])
                  (GrinCase (value "x") (var "_scrut") [alt (store [value "x"])])
              )
          ),
      testCase "an alternative binder shadows the substitution it would capture" $
        assertEqual
          "not captured"
          (GrinCase (value "scrutinee") (var "_scrut") [altBinding "y" (store [value "x"])])
          ( normalizeGrinExpr
              ( GrinBind
                  [var "x"]
                  (GrinConstant [value "y"])
                  (GrinCase (value "scrutinee") (var "_scrut") [altBinding "y" (store [value "x"])])
              )
          )
    ]

var :: Text -> GrinVar
var name = GrinVar name 0 liftedGrinRep

intVar :: Text -> GrinVar
intVar name = GrinVar name 0 IntRep

wordVar :: Text -> GrinVar
wordVar name = GrinVar name 0 WordRep

value :: Text -> GrinValue
value = GrinVarValue . var

intValue :: Text -> GrinValue
intValue = GrinVarValue . intVar

wordValue :: Text -> GrinValue
wordValue = GrinVarValue . wordVar

eval :: GrinValue -> GrinExpr
eval = GrinEval liftedGrinRep

store :: [GrinValue] -> GrinExpr
store = GrinStore . GrinNode (GrinConstructor "Box" 0)

alt :: GrinExpr -> GrinAlt
alt = GrinAlt GrinDefaultAlt []

altBinding :: Text -> GrinExpr -> GrinAlt
altBinding name = GrinAlt GrinDefaultAlt [var name]
