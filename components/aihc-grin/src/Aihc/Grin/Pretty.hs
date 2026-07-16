-- | Human-readable GRIN rendering for diagnostics and golden tests.
module Aihc.Grin.Pretty
  ( renderProgram,
    renderExpr,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Prim (PrimOp, primOpArity, primOpName)
import Aihc.Tc.Types (RuntimeRep)
import Data.List (intercalate)
import Data.Text qualified as T

renderProgram :: GrinProgram -> String
renderProgram program =
  intercalate
    "\n\n"
    ( map renderConstructor (grinConstructors program)
        <> map renderPrimitive (grinPrimitives program)
        <> map renderForeign (grinForeignCalls program)
        <> map renderCaf (grinCafs program)
        <> map renderFunction (grinFunctions program)
    )

renderConstructor :: (T.Text, [RuntimeRep]) -> String
renderConstructor (name, fieldReps) =
  "constructor "
    <> T.unpack name
    <> "/"
    <> show (length fieldReps)
    <> " ["
    <> intercalate ", " (map show fieldReps)
    <> "]"

renderPrimitive :: (GrinVar, PrimOp) -> String
renderPrimitive (var, primOp) =
  "primitive " <> renderVar var <> "/" <> show (primOpArity primOp)

renderForeign :: GrinForeignCall -> String
renderForeign foreignCall =
  "foreign \""
    <> T.unpack (grinForeignCallSymbol foreignCall)
    <> "\" "
    <> T.unpack (grinForeignCallName foreignCall)

renderCaf :: (GrinVar, GrinNode) -> String
renderCaf (var, node) =
  "caf " <> renderVar var <> " = " <> renderNode node

renderFunction :: GrinFunction -> String
renderFunction function =
  T.unpack (unFunctionName (grinFunctionName function))
    <> concatMap ((" " <>) . renderVar) (grinFunctionParameters function)
    <> " -> "
    <> show (grinFunctionResultRep function)
    <> " =\n"
    <> renderExprIndented 2 (grinFunctionBody function)

renderExpr :: GrinExpr -> String
renderExpr = renderExprIndented 0

renderExprIndented :: Int -> GrinExpr -> String
renderExprIndented indentation expr =
  case expr of
    GrinReturn value -> indent indentation <> "return " <> renderValue value
    GrinBind var valueExpr body ->
      indent indentation
        <> renderVar var
        <> " <-\n"
        <> renderExprIndented (indentation + 2) valueExpr
        <> "\n"
        <> renderExprIndented indentation body
    GrinStore node -> indent indentation <> "store " <> renderNode node
    GrinStoreRec bindings body ->
      indent indentation
        <> "store-rec\n"
        <> intercalate
          "\n"
          [ indent (indentation + 2) <> renderVar var <> " = " <> renderNode node
          | (var, node) <- bindings
          ]
        <> "\n"
        <> renderExprIndented indentation body
    GrinFetch runtimeRep pointer ->
      indent indentation <> "fetch @" <> show runtimeRep <> " " <> renderValue pointer
    GrinUpdate pointer value ->
      indent indentation <> "update " <> renderValue pointer <> " " <> renderValue value
    GrinEval runtimeRep value ->
      indent indentation <> "eval @" <> show runtimeRep <> " " <> renderValue value
    GrinApply runtimeRep function argument ->
      indent indentation
        <> "apply @"
        <> show runtimeRep
        <> " "
        <> renderValue function
        <> " "
        <> renderValue argument
    GrinCase scrutinee binder alternatives ->
      indent indentation
        <> "case "
        <> renderValue scrutinee
        <> " as "
        <> renderVar binder
        <> " of\n"
        <> intercalate "\n" (map (renderAlt (indentation + 2)) alternatives)
    GrinDictSelect runtimeRep dictionary index ->
      indent indentation
        <> "select @"
        <> show runtimeRep
        <> " "
        <> renderValue dictionary
        <> " "
        <> show index
    GrinThrow exception -> indent indentation <> "throw " <> renderValue exception
    GrinCatch runtimeRep action handler state ->
      indent indentation
        <> "catch @"
        <> show runtimeRep
        <> " "
        <> unwords (map renderValue [action, handler, state])
    GrinScheduler runtimeRep schedulerOp arguments ->
      indent indentation
        <> "scheduler @"
        <> show runtimeRep
        <> " "
        <> show schedulerOp
        <> concatMap ((" " <>) . renderValue) arguments

renderAlt :: Int -> GrinAlt -> String
renderAlt indentation alt =
  indent indentation
    <> renderAltCon (grinAltCon alt)
    <> concatMap ((" " <>) . renderVar) (grinAltBinders alt)
    <> " ->\n"
    <> renderExprIndented (indentation + 2) (grinAltRhs alt)

renderAltCon :: GrinAltCon -> String
renderAltCon altCon =
  case altCon of
    GrinDataAlt name -> T.unpack name
    GrinLitAlt literal -> renderLiteral literal
    GrinDefaultAlt -> "_"

renderValue :: GrinValue -> String
renderValue value =
  case value of
    GrinVarValue var -> renderVar var
    GrinLitValue literal -> renderLiteral literal
    GrinNodeValue node -> renderNode node

renderNode :: GrinNode -> String
renderNode node =
  "("
    <> renderNodeTag (grinNodeTag node)
    <> concatMap ((" " <>) . renderValue) (grinNodeFields node)
    <> ")"

renderNodeTag :: GrinNodeTag -> String
renderNodeTag nodeTag =
  case nodeTag of
    GrinConstructor name -> "C" <> T.unpack name
    GrinClosure functionName -> "P" <> T.unpack (unFunctionName functionName)
    GrinThunk functionName -> "F" <> T.unpack (unFunctionName functionName)
    GrinPrimitive primOp -> "Prim[" <> T.unpack (primOpName primOp) <> "/" <> show (primOpArity primOp) <> "]"
    GrinForeign foreignCall -> "Foreign[" <> T.unpack (grinForeignCallName foreignCall) <> "]"
    GrinForeignIOAction foreignCall -> "ForeignIO[" <> T.unpack (grinForeignCallName foreignCall) <> "]"
    GrinDictionary -> "Dict"

renderLiteral :: GrinLiteral -> String
renderLiteral literal =
  case literal of
    GrinLitInt _ value -> show value
    GrinLitChar _ value -> show value
    GrinLitString value -> show (T.unpack value)

renderVar :: GrinVar -> String
renderVar var =
  T.unpack (grinVarName var)
    <> "%"
    <> show (grinVarUnique var)
    <> " :: "
    <> show (grinVarRuntimeRep var)

indent :: Int -> String
indent count = replicate count ' '
