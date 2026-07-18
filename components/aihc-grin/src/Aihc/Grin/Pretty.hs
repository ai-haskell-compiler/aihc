-- | Human-readable GRIN rendering for diagnostics and golden tests.
module Aihc.Grin.Pretty
  ( renderProgram,
    renderExpr,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (..))
import Data.List (intercalate)
import Data.Text qualified as T

renderProgram :: GrinProgram -> String
renderProgram program =
  intercalate
    "\n\n"
    ( map renderConstructor (grinConstructors program)
        <> map renderPrimitive (grinPrimitives program)
        <> map renderForeign (grinForeignCalls program)
        <> map (("external global " <>) . T.unpack) (grinExternalGlobals program)
        <> map renderExternalFunction (grinExternalFunctions program)
        <> map renderGlobal (grinWhnfGlobals program)
        <> map renderCaf (grinCafs program)
        <> map renderFunction (grinFunctions program)
    )

renderExternalFunction :: GrinCodeInfo -> String
renderExternalFunction info =
  "external "
    <> T.unpack (grinCodeSourceName info)
    <> "/"
    <> show (length (grinCodeParameterLayouts info))
    <> " = "
    <> T.unpack (unFunctionName (grinCodeFunctionName info))

renderConstructor :: (T.Text, [RuntimeRep]) -> String
renderConstructor (name, fieldReps) =
  "constructor "
    <> T.unpack name
    <> "/"
    <> show (length fieldReps)
    <> " ["
    <> intercalate ", " (map show fieldReps)
    <> "]"

renderPrimitive :: (GrinVar, Int) -> String
renderPrimitive (var, arity) =
  "primitive " <> renderVar var <> "/" <> show arity

renderForeign :: GrinForeignCall -> String
renderForeign foreignCall =
  "foreign \""
    <> T.unpack (grinForeignCallSymbol foreignCall)
    <> "\" "
    <> T.unpack (grinForeignCallName foreignCall)

renderGlobal :: (GrinVar, GrinNode) -> String
renderGlobal (var, node) =
  "global " <> renderVar var <> " = " <> renderNode node

renderCaf :: (GrinVar, GrinNode) -> String
renderCaf (var, node) =
  "caf " <> renderVar var <> " = " <> renderNode node

renderFunction :: GrinFunction -> String
renderFunction function =
  T.unpack (unFunctionName (grinFunctionName function))
    <> concatMap ((" " <>) . renderVarAtom) (grinFunctionParameters function)
    <> " -> "
    <> show (grinFunctionResultRep function)
    <> " =\n"
    <> renderExprIndented 2 (grinFunctionBody function)

renderExpr :: GrinExpr -> String
renderExpr = renderExprIndented 0

renderExprIndented :: Int -> GrinExpr -> String
renderExprIndented indentation expr =
  case expr of
    GrinConstant values -> indent indentation <> "constant" <> renderValues values
    GrinBind vars valueExpr body ->
      indent indentation
        <> renderBinders vars
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
      indent indentation <> "fetch @" <> renderRuntimeRepArgument runtimeRep <> " " <> renderValue pointer
    GrinUpdate pointer value ->
      indent indentation <> "update " <> renderValue pointer <> " " <> renderValue value
    GrinEval runtimeRep value ->
      indent indentation <> "eval @" <> renderRuntimeRepArgument runtimeRep <> " " <> renderValue value
    GrinCall runtimeRep functionName arguments ->
      indent indentation
        <> "call @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> T.unpack (unFunctionName functionName)
        <> renderValues arguments
    GrinPrimitiveCall runtimeRep name arguments ->
      indent indentation
        <> "primitive-call @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> T.unpack name
        <> renderValues arguments
    GrinApply runtimeRep function arguments ->
      indent indentation
        <> "apply @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> renderValue function
        <> renderValues arguments
    GrinCase scrutinee binder alternatives ->
      indent indentation
        <> "case "
        <> renderValue scrutinee
        <> " as "
        <> renderVar binder
        <> " of\n"
        <> intercalate "\n" (map (renderAlt (indentation + 2)) alternatives)
    GrinThrow exception -> indent indentation <> "throw " <> renderValue exception
    GrinCatch runtimeRep action handler state ->
      indent indentation
        <> "catch @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> unwords (map renderValue [action, handler])
        <> renderValues state
    GrinForeignCallExpr foreignCall arguments ->
      indent indentation
        <> "foreign-call "
        <> T.unpack (grinForeignCallName foreignCall)
        <> concatMap ((" " <>) . renderValue) arguments

renderValues :: [GrinValue] -> String
renderValues = concatMap ((" " <>) . renderValue)

renderBinders :: [GrinVar] -> String
renderBinders vars =
  case vars of
    [] -> "()"
    _ -> intercalate ", " (map renderVar vars)

renderAlt :: Int -> GrinAlt -> String
renderAlt indentation alt =
  indent indentation
    <> renderAltCon (grinAltCon alt)
    <> concatMap ((" " <>) . renderVarAtom) (grinAltBinders alt)
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
    GrinVarValue var -> renderVarAtom var
    GrinLitValue literal -> renderLiteral literal

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
    GrinClosure functionName argumentCount ->
      "P" <> T.unpack (unFunctionName functionName) <> "/" <> show argumentCount
    GrinThunk functionName -> "F" <> T.unpack (unFunctionName functionName)
    GrinPrimitive name arity -> "Prim[" <> T.unpack name <> "/" <> show arity <> "]"

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

renderVarAtom :: GrinVar -> String
renderVarAtom var = "(" <> renderVar var <> ")"

renderRuntimeRepArgument :: RuntimeRep -> String
renderRuntimeRepArgument runtimeRep =
  case runtimeRep of
    VecRep {} -> parenthesized
    TupleRep {} -> parenthesized
    SumRep {} -> parenthesized
    BoxedRep {} -> parenthesized
    RuntimeRepVar {} -> parenthesized
    RuntimeRepMeta {} -> parenthesized
    _ -> show runtimeRep
  where
    parenthesized = "(" <> show runtimeRep <> ")"

indent :: Int -> String
indent count = replicate count ' '
