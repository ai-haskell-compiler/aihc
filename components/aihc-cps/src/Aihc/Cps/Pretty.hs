-- | Stable, human-readable rendering of Loom IR.
module Aihc.Cps.Pretty
  ( renderProgram,
  )
where

import Aihc.Cps.Syntax
import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep)
import Data.List (intercalate)
import Data.Text qualified as T

renderProgram :: LoomProgram -> String
renderProgram program =
  intercalate
    "\n\n"
    ( ["loom-ir 1"]
        <> map renderConstructor (loomConstructors program)
        <> map renderPrimitive (loomPrimitives program)
        <> map renderForeign (loomForeignCalls program)
        <> map renderGlobal (loomWhnfGlobals program)
        <> map renderCaf (loomCafs program)
        <> map renderFunction (loomFunctions program)
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

renderFunction :: LoomFunction -> String
renderFunction function =
  "code "
    <> T.unpack (unFunctionName (loomFunctionName function))
    <> concatMap ((" " <>) . renderVar) (loomFunctionParameters function)
    <> " return "
    <> renderContVar (loomFunctionReturn function)
    <> " =\n"
    <> renderTerm 2 (loomFunctionBody function)

renderTerm :: Int -> LoomTerm -> String
renderTerm indentation term =
  case term of
    LoomLetCont continuation body ->
      indent indentation
        <> "letcont "
        <> renderContVar (loomContinuationName continuation)
        <> concatMap ((" " <>) . renderVar) (loomContinuationParameters continuation)
        <> " =\n"
        <> renderTerm (indentation + 2) (loomContinuationBody continuation)
        <> "\n"
        <> indent indentation
        <> "in\n"
        <> renderTerm (indentation + 2) body
    LoomInvoke operation continuation ->
      indent indentation <> renderOperation operation <> " -> " <> renderContVar continuation
    LoomContinue continuation values ->
      indent indentation
        <> "continue "
        <> renderContVar continuation
        <> concatMap ((" " <>) . renderValue) values
    LoomCase scrutinee binder alternatives ->
      indent indentation
        <> "case "
        <> renderValue scrutinee
        <> " as "
        <> renderVar binder
        <> " of\n"
        <> intercalate "\n" (map (renderAlt (indentation + 2)) alternatives)
    LoomStoreRec bindings body ->
      indent indentation
        <> "store-rec\n"
        <> intercalate
          "\n"
          [indent (indentation + 2) <> renderVar var <> " = " <> renderNode node | (var, node) <- bindings]
        <> "\n"
        <> indent indentation
        <> "in\n"
        <> renderTerm (indentation + 2) body

renderOperation :: LoomOperation -> String
renderOperation operation =
  case operation of
    LoomStore node -> "store " <> renderNode node
    LoomFetch runtimeRep pointer -> "fetch @" <> show runtimeRep <> " " <> renderValue pointer
    LoomUpdate pointer value -> "update " <> renderValue pointer <> " " <> renderValue value
    LoomEval runtimeRep value -> "eval @" <> show runtimeRep <> " " <> renderValue value
    LoomApply runtimeRep function arguments ->
      "apply @"
        <> show runtimeRep
        <> " "
        <> renderValue function
        <> concatMap ((" " <>) . renderValue) arguments
    LoomDictSelect runtimeRep dictionary index ->
      "select @" <> show runtimeRep <> " " <> renderValue dictionary <> " " <> show index
    LoomThrow exception -> "throw " <> renderValue exception
    LoomCatch runtimeRep action handler state ->
      "catch @"
        <> show runtimeRep
        <> " "
        <> unwords (map renderValue [action, handler])
        <> concatMap ((" " <>) . renderValue) state
    LoomForeignCall foreignCall arguments ->
      "foreign-call "
        <> T.unpack (grinForeignCallName foreignCall)
        <> concatMap ((" " <>) . renderValue) arguments

renderAlt :: Int -> LoomAlt -> String
renderAlt indentation alternative =
  indent indentation
    <> renderAltCon (loomAltCon alternative)
    <> concatMap ((" " <>) . renderVar) (loomAltBinders alternative)
    <> " ->\n"
    <> renderTerm (indentation + 2) (loomAltBody alternative)

renderAltCon :: GrinAltCon -> String
renderAltCon alternative =
  case alternative of
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
    GrinClosure functionName argumentCount ->
      "P" <> T.unpack (unFunctionName functionName) <> "/" <> show argumentCount
    GrinThunk functionName -> "F" <> T.unpack (unFunctionName functionName)
    GrinPrimitive name arity -> "Prim[" <> T.unpack name <> "/" <> show arity <> "]"
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

renderContVar :: LoomContVar -> String
renderContVar continuation =
  T.unpack (loomContName continuation)
    <> "%"
    <> show (loomContUnique continuation)
    <> " :: cont["
    <> intercalate ", " (map show (loomContRuntimeReps continuation))
    <> "]"

indent :: Int -> String
indent count = replicate count ' '
