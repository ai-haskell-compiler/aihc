-- | Human-readable GRIN rendering for diagnostics and golden tests.
module Aihc.Grin.Pretty
  ( renderProgram,
    renderExpr,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Data.ByteString qualified as BS
import Data.Char (chr, isPrint, isSpace)
import Data.List (intercalate)
import Data.Text qualified as T

renderProgram :: GrinProgram -> String
renderProgram program =
  intercalate
    "\n\n"
    ( map renderConstructor (grinConstructors program)
        <> map renderPrimitive (grinPrimitives program)
        <> map renderForeign (grinForeignCalls program)
        <> map (("external global " <>) . renderName) (grinExternalGlobals program)
        <> map renderExternalFunction (grinExternalFunctions program)
        <> map renderGlobal (grinWhnfGlobals program)
        <> map renderCaf (grinCafs program)
        <> map renderFunction (grinFunctions program)
    )

renderExternalFunction :: GrinCodeInfo -> String
renderExternalFunction info =
  "external "
    <> renderName (grinCodeSourceName info)
    <> "/"
    <> show (length (grinCodeParameterLayouts info))
    <> " "
    <> renderLayouts (grinCodeParameterLayouts info)
    <> " -> "
    <> show (grinCodeResultRep info)
    <> " = "
    <> renderFunctionName (grinCodeFunctionName info)

renderConstructor :: (T.Text, [[RuntimeRep]]) -> String
renderConstructor (name, fieldLayouts) =
  "constructor "
    <> renderName name
    <> "/"
    <> show (length fieldLayouts)
    <> " ["
    <> intercalate ", " (map renderLayout fieldLayouts)
    <> "]"
  where
    renderLayout layout =
      case layout of
        [runtimeRep] -> show runtimeRep
        _ -> "[" <> intercalate ", " (map show layout) <> "]"

renderPrimitive :: (GrinVar, Int) -> String
renderPrimitive (var, arity) =
  "primitive " <> renderVar var <> "/" <> show arity

renderForeign :: GrinForeignCall -> String
renderForeign foreignCall =
  "foreign " <> renderForeignCall foreignCall

renderGlobal :: (GrinVar, GrinNode) -> String
renderGlobal (var, node) =
  "global " <> renderVar var <> " = " <> renderNode node

renderCaf :: (GrinVar, GrinNode) -> String
renderCaf (var, node) =
  "caf " <> renderVar var <> " = " <> renderNode node

renderFunction :: GrinFunction -> String
renderFunction function =
  renderFunctionName (grinFunctionName function)
    <> concatMap ((" " <>) . renderVarAtom) (grinFunctionParameters function)
    <> " -> "
    <> show (grinFunctionResultRep function)
    <> maybe "" ((" link " <>) . renderName) (grinFunctionLinkName function)
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
    GrinEnsureHeap requiredWords roots ->
      indent indentation
        <> "ensure-heap "
        <> show requiredWords
        <> renderValues roots
    GrinStoreUnchecked node -> indent indentation <> "store-unchecked " <> renderNode node
    GrinStoreRec bindings body ->
      renderStoreRec indentation "store-rec" bindings body
    GrinStoreRecUnchecked bindings body ->
      renderStoreRec indentation "store-rec-unchecked" bindings body
    GrinFetch runtimeRep pointer ->
      indent indentation <> "fetch @" <> renderRuntimeRepArgument runtimeRep <> " " <> renderValue pointer
    GrinUpdate pointer value ->
      indent indentation <> "update " <> renderValue pointer <> " " <> renderValue value
    GrinUpdateBlackhole pointer value ->
      indent indentation <> "update-blackhole " <> renderValue pointer <> " " <> renderValue value
    GrinEval runtimeRep value ->
      indent indentation <> "eval @" <> renderRuntimeRepArgument runtimeRep <> " " <> renderValue value
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      indent indentation
        <> "cps-eval @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> unwords (map renderValue [value, continuation, updateContinuation])
    GrinCall runtimeRep functionName arguments ->
      indent indentation
        <> "call @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> renderFunctionName functionName
        <> renderValues arguments
    GrinPrimitiveCall runtimeRep name arguments ->
      indent indentation
        <> "primitive-call @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> renderName name
        <> renderValues arguments
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      indent indentation
        <> "cps-primitive-call @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> renderName name
        <> renderValues arguments
        <> " -> "
        <> renderValue continuation
    GrinApply runtimeRep function arguments ->
      indent indentation
        <> "apply @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> renderValue function
        <> renderArgument arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      indent indentation
        <> "cps-apply @"
        <> renderRuntimeRepArgument runtimeRep
        <> " "
        <> renderValue function
        <> renderArgument arguments
        <> " -> "
        <> renderValue continuation
    GrinContinue continuation values ->
      indent indentation
        <> "continue "
        <> renderValue continuation
        <> renderArgument values
    GrinHalt values -> indent indentation <> "halt" <> renderValues values
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
        <> renderForeignCall foreignCall
        <> " with"
        <> renderValues arguments

renderStoreRec :: Int -> String -> [(GrinVar, GrinNode)] -> GrinExpr -> String
renderStoreRec indentation name bindings body =
  indent indentation
    <> name
    <> "\n"
    <> intercalate
      "\n"
      [ indent (indentation + 2) <> renderVar var <> " = " <> renderNode node
      | (var, node) <- bindings
      ]
    <> "\n"
    <> renderExprIndented indentation body

renderValues :: [GrinValue] -> String
renderValues = concatMap ((" " <>) . renderValue)

renderArgument :: [GrinValue] -> String
renderArgument values =
  case values of
    [] -> " ()"
    [value] -> " " <> renderValue value
    _ -> " (" <> unwords (map renderValue values) <> ")"

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
    GrinDataAlt name -> "data " <> renderName name
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
    GrinConstructor name remaining ->
      "C" <> renderName name <> if remaining == 0 then "" else "/" <> show remaining
    GrinClosure functionName argumentLayouts ->
      "P"
        <> renderFunctionName functionName
        <> "/"
        <> if all (== [BoxedRep Lifted]) argumentLayouts
          then show (length argumentLayouts)
          else show (length argumentLayouts) <> renderLayouts argumentLayouts
    GrinThunk functionName -> "F" <> renderFunctionName functionName

renderLiteral :: GrinLiteral -> String
renderLiteral literal =
  case literal of
    GrinLitInt runtimeRep value -> "(" <> show value <> " :: " <> show runtimeRep <> ")"
    GrinLitChar runtimeRep value -> "(" <> show value <> " :: " <> show runtimeRep <> ")"
    GrinLitString value -> show (T.unpack value)
    GrinLitAddr value -> show (map (chr . fromIntegral) (BS.unpack value)) <> "#"

renderVar :: GrinVar -> String
renderVar var =
  renderName (grinVarName var)
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

renderLayouts :: [[RuntimeRep]] -> String
renderLayouts layouts = "[" <> intercalate ", " (map renderLayout layouts) <> "]"
  where
    renderLayout layout = "[" <> intercalate ", " (map show layout) <> "]"

renderForeignCall :: GrinForeignCall -> String
renderForeignCall foreignCall =
  renderName (grinForeignCallName foreignCall)
    <> " = "
    <> show (T.unpack (grinForeignCallSymbol foreignCall))
    <> " :: "
    <> renderForeignSignature (grinForeignCallSignature foreignCall)

renderForeignSignature :: GrinForeignSignature -> String
renderForeignSignature signature =
  "("
    <> intercalate ", " (map renderForeignType (grinForeignArgumentTypes signature))
    <> ") -> "
    <> renderForeignType (grinForeignResultType signature)
    <> " ! "
    <> case grinForeignEffect signature of
      GrinForeignPure -> "pure"
      GrinForeignRealWorld -> "real-world"

renderForeignType :: GrinForeignType -> String
renderForeignType foreignType =
  case foreignType of
    GrinForeignInt32 -> "int32"
    GrinForeignWord64 -> "word64"
    GrinForeignAddr -> "addr"

renderFunctionName :: FunctionName -> String
renderFunctionName = renderName . unFunctionName

renderName :: T.Text -> String
renderName name
  | not (T.null name) && T.all isBareNameCharacter name = T.unpack name
  | otherwise = show (T.unpack name)
  where
    isBareNameCharacter character =
      isPrint character
        && not (isSpace character)
        && character `notElem` ['"', '(', ')', '[', ']', ',', '=', '/', '%']

indent :: Int -> String
indent count = replicate count ' '
