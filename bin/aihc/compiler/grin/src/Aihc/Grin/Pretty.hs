-- | Human-readable GRIN rendering for diagnostics and golden tests.
module Aihc.Grin.Pretty
  ( prettyProgram,
    prettyExpr,
  )
where

import Aihc.Grin.Syntax
import Data.ByteString qualified as BS
import Data.Char (chr, isPrint, isSpace)
import Data.Text qualified as T
import Prettyprinter (Doc, comma, hardline, hsep, indent, parens, pretty, punctuate, space, vsep, (<+>))

prettyProgram :: GrinProgram -> Doc ann
prettyProgram program =
  vsep (punctuate hardline documents)
  where
    documents =
      map prettyConstructor (grinConstructors program)
        <> map prettyPrimitive (grinPrimitives program)
        <> map prettyForeign (grinForeignCalls program)
        <> map prettyGlobal (grinGlobals program)
        <> map prettyFunction (grinFunctions program)

prettyConstructor :: (T.Text, [[GrinRep]]) -> Doc ann
prettyConstructor (name, fieldLayouts) =
  "constructor" <+> prettyName name <+> "[" <> hsep (punctuate comma (map prettyLayout fieldLayouts)) <> "]"
  where
    prettyLayout layout =
      case layout of
        [runtimeRep] -> prettyShow runtimeRep
        _ -> "[" <> hsep (punctuate comma (map prettyShow layout)) <> "]"

prettyPrimitive :: (GrinVar, Int) -> Doc ann
prettyPrimitive (var, arity) =
  "primitive" <+> prettyVar var <> "/" <> pretty arity

prettyForeign :: GrinForeignCall -> Doc ann
prettyForeign foreignCall =
  "foreign" <+> prettyForeignCall foreignCall

prettyGlobal :: (T.Text, GrinNode) -> Doc ann
prettyGlobal (name, node) =
  "global" <+> prettyName name <+> "=" <+> prettyNode node

prettyFunction :: GrinFunction -> Doc ann
prettyFunction function =
  prettyFunctionName (grinFunctionName function)
    <> foldMap ((space <>) . prettyVarAtom) (grinFunctionParameters function)
    <+> "->"
    <+> prettyShow (grinFunctionResultRep function)
    <+> "="
    <> hardline
    <> indent 2 (prettyExpr (grinFunctionBody function))

prettyExpr :: GrinExpr -> Doc ann
prettyExpr expr =
  case expr of
    GrinConstant values -> "constant" <> prettyValues values
    GrinBind vars valueExpr body ->
      prettyBinders vars
        <+> "<-"
        <> hardline
        <> indent 2 (prettyExpr valueExpr)
        <> hardline
        <> prettyExpr body
    GrinStore node -> "store" <+> prettyNode node
    GrinEnsureHeap requiredWords roots ->
      "ensure-heap" <+> prettyValue requiredWords <> prettyValues roots
    GrinStoreUnchecked node -> "store-unchecked" <+> prettyNode node
    GrinStoreRec bindings body ->
      prettyStoreRec "store-rec" bindings body
    GrinStoreRecUnchecked bindings body ->
      prettyStoreRec "store-rec-unchecked" bindings body
    GrinUpdate pointer value ->
      "update" <+> prettyValue pointer <+> prettyValue value
    GrinUpdateBlackhole pointer value ->
      "update-blackhole" <+> prettyValue pointer <+> prettyValue value
    GrinEval runtimeRep value ->
      "eval" <+> "@" <> prettyRuntimeRepArgument runtimeRep <+> prettyValue value
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      "cps-eval"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> hsep (map prettyValue [value, continuation, updateContinuation])
    GrinCall runtimeRep functionName arguments ->
      "call"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyFunctionName functionName
        <> prettyValues arguments
    GrinPrimitiveCall runtimeRep name arguments ->
      "primitive-call"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyName name
        <> prettyValues arguments
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      "cps-primitive-call"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyName name
        <> prettyValues arguments
        <+> "->"
        <+> prettyValue continuation
    GrinApply runtimeRep function arguments ->
      "apply"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyValue function
        <> prettyArgument arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      "cps-apply"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> prettyValue function
        <> prettyArgument arguments
        <+> "->"
        <+> prettyValue continuation
    GrinContinue continuation values ->
      "continue" <+> prettyValue continuation <> prettyArgument values
    GrinCpsRaise exception continuation ->
      "raise-cps" <+> prettyValue exception <+> prettyValue continuation
    GrinHalt values -> "halt" <> prettyValues values
    GrinExit status -> "exit" <+> prettyValue status
    GrinCase scrutinee binder alternatives ->
      "case"
        <+> prettyValue scrutinee
        <+> "as"
        <+> prettyVar binder
        <+> "of"
        <> hardline
        <> case alternatives of
          [] -> mempty
          _ -> indent 2 (vsep (map prettyAlt alternatives))
    GrinThrow exception -> "throw" <+> prettyValue exception
    GrinCatch runtimeRep action handler state ->
      "catch"
        <+> "@"
        <> prettyRuntimeRepArgument runtimeRep
        <+> hsep (map prettyValue [action, handler])
        <> prettyValues state
    GrinForeignCallExpr foreignCall arguments ->
      "foreign-call"
        <+> prettyForeignCall foreignCall
        <+> "with"
        <> prettyValues arguments

prettyStoreRec :: Doc ann -> [(GrinVar, GrinNode)] -> GrinExpr -> Doc ann
prettyStoreRec name bindings body =
  name
    <> hardline
    <> indent 2 (vsep (map prettyBinding bindings))
    <> hardline
    <> prettyExpr body
  where
    prettyBinding (var, node) = prettyVar var <+> "=" <+> prettyNode node

prettyValues :: [GrinValue] -> Doc ann
prettyValues = foldMap ((space <>) . prettyValue)

prettyArgument :: [GrinValue] -> Doc ann
prettyArgument values =
  space
    <> case values of
      [] -> "()"
      [value] -> prettyValue value
      _ -> parens (hsep (map prettyValue values))

prettyBinders :: [GrinVar] -> Doc ann
prettyBinders vars =
  case vars of
    [] -> "()"
    _ -> hsep (punctuate comma (map prettyVar vars))

prettyAlt :: GrinAlt -> Doc ann
prettyAlt alt =
  prettyAltCon (grinAltCon alt)
    <> foldMap ((space <>) . prettyVarAtom) (grinAltBinders alt)
    <+> "->"
    <> hardline
    <> indent 2 (prettyExpr (grinAltRhs alt))

prettyAltCon :: GrinAltCon -> Doc ann
prettyAltCon altCon =
  case altCon of
    GrinDataAlt name -> "data" <+> prettyName name
    GrinLitAlt literal -> prettyLiteral literal
    GrinDefaultAlt -> "_"

prettyValue :: GrinValue -> Doc ann
prettyValue value =
  case value of
    GrinVarValue var -> prettyVarAtom var
    GrinGlobalValue name -> "global-ref" <+> prettyName name
    GrinLitValue literal -> prettyLiteral literal

prettyNode :: GrinNode -> Doc ann
prettyNode node =
  parens
    ( prettyNodeTag (grinNodeTag node)
        <> foldMap ((space <>) . prettyValue) (grinNodeFields node)
    )

prettyNodeTag :: GrinNodeTag -> Doc ann
prettyNodeTag nodeTag =
  case nodeTag of
    GrinConstructor name remaining ->
      "C" <> prettyName name <> if remaining == 0 then mempty else "/" <> pretty remaining
    GrinClosure functionName argumentLayouts ->
      "P"
        <> prettyFunctionName functionName
        <> "/"
        <> if all (== [BoxedRep Lifted]) argumentLayouts
          then pretty (length argumentLayouts)
          else prettyLayouts argumentLayouts
    GrinThunk functionName -> "F" <> prettyFunctionName functionName

prettyLiteral :: GrinLiteral -> Doc ann
prettyLiteral literal =
  case literal of
    GrinLitInt runtimeRep value -> parens (pretty value <+> "::" <+> prettyShow runtimeRep)
    GrinLitChar runtimeRep value -> parens (pretty (show value) <+> "::" <+> prettyShow runtimeRep)
    GrinLitString value -> pretty (show (T.unpack value))
    GrinLitAddr value -> pretty (show (map (chr . fromIntegral) (BS.unpack value))) <> "#"

-- | A variable's number only disambiguates same-named binders, so the common
-- case of a single binder for a name prints without one.
prettyVar :: GrinVar -> Doc ann
prettyVar var =
  prettyName (grinVarName var)
    <> prettyNumber
    <+> "::"
    <+> prettyShow (grinVarRuntimeRep var)
  where
    number = grinVarUnique var
    prettyNumber
      | number == 0 && not (grinVarNameNeedsNumber (grinVarName var)) = mempty
      | otherwise = "%" <> pretty number

prettyVarAtom :: GrinVar -> Doc ann
prettyVarAtom = parens . prettyVar

prettyRuntimeRepArgument :: GrinRep -> Doc ann
prettyRuntimeRepArgument runtimeRep =
  case runtimeRep of
    VecRep {} -> parenthesized
    TupleRep {} -> parenthesized
    SumRep {} -> parenthesized
    BoxedRep {} -> parenthesized
    _ -> prettyShow runtimeRep
  where
    parenthesized = parens (prettyShow runtimeRep)

prettyLayouts :: [[GrinRep]] -> Doc ann
prettyLayouts layouts =
  "[" <> hsep (punctuate comma (map prettyLayout layouts)) <> "]"
  where
    prettyLayout layout = "[" <> hsep (punctuate comma (map prettyShow layout)) <> "]"

prettyForeignCall :: GrinForeignCall -> Doc ann
prettyForeignCall foreignCall =
  prettyName (grinForeignCallName foreignCall)
    <+> "="
    <+> prettyForeignTarget (grinForeignCallTarget foreignCall)
    <> pretty (show (T.unpack (grinForeignCallSymbol foreignCall)))
    <+> "::"
    <+> prettyForeignSignature (grinForeignCallSignature foreignCall)

prettyForeignTarget :: GrinForeignTarget -> Doc ann
prettyForeignTarget target =
  case target of
    GrinForeignFunction -> mempty
    GrinForeignAddress -> "address "

prettyForeignSignature :: GrinForeignSignature -> Doc ann
prettyForeignSignature signature =
  parens (hsep (punctuate comma (map prettyForeignType (grinForeignArgumentTypes signature))))
    <+> "->"
    <+> prettyForeignType (grinForeignResultType signature)
    <+> "!"
    <+> case grinForeignEffect signature of
      GrinForeignPure -> "pure"
      GrinForeignRealWorld -> "real-world"

prettyForeignType :: GrinForeignType -> Doc ann
prettyForeignType foreignType =
  case foreignType of
    GrinForeignInt -> "int"
    GrinForeignInt8 -> "int8"
    GrinForeignInt16 -> "int16"
    GrinForeignInt32 -> "int32"
    GrinForeignInt64 -> "int64"
    GrinForeignWord -> "word"
    GrinForeignWord8 -> "word8"
    GrinForeignWord16 -> "word16"
    GrinForeignWord32 -> "word32"
    GrinForeignWord64 -> "word64"
    GrinForeignAddr -> "addr"
    GrinForeignVoid -> "void"

prettyFunctionName :: FunctionName -> Doc ann
prettyFunctionName = prettyName . unFunctionName

prettyName :: T.Text -> Doc ann
prettyName name
  | not (T.null name) && T.all isBareNameCharacter name = pretty name
  | otherwise = pretty (show (T.unpack name))
  where
    isBareNameCharacter character =
      isPrint character
        && not (isSpace character)
        && character `notElem` ['"', '(', ')', '[', ']', ',', '=', '/', '%']

prettyShow :: (Show value) => value -> Doc ann
prettyShow = pretty . show
