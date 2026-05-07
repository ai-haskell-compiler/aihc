{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Compat.Internal.Convert
  ( toGhcHsExpr,
    toGhcLHsExpr,
  )
where

import Aihc.Parser.Syntax qualified as A
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import GHC.Builtin.Types (nilDataCon, tupleDataCon, tupleTyCon, unrestrictedFunTyConName)
import GHC.Data.FastString (mkFastString)
import GHC.Hs
import GHC.Types.Basic
import GHC.Types.Name.Occurrence (mkDataOcc, mkVarOcc)
import GHC.Types.Name.Reader (RdrName, getRdrName, mkRdrQual, mkRdrUnqual)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), noSrcSpan, unLoc)
import Language.Haskell.Syntax.Basic (FieldLabelString (..), LexicalFixity (..))
import Language.Haskell.Syntax.Specificity (Specificity (..))

toGhcHsExpr :: A.Expr -> HsExpr GhcPs
toGhcHsExpr expr =
  case A.peelExprAnn expr of
    A.EAnn _ inner -> toGhcHsExpr inner
    A.EVar name -> HsVar noExtField (lA (toRdrName name))
    A.ETypeSyntax _ ty -> HsEmbTy noAnn (toWcType ty)
    A.EInt value numeric raw -> integerExpr value numeric raw
    A.EFloat value floatTy raw -> floatExpr value floatTy raw
    A.EChar value _ -> HsLit noExtField (HsChar NoSourceText value)
    A.ECharHash value _ -> HsLit noExtField (HsCharPrim NoSourceText value)
    A.EString value _ -> HsLit noExtField (HsString NoSourceText (mkFastString (T.unpack value)))
    A.EStringHash value _ -> HsLit noExtField (HsStringPrim NoSourceText (BS.pack (T.unpack value)))
    A.EOverloadedLabel value _ -> HsOverLabel NoSourceText (mkFastString (T.unpack value))
    A.EQuasiQuote quoter body -> HsUntypedSplice noExtField (HsQuasiQuote noExtField (lA (toRdrName (A.nameFromText quoter))) (lA (mkFastString (T.unpack body))))
    A.EIf cond yes no -> HsIf noAnn (toGhcLHsExpr cond) (toGhcLHsExpr yes) (toGhcLHsExpr no)
    A.EMultiWayIf rhss -> HsMultiIf noAnn (nonEmpty (map toGRHS rhss))
    A.ELambdaPats pats body -> HsLam noAnn LamSingle (matchGroup (LamAlt LamSingle) [match (LamAlt LamSingle) pats (A.UnguardedRhs [] body Nothing)])
    A.ELambdaCase alts -> HsLam noAnn LamCase (matchGroup (LamAlt LamCase) (map (caseAltMatchWith (LamAlt LamCase)) alts))
    A.ELambdaCases alts -> HsLam noAnn LamCases (matchGroup (LamAlt LamCases) (map (lambdaCasesAltMatchWith (LamAlt LamCases)) alts))
    A.EInfix lhs op rhs -> OpApp noExtField (toGhcLHsExpr lhs) (operatorExpr op) (toGhcLHsExpr rhs)
    A.ENegate inner -> NegApp noAnn (toGhcLHsExpr inner) noSyntaxExpr
    A.ESectionL lhs op -> SectionL noExtField (toGhcLHsExpr lhs) (operatorExpr op)
    A.ESectionR op rhs -> SectionR noExtField (operatorExpr op) (toGhcLHsExpr rhs)
    A.ELetDecls decls body -> HsLet noAnn (localBinds decls) (toGhcLHsExpr body)
    A.ECase scrut alts -> HsCase noAnn (toGhcLHsExpr scrut) (matchGroup CaseAlt (map caseAltMatch alts))
    A.EDo stmts flavor -> HsDo noAnn (doFlavor flavor) (lA (doStmts stmts))
    A.EListComp body stmts -> HsDo noAnn ListComp (lA (compStmts body stmts))
    A.EListCompParallel body stmtss -> HsDo noAnn ListComp (lA (parCompStmts body stmtss))
    A.EArithSeq seq' -> ArithSeq noAnn Nothing (arithSeqInfo seq')
    A.ERecordCon con fields wildcard -> RecordCon recordAnn (lA (toRdrName con)) (recordBinds fields wildcard)
    A.ERecordUpd target fields | Just con <- recordConTarget target -> RecordCon recordAnn (lA con) (recordBinds fields False)
    A.ERecordUpd target fields -> RecordUpd recordAnn (toGhcLHsExpr target) (RegularRecUpdFields noExtField (map recordUpdField fields))
    A.EGetField base field -> HsGetField noExtField (toGhcLHsExpr base) (lA (dotField field))
    A.EGetFieldProjection fields -> HsProjection noAnn (nonEmpty (map dotField fields))
    A.ETypeSig inner ty -> ExprWithTySig noAnn (toGhcLHsExpr inner) (toSigWcType ty)
    A.EParen inner
      | A.EGetFieldProjection {} <- A.peelExprAnn inner -> toGhcHsExpr inner
      | otherwise -> HsPar parAnn (toGhcLHsExpr inner)
    A.EList [] -> HsVar noExtField (lA (getRdrName nilDataCon))
    A.EList elems -> ExplicitList noAnn (map toGhcLHsExpr elems)
    A.ETuple flavor elems -> tupleExpr flavor elems
    A.EUnboxedSum altIdx arity inner -> ExplicitSum noAnn (altIdx + 1) arity (toGhcLHsExpr inner)
    A.ETypeApp inner ty -> HsAppType noAnn (toGhcLHsExpr inner) (toWcType ty)
    A.EApp fun arg -> HsApp noExtField (toGhcLHsExpr fun) (toGhcLHsExpr arg)
    A.ETHExpQuote body -> HsUntypedBracket noExtField (ExpBr noAnn (toGhcLHsExpr body))
    A.ETHTypedQuote body -> HsTypedBracket noAnn (toGhcLHsExpr body)
    A.ETHDeclQuote decls -> HsUntypedBracket noExtField (DecBrL noAnn (map toLHsDecl decls))
    A.ETHTypeQuote ty -> HsUntypedBracket noExtField (TypBr noAnn (toLHsType ty))
    A.ETHPatQuote pat -> HsUntypedBracket noExtField (PatBr noAnn (toLPat pat))
    A.ETHNameQuote quoted -> HsUntypedBracket noExtField (VarBr noAnn True (quotedExprName quoted))
    A.ETHTypeNameQuote ty -> HsUntypedBracket noExtField (VarBr noAnn False (quotedTypeName ty))
    A.ETHSplice body -> HsUntypedSplice noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr body))
    A.ETHTypedSplice body -> HsTypedSplice noExtField (HsTypedSpliceExpr noAnn (toGhcLHsExpr body))
    A.EProc pat cmd -> HsProc noAnn (toLPat pat) (lA (HsCmdTop noExtField (toLHsCmd cmd)))
    A.EPragma _ inner -> toGhcHsExpr inner

toGhcLHsExpr :: A.Expr -> LHsExpr GhcPs
toGhcLHsExpr = lA . toGhcHsExpr

integerExpr :: Integer -> A.NumericType -> T.Text -> HsExpr GhcPs
integerExpr value numeric raw =
  case numeric of
    A.TInteger -> HsOverLit noExtField (mkHsIntegral (IL (sourceText raw) False value))
    A.TIntHash -> HsLit noExtField (HsIntPrim NoSourceText value)
    A.TWordHash -> HsLit noExtField (HsWordPrim NoSourceText value)
    A.TInt8Hash -> HsLit noExtField (HsInt8Prim NoSourceText value)
    A.TInt16Hash -> HsLit noExtField (HsInt16Prim NoSourceText value)
    A.TInt32Hash -> HsLit noExtField (HsInt32Prim NoSourceText value)
    A.TInt64Hash -> HsLit noExtField (HsInt64Prim NoSourceText value)
    A.TWord8Hash -> HsLit noExtField (HsWord8Prim NoSourceText value)
    A.TWord16Hash -> HsLit noExtField (HsWord16Prim NoSourceText value)
    A.TWord32Hash -> HsLit noExtField (HsWord32Prim NoSourceText value)
    A.TWord64Hash -> HsLit noExtField (HsWord64Prim NoSourceText value)

floatExpr :: Rational -> A.FloatType -> T.Text -> HsExpr GhcPs
floatExpr value floatTy raw =
  let lit = fractionalLit value raw
   in case floatTy of
        A.TFractional -> HsOverLit noExtField (mkHsFractional lit)
        A.TFloatHash -> HsLit noExtField (HsFloatPrim noExtField lit)
        A.TDoubleHash -> HsLit noExtField (HsDoublePrim noExtField lit)

fractionalLit :: Rational -> T.Text -> FractionalLit
fractionalLit value raw =
  FL (sourceText raw) False significand' exponent' Base10
  where
    exponent' = fractionalExponent raw
    significand' = value / (10 ^^ exponent')

fractionalExponent :: T.Text -> Integer
fractionalExponent raw =
  explicitExponent - fromIntegral fractionalDigits
  where
    token = filter (/= '_') (T.unpack raw)
    tokenSignificand = takeWhile (\c -> c /= 'e' && c /= 'E' && c /= '#') token
    exponentText =
      case dropWhile (\c -> c /= 'e' && c /= 'E') token of
        [] -> ""
        (_ : rest) -> takeWhile exponentChar rest
    explicitExponent =
      case reads exponentText of
        [(value, "")] -> value
        _ -> 0
    fractionalDigits =
      case dropWhile (/= '.') tokenSignificand of
        [] -> 0
        (_ : rest) -> length (takeWhile isDigit rest)
    exponentChar c = c == '+' || c == '-' || isDigit c

tupleExpr :: A.TupleFlavor -> [Maybe A.Expr] -> HsExpr GhcPs
tupleExpr flavor elems =
  case elems of
    [] -> HsVar noExtField (lA (getRdrName (tupleDataCon (boxity flavor) 0)))
    _
      | all nullary elems -> HsVar noExtField (lA (getRdrName (tupleDataCon (boxity flavor) (length elems))))
      | otherwise -> ExplicitTuple tupleAnn (map tupArg elems) (boxity flavor)

tupArg :: Maybe A.Expr -> HsTupArg GhcPs
tupArg =
  maybe (Missing noAnn) (Present noExtField . toGhcLHsExpr)

nullary :: Maybe a -> Bool
nullary Nothing = True
nullary Just {} = False

operatorExpr :: A.Name -> LHsExpr GhcPs
operatorExpr = lA . HsVar noExtField . lA . toRdrName

recordBinds :: [A.RecordField A.Expr] -> Bool -> HsRecordBinds GhcPs
recordBinds fields wildcard =
  HsRecFields noExtField (map recordField fields) (if wildcard then Just (lA (RecFieldsDotDot 0)) else Nothing)

recordField :: A.RecordField A.Expr -> LHsRecField GhcPs (LHsExpr GhcPs)
recordField (A.RecordField name value pun) =
  lA (HsFieldBind (Just noAnn) (lA (fieldOcc name)) (toGhcLHsExpr value) pun)

recordUpdField :: A.RecordField A.Expr -> LHsRecUpdField GhcPs GhcPs
recordUpdField (A.RecordField name value pun) =
  lA (HsFieldBind (Just noAnn) (lA (fieldOcc name)) (toGhcLHsExpr value) pun)

recordConTarget :: A.Expr -> Maybe RdrName
recordConTarget expr =
  case A.peelExprAnn expr of
    A.EAnn _ inner -> recordConTarget inner
    A.EVar name
      | isConName name -> Just (toRdrName name)
      | otherwise -> Nothing
    A.EList [] -> Just (getRdrName nilDataCon)
    A.ETuple flavor [] -> Just (getRdrName (tupleDataCon (boxity flavor) 0))
    A.ETuple flavor elems | all nullary elems -> Just (getRdrName (tupleDataCon (boxity flavor) (length elems)))
    _ -> Nothing

isConName :: A.Name -> Bool
isConName name =
  case A.nameType name of
    A.NameConId -> True
    A.NameConSym -> True
    _ -> False

fieldOcc :: A.Name -> FieldOcc GhcPs
fieldOcc name = FieldOcc noExtField (lA (toRdrName name))

dotField :: A.Name -> DotFieldOcc GhcPs
dotField name = DotFieldOcc noAnn (lA (FieldLabelString (mkFastString (T.unpack (A.nameText name)))))

doFlavor :: A.DoFlavor -> HsDoFlavour
doFlavor flavor =
  case flavor of
    A.DoPlain -> DoExpr Nothing
    A.DoMdo -> MDoExpr Nothing
    A.DoQualified modName -> DoExpr (Just (mkModuleName (T.unpack modName)))
    A.DoQualifiedMdo modName -> MDoExpr (Just (mkModuleName (T.unpack modName)))

doStmts :: [A.DoStmt A.Expr] -> [ExprLStmt GhcPs]
doStmts [] = []
doStmts [A.DoExpr expr] = [lA (LastStmt noExtField (toGhcLHsExpr expr) Nothing noSyntaxExpr)]
doStmts (stmt : rest) = toDoStmt stmt : doStmts rest

toDoStmt :: A.DoStmt A.Expr -> ExprLStmt GhcPs
toDoStmt stmt =
  lA $
    case A.peelDoStmtAnn stmt of
      A.DoAnn _ inner -> unLoc (toDoStmt inner)
      A.DoBind pat body -> BindStmt noAnn (toLPat pat) (toGhcLHsExpr body)
      A.DoLetDecls decls -> LetStmt noAnn (localBinds decls)
      A.DoExpr body -> BodyStmt noExtField (toGhcLHsExpr body) noSyntaxExpr noSyntaxExpr
      A.DoRecStmt stmts ->
        RecStmt
          noAnn
          (lA (map toDoStmt stmts))
          []
          []
          noSyntaxExpr
          noSyntaxExpr
          noSyntaxExpr

compStmts :: A.Expr -> [A.CompStmt] -> [ExprLStmt GhcPs]
compStmts body stmts = compQualStmts stmts <> [lA (LastStmt noExtField (toGhcLHsExpr body) Nothing noSyntaxExpr)]

parCompStmts :: A.Expr -> [[A.CompStmt]] -> [ExprLStmt GhcPs]
parCompStmts body branches =
  [ lA $
      ParStmt
        noExtField
        (nonEmpty (map parBlock branches))
        parsedNoExpr
        noSyntaxExpr,
    lA (LastStmt noExtField (toGhcLHsExpr body) Nothing noSyntaxExpr)
  ]

parBlock :: [A.CompStmt] -> ParStmtBlock GhcPs GhcPs
parBlock stmts = ParStmtBlock noExtField (compQualStmts stmts) [] noSyntaxExpr

parsedNoExpr :: HsExpr GhcPs
parsedNoExpr = HsLit noExtField (HsString (SourceText "noExpr") (mkFastString "noExpr"))

compQualStmts :: [A.CompStmt] -> [ExprLStmt GhcPs]
compQualStmts = foldl step []
  where
    step acc stmt =
      case toCompStmt acc stmt of
        (converted, True) -> [converted]
        (converted, False) -> acc <> [converted]

toCompStmt :: [ExprLStmt GhcPs] -> A.CompStmt -> (ExprLStmt GhcPs, Bool)
toCompStmt previous stmt =
  let transformed stmt' = (lA stmt', True)
      plain stmt' = (lA stmt', False)
   in case A.peelCompStmtAnn stmt of
        A.CompAnn _ inner -> toCompStmt previous inner
        A.CompGen pat expr -> plain (BindStmt noAnn (toLPat pat) (toGhcLHsExpr expr))
        A.CompGuard expr -> plain (BodyStmt noExtField (toGhcLHsExpr expr) noSyntaxExpr noSyntaxExpr)
        A.CompLetDecls decls -> plain (LetStmt noAnn (localBinds decls))
        A.CompThen expr -> transformed (transStmt ThenForm previous expr Nothing)
        A.CompThenBy usingExpr byExpr -> transformed (transStmt ThenForm previous usingExpr (Just byExpr))
        A.CompGroupUsing expr -> transformed (transStmt GroupForm previous expr Nothing)
        A.CompGroupByUsing byExpr usingExpr -> transformed (transStmt GroupForm previous usingExpr (Just byExpr))

transStmt :: TransForm -> [ExprLStmt GhcPs] -> A.Expr -> Maybe A.Expr -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
transStmt form previous usingExpr byExpr =
  TransStmt noAnn form previous [] (toGhcLHsExpr usingExpr) (toGhcLHsExpr <$> byExpr) noSyntaxExpr noSyntaxExpr parsedNoExpr

arithSeqInfo :: A.ArithSeq -> ArithSeqInfo GhcPs
arithSeqInfo seq' =
  case A.peelArithSeqAnn seq' of
    A.ArithSeqAnn _ inner -> arithSeqInfo inner
    A.ArithSeqFrom from -> From (toGhcLHsExpr from)
    A.ArithSeqFromThen from next -> FromThen (toGhcLHsExpr from) (toGhcLHsExpr next)
    A.ArithSeqFromTo from to -> FromTo (toGhcLHsExpr from) (toGhcLHsExpr to)
    A.ArithSeqFromThenTo from next to -> FromThenTo (toGhcLHsExpr from) (toGhcLHsExpr next) (toGhcLHsExpr to)

toLPat :: A.Pattern -> LPat GhcPs
toLPat = lA . toPat

toPat :: A.Pattern -> Pat GhcPs
toPat pat =
  case A.peelPatternAnn pat of
    A.PAnn _ inner -> toPat inner
    A.PVar name -> VarPat noExtField (lA (toRdrName (A.qualifyName Nothing name)))
    A.PTypeBinder _ -> WildPat noExtField
    A.PTypeSyntax _ ty -> EmbTyPat noAnn (HsTP noExtField (toLHsType ty))
    A.PWildcard -> WildPat noExtField
    A.PLit lit -> litPat lit
    A.PQuasiQuote quoter body -> SplicePat noExtField (HsQuasiQuote noExtField (lA (toRdrName (A.nameFromText quoter))) (lA (mkFastString (T.unpack body))))
    A.PTuple flavor [] -> ConPat conPatAnn (lA (getRdrName (tupleDataCon (boxity flavor) 0))) (PrefixCon [])
    A.PTuple flavor pats -> TuplePat tupleAnn (map toLPat pats) (boxity flavor)
    A.PUnboxedSum altIdx arity inner -> SumPat noAnn (toLPat inner) (altIdx + 1) arity
    A.PList [] -> ConPat conPatAnn (lA (getRdrName nilDataCon)) (PrefixCon [])
    A.PList pats -> ListPat noAnn (map toLPat pats)
    A.PCon con [] pats -> ConPat conPatAnn (lA (toRdrName con)) (PrefixCon (map toLPat pats))
    A.PCon con _ pats -> ConPat conPatAnn (lA (toRdrName con)) (PrefixCon (map toLPat pats))
    A.PInfix lhs op rhs -> ConPat conPatAnn (lA (toRdrName op)) (InfixCon (toLPat lhs) (toLPat rhs))
    A.PView expr inner -> ViewPat noAnn (toGhcLHsExpr expr) (toLPat inner)
    A.PAs name inner -> AsPat noAnn (lA (toRdrName (A.qualifyName Nothing name))) (toLPat inner)
    A.PStrict inner -> BangPat noAnn (toLPat inner)
    A.PIrrefutable inner -> LazyPat noAnn (toLPat inner)
    A.PNegLit lit -> negLitPat lit
    A.PParen inner -> ParPat parAnn (toLPat inner)
    A.PRecord con fields wildcard ->
      ConPat conPatAnn (lA (toRdrName con)) (RecCon (HsRecFields noExtField (map patRecordField fields) (if wildcard then Just (lA (RecFieldsDotDot 0)) else Nothing)))
    A.PTypeSig inner ty -> SigPat noAnn (toLPat inner) (HsPS noAnn (toLHsType ty))
    A.PSplice expr -> SplicePat noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr expr))

litPat :: A.Literal -> Pat GhcPs
litPat lit =
  case A.peelLiteralAnn lit of
    A.LitAnn _ inner -> litPat inner
    A.LitInt value numeric raw -> numLitPat value numeric raw
    A.LitFloat value floatTy raw -> floatLitPat value floatTy raw
    A.LitChar value _ -> LitPat noExtField (HsChar NoSourceText value)
    A.LitCharHash value _ -> LitPat noExtField (HsCharPrim NoSourceText value)
    A.LitString value _ -> LitPat noExtField (HsString NoSourceText (mkFastString (T.unpack value)))
    A.LitStringHash value _ -> LitPat noExtField (HsStringPrim NoSourceText (BS.pack (T.unpack value)))

negLitPat :: A.Literal -> Pat GhcPs
negLitPat lit =
  case A.peelLiteralAnn lit of
    A.LitInt value A.TInteger raw -> NPat noAnn (lA (mkHsIntegral (IL (sourceText raw) False value))) (Just noSyntaxExpr) noSyntaxExpr
    A.LitFloat value A.TFractional raw -> NPat noAnn (lA (mkHsFractional (fractionalLit value raw))) (Just noSyntaxExpr) noSyntaxExpr
    other -> litPat other

numLitPat :: Integer -> A.NumericType -> T.Text -> Pat GhcPs
numLitPat value numeric raw =
  case numeric of
    A.TInteger -> NPat noAnn (lA (mkHsIntegral (IL (sourceText raw) False value))) Nothing noSyntaxExpr
    _ -> case integerExpr value numeric raw of
      HsLit _ lit -> LitPat noExtField lit
      _ -> NPat noAnn (lA (mkHsIntegral (mkIntegralLit value))) Nothing noSyntaxExpr

floatLitPat :: Rational -> A.FloatType -> T.Text -> Pat GhcPs
floatLitPat value floatTy raw =
  case floatExpr value floatTy raw of
    HsLit _ lit -> LitPat noExtField lit
    _ -> NPat noAnn (lA (mkHsFractional (fractionalLit value raw))) Nothing noSyntaxExpr

patRecordField :: A.RecordField A.Pattern -> LHsRecField GhcPs (LPat GhcPs)
patRecordField (A.RecordField name value pun) =
  lA (HsFieldBind (Just noAnn) (lA (fieldOcc name)) (toLPat value) pun)

toLHsType :: A.Type -> LHsType GhcPs
toLHsType = lA . toHsType

toHsType :: A.Type -> HsType GhcPs
toHsType ty =
  case A.peelTypeAnn ty of
    A.TAnn _ inner -> toHsType inner
    A.TVar name -> HsTyVar noAnn NotPromoted (lA (toRdrName (A.qualifyName Nothing name)))
    A.TCon name promotion -> HsTyVar noAnn (promotionFlag promotion) (lA (toRdrName name))
    A.TBuiltinCon builtin -> builtinType builtin
    A.TImplicitParam name inner -> HsIParamTy noAnn (lA (HsIPName (mkFastString (T.unpack (T.dropWhile (== '?') name))))) (toLHsType inner)
    A.TTypeLit lit -> HsTyLit noExtField (typeLit lit)
    A.TStar -> HsStarTy noExtField False
    A.TQuasiQuote quoter body -> HsSpliceTy noExtField (HsQuasiQuote noExtField (lA (toRdrName (A.nameFromText quoter))) (lA (mkFastString (T.unpack body))))
    A.TForall telescope body -> HsForAllTy noExtField (forallTele telescope) (toLHsType body)
    A.TApp fun arg -> HsAppTy noExtField (toLHsType fun) (toLHsType arg)
    A.TTypeApp fun arg -> HsAppKindTy noAnn (toLHsType fun) (toLHsType arg)
    A.TInfix lhs op promotion rhs -> HsOpTy noExtField (promotionFlag promotion) (toLHsType lhs) (lA (toRdrName op)) (toLHsType rhs)
    A.TFun arrow lhs rhs -> HsFunTy noExtField (multAnn arrow) (toLHsType lhs) (toLHsType rhs)
    A.TTuple flavor promotion elems ->
      case promotion of
        A.Promoted -> HsExplicitTupleTy (noAnn, noAnn, noAnn) IsPromoted (map toLHsType elems)
        A.Unpromoted -> HsTupleTy noAnn (tupleSort flavor) (map toLHsType elems)
    A.TUnboxedSum elems -> HsSumTy noAnn (map toLHsType elems)
    A.TList promotion elems ->
      case promotion of
        A.Promoted -> HsExplicitListTy (noAnn, noAnn, noAnn) IsPromoted (map toLHsType elems)
        A.Unpromoted | [elemTy] <- elems -> HsListTy noAnn (toLHsType elemTy)
        A.Unpromoted -> HsExplicitListTy (noAnn, noAnn, noAnn) NotPromoted (map toLHsType elems)
    A.TParen inner -> HsParTy parAnn (toLHsType inner)
    A.TKindSig inner kind -> HsKindSig noAnn (toLHsType inner) (toLHsType kind)
    A.TContext context body -> HsQualTy noExtField (lA (map toLHsType context)) (toLHsType body)
    A.TSplice expr -> HsSpliceTy noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr expr))
    A.TWildcard -> HsWildCardTy noAnn

builtinType :: A.TypeBuiltinCon -> HsType GhcPs
builtinType builtin =
  case builtin of
    A.TBuiltinTuple arity -> HsTyVar noAnn NotPromoted (lA (getRdrName (tupleDataCon Boxed arity)))
    A.TBuiltinArrow -> HsTyVar noAnn NotPromoted (lA (getRdrName unrestrictedFunTyConName))
    A.TBuiltinList -> HsTyVar noAnn NotPromoted (lA (getRdrName nilDataCon))
    A.TBuiltinCons -> HsTyVar noAnn NotPromoted (lA (mkRdrUnqual (mkDataOcc ":")))

typeLit :: A.TypeLiteral -> HsTyLit GhcPs
typeLit lit =
  case lit of
    A.TypeLitInteger value _ -> HsNumTy NoSourceText value
    A.TypeLitSymbol value _ -> HsStrTy NoSourceText (mkFastString (T.unpack value))
    A.TypeLitChar value _ -> HsCharTy NoSourceText value

toWcType :: A.Type -> LHsWcType GhcPs
toWcType ty = HsWC noExtField (toLHsType ty)

toSigWcType :: A.Type -> LHsSigWcType GhcPs
toSigWcType ty =
  case A.peelTypeAnn ty of
    A.TAnn _ inner -> toSigWcType inner
    A.TForall (A.ForallTelescope A.ForallInvisible binders) body ->
      HsWC noExtField (lA (HsSig noExtField (HsOuterExplicit noAnn (map toInvisibleTyVarBndr binders)) (toLHsType body)))
    _ ->
      HsWC noExtField (lA (HsSig noExtField (HsOuterImplicit noExtField) (toLHsType ty)))

forallTele :: A.ForallTelescope -> HsForAllTelescope GhcPs
forallTele (A.ForallTelescope visibility binders) =
  case visibility of
    A.ForallVisible -> HsForAllVis noAnn (map (toTyVarBndr ()) binders)
    A.ForallInvisible -> HsForAllInvis noAnn (map toInvisibleTyVarBndr binders)

toInvisibleTyVarBndr :: A.TyVarBinder -> LHsTyVarBndr Specificity GhcPs
toInvisibleTyVarBndr binder =
  toTyVarBndr specificity binder
  where
    specificity =
      case A.tyVarBinderSpecificity binder of
        A.TyVarBInferred -> InferredSpec
        A.TyVarBSpecified -> SpecifiedSpec

toTyVarBndr :: flag -> A.TyVarBinder -> LHsTyVarBndr flag GhcPs
toTyVarBndr flag binder =
  lA (HsTvb noAnn flag (HsBndrVar noExtField (lA (mkRdrUnqual (mkVarOcc (T.unpack (A.tyVarBinderName binder)))))) kind)
  where
    kind = maybe (HsBndrNoKind noExtField) (HsBndrKind noExtField . toLHsType) (A.tyVarBinderKind binder)

multAnn :: A.ArrowKind -> HsMultAnn GhcPs
multAnn arrow =
  case arrow of
    A.ArrowUnrestricted -> HsUnannotated EpPatBind
    A.ArrowLinear -> HsLinearAnn noAnn
    A.ArrowExplicit ty -> HsExplicitMult (noAnn, EpPatBind) (toLHsType ty)

localBinds :: [A.Decl] -> HsLocalBinds GhcPs
localBinds decls =
  let (binds, sigs) = foldMap declBindSig decls
   in if null binds && null sigs
        then EmptyLocalBinds noExtField
        else HsValBinds noAnn (ValBinds NoAnnSortKey binds sigs)

whereBinds :: Maybe [A.Decl] -> HsLocalBinds GhcPs
whereBinds Nothing = EmptyLocalBinds noExtField
whereBinds (Just decls) =
  let (binds, sigs) = foldMap declBindSig decls
   in HsValBinds noAnn (ValBinds NoAnnSortKey binds sigs)

declBindSig :: A.Decl -> ([LHsBind GhcPs], [LSig GhcPs])
declBindSig decl =
  case A.peelDeclAnn decl of
    A.DeclAnn _ inner -> declBindSig inner
    A.DeclValue value -> ([lA (valueBind value)], [])
    A.DeclTypeSig names ty -> ([], [lA (TypeSig noAnn (map (lA . toRdrName . A.qualifyName Nothing) names) (toSigWcType ty))])
    A.DeclSplice expr -> ([lA (PatBind noExtField (lA (SplicePat noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr expr)))) (HsUnannotated EpPatBind) (grhss (A.UnguardedRhs [] (A.EVar "undefined") Nothing)))], [])
    _ -> ([], [])

valueBind :: A.ValueDecl -> HsBind GhcPs
valueBind value =
  case value of
    A.FunctionBind name matches ->
      FunBind noExtField (lA funName) (matchGroup (FunRhs (lA funName) Prefix NoSrcStrict noAnn) (map (functionMatch name) matches))
      where
        funName = toRdrName (A.qualifyName Nothing name)
    A.PatternBind A.NoMultiplicityTag (A.PVar name) rhs ->
      FunBind noExtField (lA funName) (matchGroup (FunRhs (lA funName) Prefix NoSrcStrict noAnn) [match (FunRhs (lA funName) Prefix NoSrcStrict noAnn) [] rhs])
      where
        funName = toRdrName (A.qualifyName Nothing name)
    A.PatternBind A.NoMultiplicityTag pat rhs
      | Just name <- strictVarPatName pat ->
          let funName = toRdrName (A.qualifyName Nothing name)
           in FunBind noExtField (lA funName) (matchGroup (FunRhs (lA funName) Prefix SrcStrict noAnn) [match (FunRhs (lA funName) Prefix SrcStrict noAnn) [] rhs])
    A.PatternBind multiplicity pat rhs ->
      PatBind noExtField (toLPat pat) (patMult multiplicity) (grhss rhs)

strictVarPatName :: A.Pattern -> Maybe A.UnqualifiedName
strictVarPatName pat =
  case A.peelPatternAnn pat of
    A.PAnn _ inner -> strictVarPatName inner
    A.PStrict inner ->
      case A.peelPatternAnn inner of
        A.PAnn _ inner' -> strictVarPatName (A.PStrict inner')
        A.PVar name -> Just name
        _ -> Nothing
    _ -> Nothing

functionMatch :: A.BinderName -> A.Match -> Match GhcPs (LHsExpr GhcPs)
functionMatch name (A.Match _ headForm pats rhs) =
  Match noExtField FunRhs {mc_fun = lA (toRdrName (A.qualifyName Nothing name)), mc_fixity = fixity, mc_strictness = NoSrcStrict, mc_an = noAnn} (lA (map toLPat pats)) (grhss rhs)
  where
    fixity =
      case headForm of
        A.MatchHeadPrefix -> Prefix
        A.MatchHeadInfix -> Infix

caseAltMatch :: A.CaseAlt A.Expr -> Match GhcPs (LHsExpr GhcPs)
caseAltMatch = caseAltMatchWith CaseAlt

caseAltMatchWith :: HsMatchContext (LIdP GhcPs) -> A.CaseAlt A.Expr -> Match GhcPs (LHsExpr GhcPs)
caseAltMatchWith context (A.CaseAlt _ pat rhs) = match context [pat] rhs

lambdaCasesAltMatchWith :: HsMatchContext (LIdP GhcPs) -> A.LambdaCaseAlt -> Match GhcPs (LHsExpr GhcPs)
lambdaCasesAltMatchWith context (A.LambdaCaseAlt _ pats rhs) = match context pats rhs

match :: HsMatchContext (LIdP GhcPs) -> [A.Pattern] -> A.Rhs A.Expr -> Match GhcPs (LHsExpr GhcPs)
match context pats rhs =
  Match noExtField context (lA (map toLPat pats)) (grhss rhs)

matchGroup :: HsMatchContext (LIdP GhcPs) -> [Match GhcPs (LHsExpr GhcPs)] -> MatchGroup GhcPs (LHsExpr GhcPs)
matchGroup _ matches =
  MG FromSource (lA (map lA matches))

grhss :: A.Rhs A.Expr -> GRHSs GhcPs (LHsExpr GhcPs)
grhss rhs =
  case rhs of
    A.UnguardedRhs _ body whereDecls ->
      GRHSs emptyComments (lA (GRHS noAnn [] (toGhcLHsExpr body)) :| []) (whereBinds whereDecls)
    A.GuardedRhss _ rhss whereDecls ->
      GRHSs emptyComments (nonEmpty (map toGRHS rhss)) (whereBinds whereDecls)

toGRHS :: A.GuardedRhs A.Expr -> LGRHS GhcPs (LHsExpr GhcPs)
toGRHS (A.GuardedRhs _ guards body) =
  lA (GRHS noAnn (map guardStmt guards) (toGhcLHsExpr body))

guardStmt :: A.GuardQualifier -> GuardLStmt GhcPs
guardStmt guard =
  lA $
    case A.peelGuardQualifierAnn guard of
      A.GuardAnn _ inner -> unLoc (guardStmt inner)
      A.GuardExpr expr -> BodyStmt noExtField (toGhcLHsExpr expr) noSyntaxExpr noSyntaxExpr
      A.GuardPat pat expr -> BindStmt noAnn (toLPat pat) (toGhcLHsExpr expr)
      A.GuardLet decls -> LetStmt noAnn (localBinds decls)

patMult :: A.MultiplicityTag -> HsMultAnn GhcPs
patMult tag =
  case tag of
    A.NoMultiplicityTag -> HsUnannotated EpPatBind
    A.LinearMultiplicityTag -> HsLinearAnn noAnn
    A.ExplicitMultiplicityTag ty -> HsExplicitMult (noAnn, EpPatBind) (toLHsType ty)

toLHsDecl :: A.Decl -> LHsDecl GhcPs
toLHsDecl decl =
  case declBindSig decl of
    (bind : _, _) -> lA (ValD noExtField (unLoc bind))
    (_, sig : _) -> lA (SigD noExtField (unLoc sig))
    _ -> lA (SpliceD noExtField (SpliceDecl noExtField (lA (HsUntypedSpliceExpr noAnn (toGhcLHsExpr (A.EVar "unsupportedDecl")))) DollarSplice))

toLHsCmd :: A.Cmd -> LHsCmd GhcPs
toLHsCmd = lA . toHsCmd

toHsCmd :: A.Cmd -> HsCmd GhcPs
toHsCmd cmd =
  case A.peelCmdAnn cmd of
    A.CmdAnn _ inner -> toHsCmd inner
    A.CmdArrApp lhs appTy rhs -> HsCmdArrApp (NormalSyntax, EpaSpan noSrcSpan) (toGhcLHsExpr lhs) (toGhcLHsExpr rhs) (arrAppType appTy) True
    A.CmdInfix lhs op rhs -> HsCmdArrForm noAnn (operatorExpr op) Infix [cmdTop lhs, cmdTop rhs]
    A.CmdDo stmts -> HsCmdDo noAnn (lA (cmdDoStmts stmts))
    A.CmdIf cond yes no -> HsCmdIf noAnn noSyntaxExpr (toGhcLHsExpr cond) (toLHsCmd yes) (toLHsCmd no)
    A.CmdCase scrut alts -> HsCmdCase noAnn (toGhcLHsExpr scrut) (cmdMatchGroup CaseAlt (map cmdCaseAltMatch alts))
    A.CmdLet decls body -> HsCmdLet noAnn (localBinds decls) (toLHsCmd body)
    A.CmdLam pats body -> HsCmdLam noAnn LamSingle (cmdMatchGroup (LamAlt LamSingle) [cmdMatch (LamAlt LamSingle) pats (A.UnguardedRhs [] body Nothing)])
    A.CmdApp fun arg -> HsCmdApp noExtField (toLHsCmd fun) (toGhcLHsExpr arg)
    A.CmdPar inner -> HsCmdPar parAnn (toLHsCmd inner)

cmdTop :: A.Cmd -> LHsCmdTop GhcPs
cmdTop cmd = lA (HsCmdTop noExtField (toLHsCmd cmd))

arrAppType :: A.ArrAppType -> HsArrAppType
arrAppType appTy =
  case appTy of
    A.HsFirstOrderApp -> HsFirstOrderApp
    A.HsHigherOrderApp -> HsHigherOrderApp

cmdDoStmts :: [A.DoStmt A.Cmd] -> [CmdLStmt GhcPs]
cmdDoStmts [] = []
cmdDoStmts [A.DoExpr cmd] = [lA (LastStmt noExtField (toLHsCmd cmd) Nothing noSyntaxExpr)]
cmdDoStmts (stmt : rest) = toCmdDoStmt stmt : cmdDoStmts rest

toCmdDoStmt :: A.DoStmt A.Cmd -> CmdLStmt GhcPs
toCmdDoStmt stmt =
  lA $
    case A.peelDoStmtAnn stmt of
      A.DoAnn _ inner -> unLoc (toCmdDoStmt inner)
      A.DoBind pat body -> BindStmt noAnn (toLPat pat) (toLHsCmd body)
      A.DoLetDecls decls -> LetStmt noAnn (localBinds decls)
      A.DoExpr body -> BodyStmt noExtField (toLHsCmd body) noSyntaxExpr noSyntaxExpr
      A.DoRecStmt stmts ->
        RecStmt
          noAnn
          (lA (map toCmdDoStmt stmts))
          []
          []
          noSyntaxExpr
          noSyntaxExpr
          noSyntaxExpr

cmdCaseAltMatch :: A.CaseAlt A.Cmd -> Match GhcPs (LHsCmd GhcPs)
cmdCaseAltMatch (A.CaseAlt _ pat rhs) = cmdMatch CaseAlt [pat] rhs

cmdMatch :: HsMatchContext (LIdP GhcPs) -> [A.Pattern] -> A.Rhs A.Cmd -> Match GhcPs (LHsCmd GhcPs)
cmdMatch context pats rhs =
  Match noExtField context (lA (map toLPat pats)) (cmdGrhss rhs)

cmdMatchGroup :: HsMatchContext (LIdP GhcPs) -> [Match GhcPs (LHsCmd GhcPs)] -> MatchGroup GhcPs (LHsCmd GhcPs)
cmdMatchGroup _ matches =
  MG FromSource (lA (map lA matches))

cmdGrhss :: A.Rhs A.Cmd -> GRHSs GhcPs (LHsCmd GhcPs)
cmdGrhss rhs =
  case rhs of
    A.UnguardedRhs _ body whereDecls ->
      GRHSs emptyComments (lA (GRHS noAnn [] (toLHsCmd body)) :| []) (whereBinds whereDecls)
    A.GuardedRhss _ rhss whereDecls ->
      GRHSs emptyComments (nonEmpty (map cmdGRHS rhss)) (whereBinds whereDecls)

cmdGRHS :: A.GuardedRhs A.Cmd -> LGRHS GhcPs (LHsCmd GhcPs)
cmdGRHS (A.GuardedRhs _ guards body) =
  lA (GRHS noAnn (map guardStmt guards) (toLHsCmd body))

quotedExprName :: A.Expr -> LIdP GhcPs
quotedExprName expr =
  case A.peelExprAnn expr of
    A.EVar name -> lA (toRdrName name)
    A.EList [] -> lA (getRdrName nilDataCon)
    A.ETuple flavor elems -> lA (getRdrName (tupleDataCon (boxity flavor) (length elems)))
    _ -> lA (mkRdrUnqual (mkVarOcc "unsupportedQuote"))

quotedTypeName :: A.Type -> LIdP GhcPs
quotedTypeName ty =
  case A.peelTypeAnn ty of
    A.TCon name _ -> lA (toRdrName name)
    A.TBuiltinCon A.TBuiltinList -> lA (getRdrName nilDataCon)
    A.TTuple flavor _ elems -> lA (getRdrName (tupleTyCon (boxity flavor) (length elems)))
    _ -> lA (mkRdrUnqual (mkDataOcc "UnsupportedQuote"))

toRdrName :: A.Name -> RdrName
toRdrName name =
  let occ = case A.nameType name of
        A.NameVarId -> mkVarOcc
        A.NameVarSym -> mkVarOcc
        A.NameConId -> mkDataOcc
        A.NameConSym -> mkDataOcc
      local = occ (T.unpack (A.nameText name))
   in case A.nameQualifier name of
        Nothing -> mkRdrUnqual local
        Just qualifier -> mkRdrQual (mkModuleName (T.unpack qualifier)) local

boxity :: A.TupleFlavor -> Boxity
boxity flavor =
  case flavor of
    A.Boxed -> Boxed
    A.Unboxed -> Unboxed

tupleSort :: A.TupleFlavor -> HsTupleSort
tupleSort flavor =
  case flavor of
    A.Boxed -> HsBoxedOrConstraintTuple
    A.Unboxed -> HsUnboxedTuple

promotionFlag :: A.TypePromotion -> PromotionFlag
promotionFlag promotion =
  case promotion of
    A.Unpromoted -> NotPromoted
    A.Promoted -> IsPromoted

sourceText :: T.Text -> SourceText
sourceText = SourceText . mkFastString . T.unpack

lA :: (HasAnnotation e) => a -> GenLocated e a
lA = noLocA

recordAnn :: (Maybe (EpToken "{"), Maybe (EpToken "}"))
recordAnn = (Just noAnn, Just noAnn)

conPatAnn :: (Maybe (EpToken "{"), Maybe (EpToken "}"))
conPatAnn = (Nothing, Nothing)

parAnn :: (EpToken "(", EpToken ")")
parAnn = (noAnn, noAnn)

tupleAnn :: (EpaLocation, EpaLocation)
tupleAnn = (EpaSpan noSrcSpan, EpaSpan noSrcSpan)

nonEmpty :: [a] -> NonEmpty a
nonEmpty [] = error "aihc-parser-compat: expected a non-empty list"
nonEmpty (x : xs) = x :| xs
