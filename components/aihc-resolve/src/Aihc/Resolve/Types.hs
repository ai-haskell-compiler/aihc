{-# LANGUAGE DeriveDataTypeable #-}

module Aihc.Resolve.Types
  ( ResolvedName (..),
    ResolutionAnnotation (..),
    ResolveError (..),
    ResolveResult (..),
    renderResolveResult,
    renderResolvedName,
  )
where

import Aihc.Parser.Syntax
  ( ArithSeq (..),
    CaseAlt (..),
    Cmd (..),
    CmdCaseAlt (..),
    CompStmt (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    GuardQualifier (..),
    GuardedRhs (..),
    Match (..),
    Module (..),
    Name,
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName,
    ValueDecl (..),
    fromAnnotation,
    renderName,
    renderUnqualifiedName,
  )
import Data.List (intercalate, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

data ResolvedName
  = ResolvedTopLevel Name
  | ResolvedLocal Int UnqualifiedName
  | ResolvedError String
  deriving (Eq, Show, Typeable)

data ResolutionAnnotation = ResolutionAnnotation
  { resolutionSpan :: !SourceSpan,
    resolutionName :: !Text,
    resolutionTarget :: !ResolvedName
  }
  deriving (Eq, Show, Typeable)

newtype ResolveError
  = ResolveNotImplemented String
  deriving (Eq, Show)

data ResolveResult = ResolveResult
  { resolvedModules :: [Module],
    resolveErrors :: [ResolveError]
  }
  deriving (Show)

renderResolveResult :: ResolveResult -> String
renderResolveResult result =
  intercalate "\n" (map renderResolutionAnnotation (sortOn annotationKey (collectModules (resolvedModules result))))

renderResolvedName :: ResolvedName -> String
renderResolvedName resolvedName =
  case resolvedName of
    ResolvedTopLevel name -> T.unpack (renderName name)
    ResolvedLocal uniqueId localName -> "Local " <> show uniqueId <> " " <> T.unpack (renderUnqualifiedName localName)
    ResolvedError msg -> "Error " <> msg

renderResolutionAnnotation :: ResolutionAnnotation -> String
renderResolutionAnnotation ann =
  renderSourceSpan (resolutionSpan ann)
    <> " "
    <> T.unpack (resolutionName ann)
    <> " => "
    <> renderResolvedName (resolutionTarget ann)

annotationKey :: ResolutionAnnotation -> (FilePath, Int, Int, Int, Int, Text)
annotationKey ann =
  case resolutionSpan ann of
    SourceSpan path startLine startCol endLine endCol _ _ ->
      (path, startLine, startCol, endLine, endCol, resolutionName ann)
    NoSourceSpan ->
      ("", maxBound, maxBound, maxBound, maxBound, resolutionName ann)

renderSourceSpan :: SourceSpan -> String
renderSourceSpan span' =
  case span' of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      show startLine
        <> ":"
        <> show startCol
        <> "-"
        <> show endLine
        <> ":"
        <> show endCol
    NoSourceSpan -> "<no-source>"

collectModules :: [Module] -> [ResolutionAnnotation]
collectModules = concatMap collectModule

collectModule :: Module -> [ResolutionAnnotation]
collectModule modu = concatMap collectDecl (moduleDecls modu)

collectDecl :: Decl -> [ResolutionAnnotation]
collectDecl decl =
  ownDeclAnnotation decl
    <> case decl of
      DeclAnn _ inner -> collectDecl inner
      DeclValue _ valueDecl ->
        case valueDecl of
          FunctionBind _ _ matches -> concatMap collectMatch matches
          PatternBind _ pat rhs -> collectPattern pat <> collectRhs rhs
      DeclTypeSig {} -> []
      DeclPatSyn {} -> []
      DeclPatSynSig {} -> []
      DeclStandaloneKindSig {} -> []
      DeclFixity {} -> []
      DeclRoleAnnotation {} -> []
      DeclTypeSyn {} -> []
      DeclTypeData {} -> []
      DeclData {} -> []
      DeclNewtype {} -> []
      DeclClass {} -> []
      DeclInstance {} -> []
      DeclStandaloneDeriving {} -> []
      DeclDefault {} -> []
      DeclSplice _ expr -> collectExpr expr
      DeclForeign {} -> []
      DeclTypeFamilyDecl {} -> []
      DeclDataFamilyDecl {} -> []
      DeclTypeFamilyInst {} -> []
      DeclDataFamilyInst {} -> []
      DeclPragma {} -> []

collectMatch :: Match -> [ResolutionAnnotation]
collectMatch match =
  concatMap collectPattern (matchPats match) <> collectRhs (matchRhs match)

collectRhs :: Rhs -> [ResolutionAnnotation]
collectRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> collectExpr expr
    GuardedRhss _ guardedRhss -> concatMap collectGuardedRhs guardedRhss

collectGuardedRhs :: GuardedRhs -> [ResolutionAnnotation]
collectGuardedRhs guardedRhs =
  concatMap collectGuardQualifier (guardedRhsGuards guardedRhs) <> collectExpr (guardedRhsBody guardedRhs)

collectGuardQualifier :: GuardQualifier -> [ResolutionAnnotation]
collectGuardQualifier qualifier =
  case qualifier of
    GuardExpr _ expr -> collectExpr expr
    GuardPat _ pat expr -> collectPattern pat <> collectExpr expr
    GuardLet _ decls -> concatMap collectDecl decls

collectPattern :: Pattern -> [ResolutionAnnotation]
collectPattern pat =
  ownPatternAnnotation pat
    <> case pat of
      PAnn _ inner -> collectPattern inner
      PTuple _ _ pats -> concatMap collectPattern pats
      PUnboxedSum _ _ _ inner -> collectPattern inner
      PList _ pats -> concatMap collectPattern pats
      PCon _ _ pats -> concatMap collectPattern pats
      PInfix _ left _ right -> collectPattern left <> collectPattern right
      PView _ expr inner -> collectExpr expr <> collectPattern inner
      PAs _ _ inner -> collectPattern inner
      PStrict _ inner -> collectPattern inner
      PIrrefutable _ inner -> collectPattern inner
      PParen _ inner -> collectPattern inner
      PRecord _ _ fields _ -> concatMap (collectPattern . snd) fields
      PTypeSig _ inner ty -> collectPattern inner <> collectType ty
      PSplice _ expr -> collectExpr expr
      PVar {} -> []
      PWildcard {} -> []
      PLit {} -> []
      PQuasiQuote {} -> []
      PNegLit {} -> []

collectExpr :: Expr -> [ResolutionAnnotation]
collectExpr expr =
  ownExprAnnotation expr
    <> case expr of
      EAnn _ inner -> collectExpr inner
      EIf _ cond trueBranch falseBranch -> collectExpr cond <> collectExpr trueBranch <> collectExpr falseBranch
      EMultiWayIf _ guardedRhss -> concatMap collectGuardedRhs guardedRhss
      ELambdaPats _ pats body -> concatMap collectPattern pats <> collectExpr body
      ELambdaCase _ alts -> concatMap collectCaseAlt alts
      EInfix _ left _ right -> collectExpr left <> collectExpr right
      ENegate _ inner -> collectExpr inner
      ESectionL _ inner _ -> collectExpr inner
      ESectionR _ _ inner -> collectExpr inner
      ELetDecls _ decls body -> concatMap collectDecl decls <> collectExpr body
      ECase _ scrutinee alts -> collectExpr scrutinee <> concatMap collectCaseAlt alts
      EDo _ stmts _ -> concatMap collectDoStmtExpr stmts
      EListComp _ inner stmts -> collectExpr inner <> concatMap collectCompStmt stmts
      EListCompParallel _ inner stmtGroups -> collectExpr inner <> concatMap (concatMap collectCompStmt) stmtGroups
      EArithSeq _ arithSeq -> collectArithSeq arithSeq
      ERecordCon _ _ fields _ -> concatMap (collectExpr . snd) fields
      ERecordUpd _ inner fields -> collectExpr inner <> concatMap (collectExpr . snd) fields
      ETypeSig _ inner ty -> collectExpr inner <> collectType ty
      EParen _ inner -> collectExpr inner
      EWhereDecls _ inner decls -> collectExpr inner <> concatMap collectDecl decls
      EList _ exprs -> concatMap collectExpr exprs
      ETuple _ _ exprs -> concatMap collectExpr (foldr maybeCons [] exprs)
      EUnboxedSum _ _ _ inner -> collectExpr inner
      ETypeApp _ inner ty -> collectExpr inner <> collectType ty
      EApp _ fun arg -> collectExpr fun <> collectExpr arg
      ETHExpQuote _ inner -> collectExpr inner
      ETHTypedQuote _ inner -> collectExpr inner
      ETHDeclQuote _ decls -> concatMap collectDecl decls
      ETHTypeQuote _ ty -> collectType ty
      ETHPatQuote _ pat -> collectPattern pat
      ETHSplice _ inner -> collectExpr inner
      ETHTypedSplice _ inner -> collectExpr inner
      EProc _ pat cmd -> collectPattern pat <> collectCmd cmd
      EVar {} -> []
      EInt {} -> []
      EIntHash {} -> []
      EIntBase {} -> []
      EIntBaseHash {} -> []
      EFloat {} -> []
      EFloatHash {} -> []
      EChar {} -> []
      ECharHash {} -> []
      EString {} -> []
      EStringHash {} -> []
      EOverloadedLabel {} -> []
      EQuasiQuote {} -> []
      ETHNameQuote {} -> []
      ETHTypeNameQuote {} -> []

collectCaseAlt :: CaseAlt -> [ResolutionAnnotation]
collectCaseAlt alt = collectPattern (caseAltPattern alt) <> collectRhs (caseAltRhs alt)

collectDoStmtExpr :: DoStmt Expr -> [ResolutionAnnotation]
collectDoStmtExpr stmt =
  case stmt of
    DoBind _ pat expr -> collectPattern pat <> collectExpr expr
    DoLet _ bindings -> concatMap (collectExpr . snd) bindings
    DoLetDecls _ decls -> concatMap collectDecl decls
    DoExpr _ expr -> collectExpr expr
    DoRecStmt _ stmts -> concatMap collectDoStmtExpr stmts

collectCompStmt :: CompStmt -> [ResolutionAnnotation]
collectCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> collectPattern pat <> collectExpr expr
    CompGuard _ expr -> collectExpr expr
    CompLet _ bindings -> concatMap (collectExpr . snd) bindings
    CompLetDecls _ decls -> concatMap collectDecl decls

collectArithSeq :: ArithSeq -> [ResolutionAnnotation]
collectArithSeq arithSeq =
  case arithSeq of
    ArithSeqFrom _ start -> collectExpr start
    ArithSeqFromThen _ start next -> collectExpr start <> collectExpr next
    ArithSeqFromTo _ start end -> collectExpr start <> collectExpr end
    ArithSeqFromThenTo _ start next end -> collectExpr start <> collectExpr next <> collectExpr end

collectCmd :: Cmd -> [ResolutionAnnotation]
collectCmd cmd =
  case cmd of
    CmdArrApp _ left _ right -> collectExpr left <> collectExpr right
    CmdApp _ inner expr -> collectCmd inner <> collectExpr expr
    CmdLam _ pats inner -> concatMap collectPattern pats <> collectCmd inner
    CmdPar _ inner -> collectCmd inner
    CmdIf _ expr thenCmd elseCmd -> collectExpr expr <> collectCmd thenCmd <> collectCmd elseCmd
    CmdCase _ expr alts -> collectExpr expr <> concatMap collectCmdCaseAlt alts
    CmdLet _ decls inner -> concatMap collectDecl decls <> collectCmd inner
    CmdDo _ stmts -> concatMap collectDoStmtCmd stmts
    CmdInfix _ left _ right -> collectCmd left <> collectCmd right

collectCmdCaseAlt :: CmdCaseAlt -> [ResolutionAnnotation]
collectCmdCaseAlt alt = collectPattern (cmdCaseAltPat alt) <> collectCmd (cmdCaseAltBody alt)

collectDoStmtCmd :: DoStmt Cmd -> [ResolutionAnnotation]
collectDoStmtCmd stmt =
  case stmt of
    DoBind _ pat cmd -> collectPattern pat <> collectCmd cmd
    DoLet _ bindings -> concatMap (collectExpr . snd) bindings
    DoLetDecls _ decls -> concatMap collectDecl decls
    DoExpr _ cmd -> collectCmd cmd
    DoRecStmt _ stmts -> concatMap collectDoStmtCmd stmts

collectType :: Type -> [ResolutionAnnotation]
collectType ty =
  case ty of
    TAnn _ inner -> collectType inner
    TImplicitParam _ _ inner -> collectType inner
    TForall _ _ inner -> collectType inner
    TApp _ left right -> collectType left <> collectType right
    TFun _ left right -> collectType left <> collectType right
    TTuple _ _ _ tys -> concatMap collectType tys
    TUnboxedSum _ tys -> concatMap collectType tys
    TList _ _ tys -> concatMap collectType tys
    TParen _ inner -> collectType inner
    TKindSig _ inner kind -> collectType inner <> collectType kind
    TContext _ constraints inner -> concatMap collectType constraints <> collectType inner
    TSplice _ expr -> collectExpr expr
    TVar {} -> []
    TCon {} -> []
    TTypeLit {} -> []
    TStar {} -> []
    TQuasiQuote {} -> []
    TWildcard {} -> []

ownDeclAnnotation :: Decl -> [ResolutionAnnotation]
ownDeclAnnotation decl =
  case decl of
    DeclAnn ann _ -> maybeToList (fromAnnotation ann :: Maybe ResolutionAnnotation)
    _ -> []

ownPatternAnnotation :: Pattern -> [ResolutionAnnotation]
ownPatternAnnotation pat =
  case pat of
    PAnn ann _ -> maybeToList (fromAnnotation ann :: Maybe ResolutionAnnotation)
    _ -> []

ownExprAnnotation :: Expr -> [ResolutionAnnotation]
ownExprAnnotation expr =
  case expr of
    EAnn ann _ -> maybeToList (fromAnnotation ann :: Maybe ResolutionAnnotation)
    _ -> []

maybeToList :: Maybe a -> [a]
maybeToList maybeValue =
  case maybeValue of
    Just value -> [value]
    Nothing -> []

maybeCons :: Maybe a -> [a] -> [a]
maybeCons maybeValue rest =
  case maybeValue of
    Just value -> value : rest
    Nothing -> rest
