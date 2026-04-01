{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Properties.BareSyntax
  ( Module (..),
    ModuleHead (..),
    WarningText (..),
    ExportSpec (..),
    ImportDecl (..),
    ImportSpec (..),
    ImportItem (..),
    Decl (..),
    ValueDecl (..),
    Match (..),
    Rhs (..),
    GuardedRhs (..),
    GuardQualifier (..),
    Literal (..),
    Pattern (..),
    Type (..),
    Constraint (..),
    Expr (..),
    TypeLiteral (..),
    TypePromotion (..),
    TupleFlavor (..),
    MatchHeadForm (..),
    ImportLevel (..),
    CaseAlt (..),
    DoStmt (..),
    CompStmt (..),
    ArithSeq (..),
    moduleName,
    moduleWarningText,
    moduleExports,
    toSyntaxModule,
    toSyntaxExpr,
    toSyntaxPattern,
    toSyntaxType,
    eraseModule,
    eraseExpr,
    erasePattern,
    eraseType,
  )
where

import Aihc.Parser.Syntax
  ( ExtensionSetting,
    ImportLevel (..),
    MatchHeadForm (..),
    TupleFlavor (..),
    TypeLiteral (..),
    TypePromotion (..),
  )
import qualified Aihc.Parser.Syntax as Syntax
import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)

data WarningText
  = DeprText Text
  | WarnText Text
  deriving (Eq, Show, Generic, NFData)

data Module = Module
  { moduleHead :: Maybe ModuleHead,
    moduleLanguagePragmas :: [ExtensionSetting],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Eq, Show, Generic, NFData)

data ModuleHead = ModuleHead
  { moduleHeadName :: Text,
    moduleHeadWarningText :: Maybe WarningText,
    moduleHeadExports :: Maybe [ExportSpec]
  }
  deriving (Eq, Show, Generic, NFData)

moduleName :: Module -> Maybe Text
moduleName modu = moduleHeadName <$> moduleHead modu

moduleWarningText :: Module -> Maybe WarningText
moduleWarningText modu = moduleHeadWarningText =<< moduleHead modu

moduleExports :: Module -> Maybe [ExportSpec]
moduleExports modu = moduleHeadExports =<< moduleHead modu

data ExportSpec
  = ExportModule Text
  | ExportVar (Maybe Text) Text
  | ExportAbs (Maybe Text) Text
  | ExportAll (Maybe Text) Text
  | ExportWith (Maybe Text) Text [Text]
  deriving (Eq, Show, Generic, NFData)

data ImportDecl = ImportDecl
  { importDeclLevel :: Maybe ImportLevel,
    importDeclPackage :: Maybe Text,
    importDeclQualified :: Bool,
    importDeclQualifiedPost :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Eq, Show, Generic, NFData)

data ImportSpec = ImportSpec
  { importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Eq, Show, Generic, NFData)

data ImportItem
  = ImportItemVar (Maybe Text) Text
  | ImportItemAbs (Maybe Text) Text
  | ImportItemAll (Maybe Text) Text
  | ImportItemWith (Maybe Text) Text [Text]
  deriving (Eq, Show, Generic, NFData)

data Decl
  = DeclValue ValueDecl
  | DeclTypeSig [Text] Type
  deriving (Eq, Show, Generic, NFData)

data ValueDecl
  = FunctionBind Text [Match]
  | PatternBind Pattern Rhs
  deriving (Eq, Show, Generic, NFData)

data Match = Match
  { matchHeadForm :: MatchHeadForm,
    matchPats :: [Pattern],
    matchRhs :: Rhs
  }
  deriving (Eq, Show, Generic, NFData)

data Rhs
  = UnguardedRhs Expr
  | GuardedRhss [GuardedRhs]
  deriving (Eq, Show, Generic, NFData)

data GuardedRhs = GuardedRhs
  { guardedRhsGuards :: [GuardQualifier],
    guardedRhsBody :: Expr
  }
  deriving (Eq, Show, Generic, NFData)

data GuardQualifier
  = GuardExpr Expr
  | GuardPat Pattern Expr
  | GuardLet [Decl]
  deriving (Eq, Show, Generic, NFData)

data Literal
  = LitInt Integer Text
  | LitIntHash Integer Text
  | LitIntBase Integer Text
  | LitIntBaseHash Integer Text
  | LitFloat Double Text
  | LitFloatHash Double Text
  | LitChar Char Text
  | LitCharHash Char Text
  | LitString Text Text
  | LitStringHash Text Text
  deriving (Eq, Show, Generic, NFData)

data Pattern
  = PVar Text
  | PWildcard
  | PLit Literal
  | PQuasiQuote Text Text
  | PTuple TupleFlavor [Pattern]
  | PUnboxedSum Int Int Pattern
  | PList [Pattern]
  | PCon Text [Pattern]
  | PInfix Pattern Text Pattern
  | PView Expr Pattern
  | PAs Text Pattern
  | PStrict Pattern
  | PIrrefutable Pattern
  | PNegLit Literal
  | PParen Pattern
  | PRecord Text [(Text, Pattern)] Bool
  | PTypeSig Pattern Type
  | PSplice Expr
  deriving (Eq, Show, Generic, NFData)

data Type
  = TVar Text
  | TCon Text TypePromotion
  | TTypeLit TypeLiteral
  | TStar
  | TQuasiQuote Text Text
  | TForall [Text] Type
  | TApp Type Type
  | TFun Type Type
  | TTuple TupleFlavor TypePromotion [Type]
  | TUnboxedSum [Type]
  | TList TypePromotion Type
  | TParen Type
  | TContext [Constraint] Type
  | TSplice Expr
  deriving (Eq, Show, Generic, NFData)

data Constraint
  = Constraint
      { constraintClass :: Text,
        constraintArgs :: [Type]
      }
  | CParen
      { constraintInner :: Constraint
      }
  deriving (Eq, Show, Generic, NFData)

data Expr
  = EVar Text
  | EInt Integer Text
  | EIntHash Integer Text
  | EIntBase Integer Text
  | EIntBaseHash Integer Text
  | EFloat Double Text
  | EFloatHash Double Text
  | EChar Char Text
  | ECharHash Char Text
  | EString Text Text
  | EStringHash Text Text
  | EQuasiQuote Text Text
  | EIf Expr Expr Expr
  | EMultiWayIf [GuardedRhs]
  | ELambdaPats [Pattern] Expr
  | ELambdaCase [CaseAlt]
  | EInfix Expr Text Expr
  | ENegate Expr
  | ESectionL Expr Text
  | ESectionR Text Expr
  | ELetDecls [Decl] Expr
  | ECase Expr [CaseAlt]
  | EDo [DoStmt]
  | EListComp Expr [CompStmt]
  | EListCompParallel Expr [[CompStmt]]
  | EArithSeq ArithSeq
  | ERecordCon Text [(Text, Expr)] Bool
  | ERecordUpd Expr [(Text, Expr)]
  | ETypeSig Expr Type
  | EParen Expr
  | EWhereDecls Expr [Decl]
  | EList [Expr]
  | ETuple TupleFlavor [Expr]
  | ETupleSection TupleFlavor [Maybe Expr]
  | ETupleCon TupleFlavor Int
  | EUnboxedSum Int Int Expr
  | ETypeApp Expr Type
  | EApp Expr Expr
  | ETHExpQuote Expr
  | ETHTypedQuote Expr
  | ETHDeclQuote [Decl]
  | ETHTypeQuote Type
  | ETHPatQuote Pattern
  | ETHNameQuote Text
  | ETHTypeNameQuote Text
  | ETHSplice Expr
  | ETHTypedSplice Expr
  deriving (Eq, Show, Generic, NFData)

data CaseAlt = CaseAlt
  { caseAltPattern :: Pattern,
    caseAltRhs :: Rhs
  }
  deriving (Eq, Show, Generic, NFData)

data DoStmt
  = DoBind Pattern Expr
  | DoLet [(Text, Expr)]
  | DoLetDecls [Decl]
  | DoExpr Expr
  deriving (Eq, Show, Generic, NFData)

data CompStmt
  = CompGen Pattern Expr
  | CompGuard Expr
  | CompLet [(Text, Expr)]
  | CompLetDecls [Decl]
  deriving (Eq, Show, Generic, NFData)

data ArithSeq
  = ArithSeqFrom Expr
  | ArithSeqFromThen Expr Expr
  | ArithSeqFromTo Expr Expr
  | ArithSeqFromThenTo Expr Expr Expr
  deriving (Eq, Show, Generic, NFData)

span0 :: Syntax.SourceSpan
span0 = Syntax.noSourceSpan

toSyntaxWarningText :: WarningText -> Syntax.WarningText
toSyntaxWarningText warningText =
  case warningText of
    DeprText msg -> Syntax.DeprText span0 msg
    WarnText msg -> Syntax.WarnText span0 msg

toSyntaxModuleHead :: ModuleHead -> Syntax.ModuleHead
toSyntaxModuleHead head' =
  Syntax.ModuleHead
    { Syntax.moduleHeadSpan = span0,
      Syntax.moduleHeadName = moduleHeadName head',
      Syntax.moduleHeadWarningText = toSyntaxWarningText <$> moduleHeadWarningText head',
      Syntax.moduleHeadExports = fmap (map toSyntaxExportSpec) (moduleHeadExports head')
    }

toSyntaxModule :: Module -> Syntax.Module
toSyntaxModule modu =
  Syntax.Module
    { Syntax.moduleSpan = span0,
      Syntax.moduleHead = toSyntaxModuleHead <$> moduleHead modu,
      Syntax.moduleLanguagePragmas = moduleLanguagePragmas modu,
      Syntax.moduleImports = map toSyntaxImportDecl (moduleImports modu),
      Syntax.moduleDecls = map toSyntaxDecl (moduleDecls modu)
    }

toSyntaxExportSpec :: ExportSpec -> Syntax.ExportSpec
toSyntaxExportSpec spec =
  case spec of
    ExportModule modName -> Syntax.ExportModule span0 modName
    ExportVar namespace name -> Syntax.ExportVar span0 namespace name
    ExportAbs namespace name -> Syntax.ExportAbs span0 namespace name
    ExportAll namespace name -> Syntax.ExportAll span0 namespace name
    ExportWith namespace name members -> Syntax.ExportWith span0 namespace name members

toSyntaxImportDecl :: ImportDecl -> Syntax.ImportDecl
toSyntaxImportDecl decl =
  Syntax.ImportDecl
    { Syntax.importDeclSpan = span0,
      Syntax.importDeclLevel = importDeclLevel decl,
      Syntax.importDeclPackage = importDeclPackage decl,
      Syntax.importDeclQualified = importDeclQualified decl,
      Syntax.importDeclQualifiedPost = importDeclQualifiedPost decl,
      Syntax.importDeclModule = importDeclModule decl,
      Syntax.importDeclAs = importDeclAs decl,
      Syntax.importDeclSpec = toSyntaxImportSpec <$> importDeclSpec decl
    }

toSyntaxImportSpec :: ImportSpec -> Syntax.ImportSpec
toSyntaxImportSpec spec =
  Syntax.ImportSpec
    { Syntax.importSpecSpan = span0,
      Syntax.importSpecHiding = importSpecHiding spec,
      Syntax.importSpecItems = map toSyntaxImportItem (importSpecItems spec)
    }

toSyntaxImportItem :: ImportItem -> Syntax.ImportItem
toSyntaxImportItem item =
  case item of
    ImportItemVar namespace name -> Syntax.ImportItemVar span0 namespace name
    ImportItemAbs namespace name -> Syntax.ImportItemAbs span0 namespace name
    ImportItemAll namespace name -> Syntax.ImportItemAll span0 namespace name
    ImportItemWith namespace name members -> Syntax.ImportItemWith span0 namespace name members

toSyntaxDecl :: Decl -> Syntax.Decl
toSyntaxDecl decl =
  case decl of
    DeclValue valueDecl -> Syntax.DeclValue span0 (toSyntaxValueDecl valueDecl)
    DeclTypeSig names ty -> Syntax.DeclTypeSig span0 names (toSyntaxType ty)

toSyntaxValueDecl :: ValueDecl -> Syntax.ValueDecl
toSyntaxValueDecl valueDecl =
  case valueDecl of
    FunctionBind name matches -> Syntax.FunctionBind span0 name (map toSyntaxMatch matches)
    PatternBind pat rhs -> Syntax.PatternBind span0 (toSyntaxPattern pat) (toSyntaxRhs rhs)

toSyntaxMatch :: Match -> Syntax.Match
toSyntaxMatch match =
  Syntax.Match
    { Syntax.matchSpan = span0,
      Syntax.matchHeadForm = matchHeadForm match,
      Syntax.matchPats = map toSyntaxPattern (matchPats match),
      Syntax.matchRhs = toSyntaxRhs (matchRhs match)
    }

toSyntaxRhs :: Rhs -> Syntax.Rhs
toSyntaxRhs rhs =
  case rhs of
    UnguardedRhs expr -> Syntax.UnguardedRhs span0 (toSyntaxExpr expr)
    GuardedRhss rhss -> Syntax.GuardedRhss span0 (map toSyntaxGuardedRhs rhss)

toSyntaxGuardedRhs :: GuardedRhs -> Syntax.GuardedRhs
toSyntaxGuardedRhs grhs =
  Syntax.GuardedRhs
    { Syntax.guardedRhsSpan = span0,
      Syntax.guardedRhsGuards = map toSyntaxGuardQualifier (guardedRhsGuards grhs),
      Syntax.guardedRhsBody = toSyntaxExpr (guardedRhsBody grhs)
    }

toSyntaxGuardQualifier :: GuardQualifier -> Syntax.GuardQualifier
toSyntaxGuardQualifier qualifier =
  case qualifier of
    GuardExpr expr -> Syntax.GuardExpr span0 (toSyntaxExpr expr)
    GuardPat pat expr -> Syntax.GuardPat span0 (toSyntaxPattern pat) (toSyntaxExpr expr)
    GuardLet decls -> Syntax.GuardLet span0 (map toSyntaxDecl decls)

toSyntaxLiteral :: Literal -> Syntax.Literal
toSyntaxLiteral lit =
  case lit of
    LitInt value repr -> Syntax.LitInt span0 value repr
    LitIntHash value repr -> Syntax.LitIntHash span0 value repr
    LitIntBase value repr -> Syntax.LitIntBase span0 value repr
    LitIntBaseHash value repr -> Syntax.LitIntBaseHash span0 value repr
    LitFloat value repr -> Syntax.LitFloat span0 value repr
    LitFloatHash value repr -> Syntax.LitFloatHash span0 value repr
    LitChar value repr -> Syntax.LitChar span0 value repr
    LitCharHash value repr -> Syntax.LitCharHash span0 value repr
    LitString value repr -> Syntax.LitString span0 value repr
    LitStringHash value repr -> Syntax.LitStringHash span0 value repr

toSyntaxPattern :: Pattern -> Syntax.Pattern
toSyntaxPattern pat =
  case pat of
    PVar name -> Syntax.PVar span0 name
    PWildcard -> Syntax.PWildcard span0
    PLit lit -> Syntax.PLit span0 (toSyntaxLiteral lit)
    PQuasiQuote quoter body -> Syntax.PQuasiQuote span0 quoter body
    PTuple tupleFlavor elems -> Syntax.PTuple span0 tupleFlavor (map toSyntaxPattern elems)
    PUnboxedSum altIdx arity inner -> Syntax.PUnboxedSum span0 altIdx arity (toSyntaxPattern inner)
    PList elems -> Syntax.PList span0 (map toSyntaxPattern elems)
    PCon con args -> Syntax.PCon span0 con (map toSyntaxPattern args)
    PInfix lhs op rhs -> Syntax.PInfix span0 (toSyntaxPattern lhs) op (toSyntaxPattern rhs)
    PView expr inner -> Syntax.PView span0 (toSyntaxExpr expr) (toSyntaxPattern inner)
    PAs name inner -> Syntax.PAs span0 name (toSyntaxPattern inner)
    PStrict inner -> Syntax.PStrict span0 (toSyntaxPattern inner)
    PIrrefutable inner -> Syntax.PIrrefutable span0 (toSyntaxPattern inner)
    PNegLit lit -> Syntax.PNegLit span0 (toSyntaxLiteral lit)
    PParen inner -> Syntax.PParen span0 (toSyntaxPattern inner)
    PRecord con fields rwc -> Syntax.PRecord span0 con [(name, toSyntaxPattern fieldPat) | (name, fieldPat) <- fields] rwc
    PTypeSig inner ty -> Syntax.PTypeSig span0 (toSyntaxPattern inner) (toSyntaxType ty)
    PSplice body -> Syntax.PSplice span0 (toSyntaxExpr body)

toSyntaxType :: Type -> Syntax.Type
toSyntaxType ty =
  case ty of
    TVar name -> Syntax.TVar span0 name
    TCon name promoted -> Syntax.TCon span0 name promoted
    TTypeLit lit -> Syntax.TTypeLit span0 lit
    TStar -> Syntax.TStar span0
    TQuasiQuote quoter body -> Syntax.TQuasiQuote span0 quoter body
    TForall binders inner -> Syntax.TForall span0 binders (toSyntaxType inner)
    TApp fn arg -> Syntax.TApp span0 (toSyntaxType fn) (toSyntaxType arg)
    TFun lhs rhs -> Syntax.TFun span0 (toSyntaxType lhs) (toSyntaxType rhs)
    TTuple tupleFlavor promoted elems -> Syntax.TTuple span0 tupleFlavor promoted (map toSyntaxType elems)
    TUnboxedSum elems -> Syntax.TUnboxedSum span0 (map toSyntaxType elems)
    TList promoted inner -> Syntax.TList span0 promoted (toSyntaxType inner)
    TParen inner -> Syntax.TParen span0 (toSyntaxType inner)
    TContext constraints inner -> Syntax.TContext span0 (map toSyntaxConstraint constraints) (toSyntaxType inner)
    TSplice body -> Syntax.TSplice span0 (toSyntaxExpr body)

toSyntaxConstraint :: Constraint -> Syntax.Constraint
toSyntaxConstraint constraint =
  case constraint of
    Constraint cls args ->
      Syntax.Constraint
        { Syntax.constraintSpan = span0,
          Syntax.constraintClass = cls,
          Syntax.constraintArgs = map toSyntaxType args
        }
    CParen inner -> Syntax.CParen span0 (toSyntaxConstraint inner)

toSyntaxExpr :: Expr -> Syntax.Expr
toSyntaxExpr expr =
  case expr of
    EVar name -> Syntax.EVar span0 name
    EInt value repr -> Syntax.EInt span0 value repr
    EIntHash value repr -> Syntax.EIntHash span0 value repr
    EIntBase value repr -> Syntax.EIntBase span0 value repr
    EIntBaseHash value repr -> Syntax.EIntBaseHash span0 value repr
    EFloat value repr -> Syntax.EFloat span0 value repr
    EFloatHash value repr -> Syntax.EFloatHash span0 value repr
    EChar value repr -> Syntax.EChar span0 value repr
    ECharHash value repr -> Syntax.ECharHash span0 value repr
    EString value repr -> Syntax.EString span0 value repr
    EStringHash value repr -> Syntax.EStringHash span0 value repr
    EQuasiQuote quoter body -> Syntax.EQuasiQuote span0 quoter body
    EIf cond yes no -> Syntax.EIf span0 (toSyntaxExpr cond) (toSyntaxExpr yes) (toSyntaxExpr no)
    EMultiWayIf rhss -> Syntax.EMultiWayIf span0 (map toSyntaxGuardedRhs rhss)
    ELambdaPats pats body -> Syntax.ELambdaPats span0 (map toSyntaxPattern pats) (toSyntaxExpr body)
    ELambdaCase alts -> Syntax.ELambdaCase span0 (map toSyntaxCaseAlt alts)
    EInfix lhs op rhs -> Syntax.EInfix span0 (toSyntaxExpr lhs) op (toSyntaxExpr rhs)
    ENegate inner -> Syntax.ENegate span0 (toSyntaxExpr inner)
    ESectionL inner op -> Syntax.ESectionL span0 (toSyntaxExpr inner) op
    ESectionR op inner -> Syntax.ESectionR span0 op (toSyntaxExpr inner)
    ELetDecls decls body -> Syntax.ELetDecls span0 (map toSyntaxDecl decls) (toSyntaxExpr body)
    ECase scrutinee alts -> Syntax.ECase span0 (toSyntaxExpr scrutinee) (map toSyntaxCaseAlt alts)
    EDo stmts -> Syntax.EDo span0 (map toSyntaxDoStmt stmts)
    EListComp body stmts -> Syntax.EListComp span0 (toSyntaxExpr body) (map toSyntaxCompStmt stmts)
    EListCompParallel body stmtss -> Syntax.EListCompParallel span0 (toSyntaxExpr body) (map (map toSyntaxCompStmt) stmtss)
    EArithSeq seq' -> Syntax.EArithSeq span0 (toSyntaxArithSeq seq')
    ERecordCon con fields rwc -> Syntax.ERecordCon span0 con [(name, toSyntaxExpr fieldExpr) | (name, fieldExpr) <- fields] rwc
    ERecordUpd target fields -> Syntax.ERecordUpd span0 (toSyntaxExpr target) [(name, toSyntaxExpr fieldExpr) | (name, fieldExpr) <- fields]
    ETypeSig inner ty -> Syntax.ETypeSig span0 (toSyntaxExpr inner) (toSyntaxType ty)
    EParen inner -> Syntax.EParen span0 (toSyntaxExpr inner)
    EWhereDecls body decls -> Syntax.EWhereDecls span0 (toSyntaxExpr body) (map toSyntaxDecl decls)
    EList elems -> Syntax.EList span0 (map toSyntaxExpr elems)
    ETuple tupleFlavor elems -> Syntax.ETuple span0 tupleFlavor (map toSyntaxExpr elems)
    ETupleSection tupleFlavor elems -> Syntax.ETupleSection span0 tupleFlavor (map (fmap toSyntaxExpr) elems)
    ETupleCon tupleFlavor n -> Syntax.ETupleCon span0 tupleFlavor n
    EUnboxedSum altIdx arity inner -> Syntax.EUnboxedSum span0 altIdx arity (toSyntaxExpr inner)
    ETypeApp inner ty -> Syntax.ETypeApp span0 (toSyntaxExpr inner) (toSyntaxType ty)
    EApp fn arg -> Syntax.EApp span0 (toSyntaxExpr fn) (toSyntaxExpr arg)
    ETHExpQuote body -> Syntax.ETHExpQuote span0 (toSyntaxExpr body)
    ETHTypedQuote body -> Syntax.ETHTypedQuote span0 (toSyntaxExpr body)
    ETHDeclQuote decls -> Syntax.ETHDeclQuote span0 (map toSyntaxDecl decls)
    ETHTypeQuote ty -> Syntax.ETHTypeQuote span0 (toSyntaxType ty)
    ETHPatQuote pat -> Syntax.ETHPatQuote span0 (toSyntaxPattern pat)
    ETHNameQuote name -> Syntax.ETHNameQuote span0 name
    ETHTypeNameQuote name -> Syntax.ETHTypeNameQuote span0 name
    ETHSplice body -> Syntax.ETHSplice span0 (toSyntaxExpr body)
    ETHTypedSplice body -> Syntax.ETHTypedSplice span0 (toSyntaxExpr body)

toSyntaxCaseAlt :: CaseAlt -> Syntax.CaseAlt
toSyntaxCaseAlt alt =
  Syntax.CaseAlt
    { Syntax.caseAltSpan = span0,
      Syntax.caseAltPattern = toSyntaxPattern (caseAltPattern alt),
      Syntax.caseAltRhs = toSyntaxRhs (caseAltRhs alt)
    }

toSyntaxDoStmt :: DoStmt -> Syntax.DoStmt
toSyntaxDoStmt stmt =
  case stmt of
    DoBind pat expr -> Syntax.DoBind span0 (toSyntaxPattern pat) (toSyntaxExpr expr)
    DoLet bindings -> Syntax.DoLet span0 [(name, toSyntaxExpr expr) | (name, expr) <- bindings]
    DoLetDecls decls -> Syntax.DoLetDecls span0 (map toSyntaxDecl decls)
    DoExpr expr -> Syntax.DoExpr span0 (toSyntaxExpr expr)

toSyntaxCompStmt :: CompStmt -> Syntax.CompStmt
toSyntaxCompStmt stmt =
  case stmt of
    CompGen pat expr -> Syntax.CompGen span0 (toSyntaxPattern pat) (toSyntaxExpr expr)
    CompGuard expr -> Syntax.CompGuard span0 (toSyntaxExpr expr)
    CompLet bindings -> Syntax.CompLet span0 [(name, toSyntaxExpr expr) | (name, expr) <- bindings]
    CompLetDecls decls -> Syntax.CompLetDecls span0 (map toSyntaxDecl decls)

toSyntaxArithSeq :: ArithSeq -> Syntax.ArithSeq
toSyntaxArithSeq seq' =
  case seq' of
    ArithSeqFrom from -> Syntax.ArithSeqFrom span0 (toSyntaxExpr from)
    ArithSeqFromThen from thenE -> Syntax.ArithSeqFromThen span0 (toSyntaxExpr from) (toSyntaxExpr thenE)
    ArithSeqFromTo from to -> Syntax.ArithSeqFromTo span0 (toSyntaxExpr from) (toSyntaxExpr to)
    ArithSeqFromThenTo from thenE to -> Syntax.ArithSeqFromThenTo span0 (toSyntaxExpr from) (toSyntaxExpr thenE) (toSyntaxExpr to)

eraseWarningText :: Syntax.WarningText -> WarningText
eraseWarningText warningText =
  case warningText of
    Syntax.DeprText _ msg -> DeprText msg
    Syntax.WarnText _ msg -> WarnText msg

eraseModule :: Syntax.Module -> Module
eraseModule modu =
  Module
    { moduleHead = eraseModuleHead <$> Syntax.moduleHead modu,
      moduleLanguagePragmas = Syntax.moduleLanguagePragmas modu,
      moduleImports = map eraseImportDecl (Syntax.moduleImports modu),
      moduleDecls = map eraseDecl (Syntax.moduleDecls modu)
    }

eraseModuleHead :: Syntax.ModuleHead -> ModuleHead
eraseModuleHead head' =
  ModuleHead
    { moduleHeadName = Syntax.moduleHeadName head',
      moduleHeadWarningText = eraseWarningText <$> Syntax.moduleHeadWarningText head',
      moduleHeadExports = fmap (map eraseExportSpec) (Syntax.moduleHeadExports head')
    }

eraseExportSpec :: Syntax.ExportSpec -> ExportSpec
eraseExportSpec spec =
  case spec of
    Syntax.ExportModule _ modName -> ExportModule modName
    Syntax.ExportVar _ namespace name -> ExportVar namespace name
    Syntax.ExportAbs _ namespace name -> ExportAbs namespace name
    Syntax.ExportAll _ namespace name -> ExportAll namespace name
    Syntax.ExportWith _ namespace name members -> ExportWith namespace name members

eraseImportDecl :: Syntax.ImportDecl -> ImportDecl
eraseImportDecl decl =
  ImportDecl
    { importDeclLevel = Syntax.importDeclLevel decl,
      importDeclPackage = Syntax.importDeclPackage decl,
      importDeclQualified = Syntax.importDeclQualified decl,
      importDeclQualifiedPost = Syntax.importDeclQualifiedPost decl,
      importDeclModule = Syntax.importDeclModule decl,
      importDeclAs = Syntax.importDeclAs decl,
      importDeclSpec = eraseImportSpec <$> Syntax.importDeclSpec decl
    }

eraseImportSpec :: Syntax.ImportSpec -> ImportSpec
eraseImportSpec spec =
  ImportSpec
    { importSpecHiding = Syntax.importSpecHiding spec,
      importSpecItems = map eraseImportItem (Syntax.importSpecItems spec)
    }

eraseImportItem :: Syntax.ImportItem -> ImportItem
eraseImportItem item =
  case item of
    Syntax.ImportItemVar _ namespace name -> ImportItemVar namespace name
    Syntax.ImportItemAbs _ namespace name -> ImportItemAbs namespace name
    Syntax.ImportItemAll _ namespace name -> ImportItemAll namespace name
    Syntax.ImportItemWith _ namespace name members -> ImportItemWith namespace name members

eraseDecl :: Syntax.Decl -> Decl
eraseDecl decl =
  case decl of
    Syntax.DeclValue _ valueDecl -> DeclValue (eraseValueDecl valueDecl)
    Syntax.DeclTypeSig _ names ty -> DeclTypeSig names (eraseType ty)
    _ -> error "eraseDecl: unsupported declaration outside generated subset"

eraseValueDecl :: Syntax.ValueDecl -> ValueDecl
eraseValueDecl valueDecl =
  case valueDecl of
    Syntax.FunctionBind _ name matches -> FunctionBind name (map eraseMatch matches)
    Syntax.PatternBind _ pat rhs -> PatternBind (erasePattern pat) (eraseRhs rhs)

eraseMatch :: Syntax.Match -> Match
eraseMatch match =
  Match
    { matchHeadForm = Syntax.matchHeadForm match,
      matchPats = map erasePattern (Syntax.matchPats match),
      matchRhs = eraseRhs (Syntax.matchRhs match)
    }

eraseRhs :: Syntax.Rhs -> Rhs
eraseRhs rhs =
  case rhs of
    Syntax.UnguardedRhs _ expr -> UnguardedRhs (eraseExpr expr)
    Syntax.GuardedRhss _ rhss -> GuardedRhss (map eraseGuardedRhs rhss)

eraseGuardedRhs :: Syntax.GuardedRhs -> GuardedRhs
eraseGuardedRhs grhs =
  GuardedRhs
    { guardedRhsGuards = map eraseGuardQualifier (Syntax.guardedRhsGuards grhs),
      guardedRhsBody = eraseExpr (Syntax.guardedRhsBody grhs)
    }

eraseGuardQualifier :: Syntax.GuardQualifier -> GuardQualifier
eraseGuardQualifier qualifier =
  case qualifier of
    Syntax.GuardExpr _ expr -> GuardExpr (eraseExpr expr)
    Syntax.GuardPat _ pat expr -> GuardPat (erasePattern pat) (eraseExpr expr)
    Syntax.GuardLet _ decls -> GuardLet (map eraseDecl decls)

eraseLiteral :: Syntax.Literal -> Literal
eraseLiteral lit =
  case lit of
    Syntax.LitInt _ value repr -> LitInt value repr
    Syntax.LitIntHash _ value repr -> LitIntHash value repr
    Syntax.LitIntBase _ value repr -> LitIntBase value repr
    Syntax.LitIntBaseHash _ value repr -> LitIntBaseHash value repr
    Syntax.LitFloat _ value repr -> LitFloat value repr
    Syntax.LitFloatHash _ value repr -> LitFloatHash value repr
    Syntax.LitChar _ value repr -> LitChar value repr
    Syntax.LitCharHash _ value repr -> LitCharHash value repr
    Syntax.LitString _ value repr -> LitString value repr
    Syntax.LitStringHash _ value repr -> LitStringHash value repr

erasePattern :: Syntax.Pattern -> Pattern
erasePattern pat =
  case pat of
    Syntax.PVar _ name -> PVar name
    Syntax.PWildcard _ -> PWildcard
    Syntax.PLit _ lit -> PLit (eraseLiteral lit)
    Syntax.PQuasiQuote _ quoter body -> PQuasiQuote quoter body
    Syntax.PTuple _ tupleFlavor elems -> PTuple tupleFlavor (map erasePattern elems)
    Syntax.PUnboxedSum _ altIdx arity inner -> PUnboxedSum altIdx arity (erasePattern inner)
    Syntax.PList _ elems -> PList (map erasePattern elems)
    Syntax.PCon _ con args -> PCon con (map erasePattern args)
    Syntax.PInfix _ lhs op rhs -> PInfix (erasePattern lhs) op (erasePattern rhs)
    Syntax.PView _ expr inner -> PView (eraseExpr expr) (erasePattern inner)
    Syntax.PAs _ name inner -> PAs name (erasePattern inner)
    Syntax.PStrict _ inner -> PStrict (erasePattern inner)
    Syntax.PIrrefutable _ inner -> PIrrefutable (erasePattern inner)
    Syntax.PNegLit _ lit -> PNegLit (eraseLiteral lit)
    Syntax.PParen _ inner -> PParen (erasePattern inner)
    Syntax.PRecord _ con fields rwc -> PRecord con [(name, erasePattern fieldPat) | (name, fieldPat) <- fields] rwc
    Syntax.PTypeSig _ inner ty -> PTypeSig (erasePattern inner) (eraseType ty)
    Syntax.PSplice _ body -> PSplice (eraseExpr body)

eraseType :: Syntax.Type -> Type
eraseType ty =
  case ty of
    Syntax.TVar _ name -> TVar name
    Syntax.TCon _ name promoted -> TCon name promoted
    Syntax.TTypeLit _ lit -> TTypeLit lit
    Syntax.TStar _ -> TStar
    Syntax.TQuasiQuote _ quoter body -> TQuasiQuote quoter body
    Syntax.TForall _ binders inner -> TForall binders (eraseType inner)
    Syntax.TApp _ fn arg -> TApp (eraseType fn) (eraseType arg)
    Syntax.TFun _ lhs rhs -> TFun (eraseType lhs) (eraseType rhs)
    Syntax.TTuple _ tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map eraseType elems)
    Syntax.TUnboxedSum _ elems -> TUnboxedSum (map eraseType elems)
    Syntax.TList _ promoted inner -> TList promoted (eraseType inner)
    Syntax.TParen _ inner -> TParen (eraseType inner)
    Syntax.TContext _ constraints inner -> TContext (map eraseConstraint constraints) (eraseType inner)
    Syntax.TSplice _ body -> TSplice (eraseExpr body)

eraseConstraint :: Syntax.Constraint -> Constraint
eraseConstraint constraint =
  case constraint of
    Syntax.Constraint _ cls args -> Constraint cls (map eraseType args)
    Syntax.CParen _ inner -> CParen (eraseConstraint inner)

eraseExpr :: Syntax.Expr -> Expr
eraseExpr expr =
  case expr of
    Syntax.EVar _ name -> EVar name
    Syntax.EInt _ value repr -> EInt value repr
    Syntax.EIntHash _ value repr -> EIntHash value repr
    Syntax.EIntBase _ value repr -> EIntBase value repr
    Syntax.EIntBaseHash _ value repr -> EIntBaseHash value repr
    Syntax.EFloat _ value repr -> EFloat value repr
    Syntax.EFloatHash _ value repr -> EFloatHash value repr
    Syntax.EChar _ value repr -> EChar value repr
    Syntax.ECharHash _ value repr -> ECharHash value repr
    Syntax.EString _ value repr -> EString value repr
    Syntax.EStringHash _ value repr -> EStringHash value repr
    Syntax.EQuasiQuote _ quoter body -> EQuasiQuote quoter body
    Syntax.EIf _ cond yes no -> EIf (eraseExpr cond) (eraseExpr yes) (eraseExpr no)
    Syntax.EMultiWayIf _ rhss -> EMultiWayIf (map eraseGuardedRhs rhss)
    Syntax.ELambdaPats _ pats body -> ELambdaPats (map erasePattern pats) (eraseExpr body)
    Syntax.ELambdaCase _ alts -> ELambdaCase (map eraseCaseAlt alts)
    Syntax.EInfix _ lhs op rhs -> EInfix (eraseExpr lhs) op (eraseExpr rhs)
    Syntax.ENegate _ inner -> ENegate (eraseExpr inner)
    Syntax.ESectionL _ inner op -> ESectionL (eraseExpr inner) op
    Syntax.ESectionR _ op inner -> ESectionR op (eraseExpr inner)
    Syntax.ELetDecls _ decls body -> ELetDecls (map eraseDecl decls) (eraseExpr body)
    Syntax.ECase _ scrutinee alts -> ECase (eraseExpr scrutinee) (map eraseCaseAlt alts)
    Syntax.EDo _ stmts -> EDo (map eraseDoStmt stmts)
    Syntax.EListComp _ body stmts -> EListComp (eraseExpr body) (map eraseCompStmt stmts)
    Syntax.EListCompParallel _ body stmtss -> EListCompParallel (eraseExpr body) (map (map eraseCompStmt) stmtss)
    Syntax.EArithSeq _ seq' -> EArithSeq (eraseArithSeq seq')
    Syntax.ERecordCon _ con fields rwc -> ERecordCon con [(name, eraseExpr fieldExpr) | (name, fieldExpr) <- fields] rwc
    Syntax.ERecordUpd _ target fields -> ERecordUpd (eraseExpr target) [(name, eraseExpr fieldExpr) | (name, fieldExpr) <- fields]
    Syntax.ETypeSig _ inner ty -> ETypeSig (eraseExpr inner) (eraseType ty)
    Syntax.EParen _ inner -> EParen (eraseExpr inner)
    Syntax.EWhereDecls _ body decls -> EWhereDecls (eraseExpr body) (map eraseDecl decls)
    Syntax.EList _ elems -> EList (map eraseExpr elems)
    Syntax.ETuple _ tupleFlavor elems -> ETuple tupleFlavor (map eraseExpr elems)
    Syntax.ETupleSection _ tupleFlavor elems -> ETupleSection tupleFlavor (map (fmap eraseExpr) elems)
    Syntax.ETupleCon _ tupleFlavor n -> ETupleCon tupleFlavor n
    Syntax.EUnboxedSum _ altIdx arity inner -> EUnboxedSum altIdx arity (eraseExpr inner)
    Syntax.ETypeApp _ inner ty -> ETypeApp (eraseExpr inner) (eraseType ty)
    Syntax.EApp _ fn arg -> EApp (eraseExpr fn) (eraseExpr arg)
    Syntax.ETHExpQuote _ body -> ETHExpQuote (eraseExpr body)
    Syntax.ETHTypedQuote _ body -> ETHTypedQuote (eraseExpr body)
    Syntax.ETHDeclQuote _ decls -> ETHDeclQuote (map eraseDecl decls)
    Syntax.ETHTypeQuote _ ty -> ETHTypeQuote (eraseType ty)
    Syntax.ETHPatQuote _ pat -> ETHPatQuote (erasePattern pat)
    Syntax.ETHNameQuote _ name -> ETHNameQuote name
    Syntax.ETHTypeNameQuote _ name -> ETHTypeNameQuote name
    Syntax.ETHSplice _ body -> ETHSplice (eraseExpr body)
    Syntax.ETHTypedSplice _ body -> ETHTypedSplice (eraseExpr body)

eraseCaseAlt :: Syntax.CaseAlt -> CaseAlt
eraseCaseAlt alt =
  CaseAlt
    { caseAltPattern = erasePattern (Syntax.caseAltPattern alt),
      caseAltRhs = eraseRhs (Syntax.caseAltRhs alt)
    }

eraseDoStmt :: Syntax.DoStmt -> DoStmt
eraseDoStmt stmt =
  case stmt of
    Syntax.DoBind _ pat expr -> DoBind (erasePattern pat) (eraseExpr expr)
    Syntax.DoLet _ bindings -> DoLet [(name, eraseExpr expr) | (name, expr) <- bindings]
    Syntax.DoLetDecls _ decls -> DoLetDecls (map eraseDecl decls)
    Syntax.DoExpr _ expr -> DoExpr (eraseExpr expr)

eraseCompStmt :: Syntax.CompStmt -> CompStmt
eraseCompStmt stmt =
  case stmt of
    Syntax.CompGen _ pat expr -> CompGen (erasePattern pat) (eraseExpr expr)
    Syntax.CompGuard _ expr -> CompGuard (eraseExpr expr)
    Syntax.CompLet _ bindings -> CompLet [(name, eraseExpr expr) | (name, expr) <- bindings]
    Syntax.CompLetDecls _ decls -> CompLetDecls (map eraseDecl decls)

eraseArithSeq :: Syntax.ArithSeq -> ArithSeq
eraseArithSeq seq' =
  case seq' of
    Syntax.ArithSeqFrom _ from -> ArithSeqFrom (eraseExpr from)
    Syntax.ArithSeqFromThen _ from thenE -> ArithSeqFromThen (eraseExpr from) (eraseExpr thenE)
    Syntax.ArithSeqFromTo _ from to -> ArithSeqFromTo (eraseExpr from) (eraseExpr to)
    Syntax.ArithSeqFromThenTo _ from thenE to -> ArithSeqFromThenTo (eraseExpr from) (eraseExpr thenE) (eraseExpr to)
