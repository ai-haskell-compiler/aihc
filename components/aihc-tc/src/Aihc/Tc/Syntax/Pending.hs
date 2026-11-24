{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The syntax tree that the type checker works on.
--
-- This tree is a copy of the syntax tree of "Aihc.Resolve.Syntax". The
-- type checker reads the resolved tree in this copy, and it writes what it
-- finds into the copy as fields with explicit types:
--
-- * Wrapper constructors such as 'ETc' and 'DeclTc' hold the information
--   for a whole node.
-- * Fields such as 'nameTc' and 'matchTc' hold the information for a name,
--   an equation, a rule, or a module.
--
-- Some of this information is pending. It has meta variables and evidence
-- variables that the solver has not filled in yet. "Aihc.Tc.Finalize"
-- changes this tree into the tree of "Aihc.Tc.Syntax", which has only final
-- information.
--
-- The tree keeps the 'Annotation' wrappers of the parser tree. They carry
-- the source spans.
--
-- The tree does not use the trees-that-grow idiom. Each phase that needs a
-- different tree has its own copy.
module Aihc.Tc.Syntax.Pending
  ( -- * Type checker information
    TcInfo (..),
    PatSynEquations (..),
    infoPending,
    infoIsPending,
    infoHasType,
    annotateDecl,
    annotateRhsCast,
    annotateExprCast,
    annotateFunCast,
    annotateSigCast,
    annotateBranchCast,
    annotateDoStmtCast,

    -- * Resolutions
    ResolutionResult (..),
    Resolved (..),
    ResolveError (..),
    RecKnotVariables (..),

    -- * Names
    Name (..),
    UnqualifiedName (..),
    BinderName,
    OperatorName,
    mkName,
    mkUnqualifiedName,
    qualifyName,
    renderName,
    renderUnqualifiedName,

    -- * Reading resolutions
    nameResolved,
    binderResolved,
    termResolved,
    typeResolved,
    resolvedNameEntity,
    resolvedBinderEntity,
    resolvedNameOrigin,
    recordSelectorNames,

    -- * Modules
    Module (..),
    ModuleHead (..),
    IEBundledMember (..),
    ExportSpec (..),
    ImportDecl (..),
    ImportSpec (..),
    ImportItem (..),

    -- * Declarations
    Decl (..),
    RuleDecl (..),
    RuleBinder (..),
    MultiplicityTag (..),
    ValueDecl (..),
    Match (..),
    PatSynDir (..),
    PatSynDecl (..),
    Rhs (..),
    GuardedRhs (..),
    GuardQualifier (..),
    RecordField (..),
    LiteralMatch (..),
    OverloadedLiteralMatch (..),
    NPlusKMatch (..),
    ListMatch (..),
    literalMatchNames,
    nPlusKMatchNames,
    listMatchNames,
    RoleAnnotation (..),
    TypeSynDecl (..),
    TypeFamilyDecl (..),
    TypeFamilyResultSig (..),
    TypeFamilyEq (..),
    DataFamilyDecl (..),
    TypeFamilyInst (..),
    DataFamilyInst (..),
    DataDecl (..),
    NewtypeDecl (..),
    DataConDecl (..),
    GadtBody (..),
    BangType (..),
    FieldDecl (..),
    DerivingClause (..),
    DerivingStrategy (..),
    StandaloneDerivingDecl (..),
    ClassDecl (..),
    ClassDeclItem (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    ForeignDecl (..),

    -- * Patterns and types
    Pattern (..),
    ForallTelescope (..),
    ArrowKind (..),
    Type (..),
    TyVarBinder (..),
    BinderHead (..),
    binderHeadName,
    binderHeadParams,
    binderHeadForm,
    instanceHeadName,
    instanceHeadTypes,
    gadtBodyResultType,
    moduleName,
    moduleExports,

    -- * Expressions
    Expr (..),
    CaseAlt (..),
    LambdaCaseAlt (..),
    DoStmt (..),
    Cmd (..),
    CompStmt (..),
    ArithSeq (..),

    -- * Literals
    Literal (..),
    peelLiteralAnn,

    -- * Wrapper peeling
    peelDeclAnn,
    peelExprAnn,
    peelPatternAnn,
    peelTypeAnn,
    peelTypeHead,
    peelDoStmtAnn,
    peelDataConAnn,
    peelClassDeclItemAnn,
    peelInstanceDeclItemAnn,
    peelGuardQualifierAnn,
    peelCompStmtAnn,
    peelArithSeqAnn,
    peelCmdAnn,

    -- * Annotations of the parser tree
    mkSpanAnnotation,
    annotationSpan,
    sourceSpanSourceName,
    pattern SourceSpan,

    -- * Types of the parser tree
    Annotation,
    ArrAppType (..),
    BuiltinCon (..),
    CallConv (..),
    DoFlavor (..),
    Extension (..),
    ExtensionSetting (..),
    FixityAssoc (..),
    FloatType (..),
    ForallVis (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    FunctionalDependency (..),
    IEBundledNamespace (..),
    IEEntityNamespace (..),
    ImportLevel (..),
    InstanceOverlapPragma (..),
    MatchHeadForm (..),
    NameType (..),
    NumericType (..),
    PatSynArgs (..),
    Pragma (..),
    PragmaType (..),
    PragmaUnpackKind (..),
    Role (..),
    RuleActivation (..),
    SourceSpan,
    TupleFlavor (..),
    TyVarBSpecificity (..),
    TyVarBVisibility (..),
    TypeFamilyInjectivity (..),
    TypeHeadForm (..),
    TypeLiteral (..),
    TypePromotion (..),
    TypeSyntaxForm (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArrAppType (..),
    BuiltinCon (..),
    CallConv (..),
    DoFlavor (..),
    Extension (..),
    ExtensionSetting (..),
    FixityAssoc (..),
    FloatType (..),
    ForallVis (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    FunctionalDependency (..),
    IEBundledNamespace (..),
    IEEntityNamespace (..),
    ImportLevel (..),
    InstanceOverlapPragma (..),
    MatchHeadForm (..),
    NameType (..),
    NumericType (..),
    PatSynArgs (..),
    Pragma (..),
    PragmaType (..),
    PragmaUnpackKind (..),
    Role (..),
    RuleActivation (..),
    SourceSpan,
    TupleFlavor (..),
    TyVarBSpecificity (..),
    TyVarBVisibility (..),
    TypeFamilyInjectivity (..),
    TypeHeadForm (..),
    TypeLiteral (..),
    TypePromotion (..),
    TypeSyntaxForm (..),
    fromAnnotation,
    mkSpanAnnotation,
    sourceSpanSourceName,
    pattern SourceSpan,
  )
import Aihc.Resolve
  ( Entity (..),
    GlobalName (..),
    PackageId,
    RecKnotVariables (..),
    ResolutionNamespace (..),
    ResolutionResult (..),
    ResolveError (..),
    Resolved (..),
    resolvedOf,
  )
import Aihc.Tc.Annotations
  ( CastDirection (..),
    PendingTcAnnotation,
    PendingTcCastAnnotation (..),
    PendingTcMatchArityCast,
    TcAnnotation,
    TcClassAnnotation,
    TcDerivingAnnotation,
    TcDerivingPlan,
    TcForeignImportAnnotation,
    TcInstanceAnnotation,
    TcInstanceMethodAnnotation,
    TcModuleIdentity,
  )
import Aihc.Tc.Env (DataFamilyInstanceInfo, TypeFamilyInstanceInfo)
import Aihc.Tc.Evidence (EvVar)
import Aihc.Tc.Types (TcType)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | One item that the type checker found for a node of the tree. Some
-- items are pending: the solver must fill in their meta variables and
-- evidence variables before the phases after the type checker read them.
data TcInfo
  = -- | The pending type of a node, with its type arguments and evidence.
    InfoType !PendingTcAnnotation
  | -- | A type that the checker writes as final, such as the type of a
    -- declaration.
    InfoCheckedType !TcAnnotation
  | -- | A pending cast of a node to a type that an equality proves.
    InfoCast !PendingTcCastAnnotation
  | -- | A pending cast that gives an equation the arity of its type.
    InfoMatchArityCast !PendingTcMatchArityCast
  | -- | The constructor pattern instantiates its type arguments.
    InfoPatternInstantiation
  | InfoInstanceMethod !TcInstanceMethodAnnotation
  | InfoInstance !TcInstanceAnnotation
  | InfoClass !TcClassAnnotation
  | InfoDeriving !TcDerivingAnnotation
  | -- | An instance that @deriving@ generated rather than the source.
    InfoDerivedInstance
  | -- | An instance that a @deriving@ clause generated.
    InfoDerivingClauseInstance
  | -- | A generated instance that reuses another instance.
    InfoCoercedDeriving !TcDerivingPlan
  | -- | The checked type of a record field selector.
    InfoSelectorType !TcType
  | -- | The next item of the statement is the occurrence type of this
    -- group variable at the knot of a @rec@ group.
    InfoRecKnotOccurrence !Entity
  | InfoModuleIdentity !TcModuleIdentity
  | InfoForeignImport !TcForeignImportAnnotation
  | InfoTypeFamilyInstance !TypeFamilyInstanceInfo
  | InfoDataFamilyInstance !DataFamilyInstanceInfo
  | -- | The checked equations of a pattern synonym.
    InfoPatSyn !PatSynEquations
  deriving (Eq, Show)

-- | The checked matcher equation, builder equations, and record field
-- selector equations of a pattern synonym. The desugarer emits them as
-- ordinary functions. A selector pairs its field label with its equation.
data PatSynEquations = PatSynEquations
  { patSynMatcher :: !Match,
    patSynBuilder :: !(Maybe [Match]),
    patSynSelectors :: ![(Text, Match)]
  }
  deriving (Eq, Show)

-- | A literal.
data Literal
  = -- | Metadata for the whole literal.
    LitAnn Annotation Literal
  | -- | What the type checker found for the whole literal.
    LitTc TcInfo Literal
  | -- | @1@, @1#@, @1#Word8@
    LitInt Integer NumericType Text
  | -- | @1.0@, @1.0#@, @1.0##@
    LitFloat Rational FloatType Text
  | -- | @'x'@
    LitChar Char Text
  | -- | @'x'#@
    LitCharHash Char Text
  | -- | @"hello"@
    LitString Text Text
  | -- | @"hello"#@
    LitStringHash Text Text
  deriving (Eq, Show)

-- | Remove the annotation and type checker wrappers of a literal.
peelLiteralAnn :: Literal -> Literal
peelLiteralAnn literal =
  case literal of
    LitAnn _ inner -> peelLiteralAnn inner
    LitTc _ inner -> peelLiteralAnn inner
    _ -> literal

-- | The source span that an annotation carries, if it carries one.
annotationSpan :: Annotation -> Maybe SourceSpan
annotationSpan = fromAnnotation

type BinderName = UnqualifiedName

type OperatorName = UnqualifiedName

-- | A name with no annotation and no resolution.
mkName :: Maybe Text -> NameType -> Text -> Name
mkName qualifier ty txt = Name qualifier ty txt [] Nothing []

-- | An unqualified name with no annotation and no resolution.
mkUnqualifiedName :: NameType -> Text -> UnqualifiedName
mkUnqualifiedName ty txt = UnqualifiedName ty txt [] Nothing []

-- | A qualified name from an unqualified name. The name keeps its
-- annotations, its resolution, and its type checker information.
qualifyName :: Maybe Text -> UnqualifiedName -> Name
qualifyName qualifier name =
  Name qualifier (unqualifiedNameType name) (unqualifiedNameText name) (unqualifiedNameAnns name) (unqualifiedNameResolution name) (unqualifiedNameTc name)

-- | The successful resolution of a name occurrence, in any namespace.
nameResolved :: Name -> Maybe Resolved
nameResolved name = resolvedOf =<< nameResolution name

-- | The successful resolution of a binder, whatever namespace it binds in.
binderResolved :: UnqualifiedName -> Maybe Resolved
binderResolved name = resolvedOf =<< unqualifiedNameResolution name

-- | The term resolution of a name occurrence.
termResolved :: Name -> Maybe Resolved
termResolved name =
  case nameResolved name of
    Just annotation | resolutionNamespace annotation == ResolutionNamespaceTerm -> Just annotation
    _ -> Nothing

-- | The type resolution of a name occurrence. A promoted data constructor
-- at the type level resolves in the term namespace, so this is any
-- resolution that is not of a module.
typeResolved :: Name -> Maybe Resolved
typeResolved name =
  case nameResolved name of
    Just annotation | resolutionNamespace annotation /= ResolutionNamespaceModule -> Just annotation
    _ -> Nothing

-- | The entity that a name occurrence stands for, in any namespace.
resolvedNameEntity :: Name -> Maybe Entity
resolvedNameEntity = fmap resolutionTarget . nameResolved

-- | The entity that a binder defines.
resolvedBinderEntity :: UnqualifiedName -> Maybe Entity
resolvedBinderEntity = fmap resolutionTarget . binderResolved

-- | The package and the module that define the top-level entity a name
-- occurrence stands for. A local binder or built-in syntax gives 'Nothing'.
resolvedNameOrigin :: Name -> Maybe (PackageId, Text)
resolvedNameOrigin name =
  case resolvedNameEntity name of
    Just (EntityGlobal global) -> Just (globalNamePackage global, globalNameModule global)
    _ -> Nothing

-- | The selector name of each record field label of one declaration. A
-- duplicate record field has a selector with its own name, which the
-- resolver puts on the field binder. Any other label is its own selector
-- name.
recordSelectorNames :: [DataConDecl] -> Map.Map Text Text
recordSelectorNames constructors =
  Map.fromList
    [ (unqualifiedNameText name, selector)
    | constructor <- constructors,
      fields <- recordFields constructor,
      field <- fields,
      name <- fieldNames field,
      let selector =
            case resolvedBinderEntity name of
              Just (EntityGlobal global) -> globalNameText global
              _ -> unqualifiedNameText name
    ]
  where
    recordFields constructor =
      case peelDataConAnn constructor of
        RecordCon _ _ _ fields -> [fields]
        GadtCon _ _ _ (GadtRecordBody fields _) -> [fields]
        _ -> []

-- | The name as the source writes it, with its qualifier.
renderName :: Name -> Text
renderName name =
  case nameQualifier name of
    Just qualifier -> qualifier <> "." <> nameText name
    Nothing -> nameText name

renderUnqualifiedName :: UnqualifiedName -> Text
renderUnqualifiedName = unqualifiedNameText

binderHeadName :: BinderHead name -> name
binderHeadName head' =
  case head' of
    PrefixBinderHead name _ -> name
    InfixBinderHead _ name _ _ -> name

binderHeadParams :: BinderHead name -> [TyVarBinder]
binderHeadParams head' =
  case head' of
    PrefixBinderHead _ params -> params
    InfixBinderHead lhs _ rhs tailParams -> lhs : rhs : tailParams

binderHeadForm :: BinderHead name -> TypeHeadForm
binderHeadForm head' =
  case head' of
    PrefixBinderHead {} -> TypeHeadPrefix
    InfixBinderHead {} -> TypeHeadInfix

-- | The class name of an instance head: the leftmost atom of the
-- application spine, under parentheses and wrappers.
instanceHeadName :: Type -> Maybe Name
instanceHeadName ty =
  case ty of
    TAnn _ inner -> instanceHeadName inner
    TResolved _ inner -> instanceHeadName inner
    TApp fun _ -> instanceHeadName fun
    TTypeApp fun _ -> instanceHeadName fun
    TParen inner -> instanceHeadName inner
    TCon name _ -> Just name
    TInfix _ name _ _ -> Just name
    _ -> Nothing

-- | The type arguments of an instance head.
instanceHeadTypes :: Type -> [Type]
instanceHeadTypes = go []
  where
    go acc ty =
      case ty of
        TAnn _ inner -> go acc inner
        TResolved _ inner -> go acc inner
        TApp fun arg -> go (arg : acc) fun
        TTypeApp fun _ -> go acc fun
        TParen inner -> go acc inner
        TInfix lhs _ _ rhs -> [lhs, rhs]
        _ -> acc

gadtBodyResultType :: GadtBody -> Type
gadtBodyResultType body =
  case body of
    GadtPrefixBody _ ty -> ty
    GadtRecordBody _ ty -> ty

moduleName :: Module -> Maybe Text
moduleName modu = moduleHeadName <$> moduleHead modu

moduleExports :: Module -> Maybe [ExportSpec]
moduleExports modu = moduleHeadExports =<< moduleHead modu

-- | Remove the annotation, error and type checker wrappers of a declaration.
peelDeclAnn :: Decl -> Decl
peelDeclAnn decl =
  case decl of
    DeclAnn _ inner -> peelDeclAnn inner
    DeclTc _ inner -> peelDeclAnn inner
    DeclError _ inner -> peelDeclAnn inner
    _ -> decl

-- | Remove the annotation, resolution and type checker wrappers of an expression.
peelExprAnn :: Expr -> Expr
peelExprAnn expr =
  case expr of
    EAnn _ inner -> peelExprAnn inner
    ETc _ inner -> peelExprAnn inner
    EResolved _ inner -> peelExprAnn inner
    _ -> expr

-- | Remove the annotation, resolution and type checker wrappers of a pattern.
peelPatternAnn :: Pattern -> Pattern
peelPatternAnn pat =
  case pat of
    PAnn _ inner -> peelPatternAnn inner
    PTc _ inner -> peelPatternAnn inner
    _ -> pat

-- | Remove the annotation and resolution wrappers of a type.
peelTypeAnn :: Type -> Type
peelTypeAnn ty =
  case ty of
    TAnn _ inner -> peelTypeAnn inner
    TResolved _ inner -> peelTypeAnn inner
    _ -> ty

-- | Remove the wrappers of a type, and its parentheses.
peelTypeHead :: Type -> Type
peelTypeHead ty =
  case ty of
    TAnn _ inner -> peelTypeHead inner
    TResolved _ inner -> peelTypeHead inner
    TParen inner -> peelTypeHead inner
    _ -> ty

-- | Remove the annotation, resolution and type checker wrappers of a constructor.
peelDataConAnn :: DataConDecl -> DataConDecl
peelDataConAnn decl =
  case decl of
    DataConAnn _ inner -> peelDataConAnn inner
    DataConTc _ inner -> peelDataConAnn inner
    DataConResolved _ inner -> peelDataConAnn inner
    _ -> decl

-- | Remove the annotation, error and type checker wrappers of a class item.
peelClassDeclItemAnn :: ClassDeclItem -> ClassDeclItem
peelClassDeclItemAnn item =
  case item of
    ClassItemAnn _ inner -> peelClassDeclItemAnn inner
    ClassItemTc _ inner -> peelClassDeclItemAnn inner
    ClassItemError _ inner -> peelClassDeclItemAnn inner
    _ -> item

-- | Remove the annotation, error and type checker wrappers of an instance item.
peelInstanceDeclItemAnn :: InstanceDeclItem -> InstanceDeclItem
peelInstanceDeclItemAnn item =
  case item of
    InstanceItemAnn _ inner -> peelInstanceDeclItemAnn inner
    InstanceItemTc _ inner -> peelInstanceDeclItemAnn inner
    InstanceItemError _ inner -> peelInstanceDeclItemAnn inner
    _ -> item

peelGuardQualifierAnn :: GuardQualifier -> GuardQualifier
peelGuardQualifierAnn qualifier =
  case qualifier of
    GuardAnn _ inner -> peelGuardQualifierAnn inner
    _ -> qualifier

peelCompStmtAnn :: CompStmt -> CompStmt
peelCompStmtAnn stmt =
  case stmt of
    CompAnn _ inner -> peelCompStmtAnn inner
    CompTc _ inner -> peelCompStmtAnn inner
    _ -> stmt

-- | Remove the annotation, resolution and type checker wrappers of a sequence.
peelArithSeqAnn :: ArithSeq -> ArithSeq
peelArithSeqAnn arithSeq =
  case arithSeq of
    ArithSeqAnn _ inner -> peelArithSeqAnn inner
    ArithSeqTc _ inner -> peelArithSeqAnn inner
    ArithSeqResolved _ inner -> peelArithSeqAnn inner
    _ -> arithSeq

peelCmdAnn :: Cmd -> Cmd
peelCmdAnn cmd =
  case cmd of
    CmdAnn _ inner -> peelCmdAnn inner
    _ -> cmd

-- | Remove the annotation, resolution, knot and type checker wrappers of a statement.
peelDoStmtAnn :: DoStmt body -> DoStmt body
peelDoStmtAnn stmt =
  case stmt of
    DoAnn _ inner -> peelDoStmtAnn inner
    DoTc _ inner -> peelDoStmtAnn inner
    DoResolved _ inner -> peelDoStmtAnn inner
    DoRecKnot _ inner -> peelDoStmtAnn inner
    _ -> stmt

-- | A qualified or unqualified name with type information.
--
-- The 'nameQualifier' is the module path (e.g., \"Data.List\"), and
-- 'nameText' is the local name (e.g., \"map\"). For unqualified names,
-- 'nameQualifier' is 'Nothing'. Example: @Data.List.map@.
data Name = Name
  { -- | Module qualifier (e.g., @\"Data.List\"@ for @Data.List.map@)
    nameQualifier :: Maybe Text,
    -- | Whether this is a variable, constructor, or operator
    nameType :: NameType,
    -- | The local name (e.g., @\"map\"@, @\".+.\"@)
    nameText :: Text,
    -- | Metadata attached to this name occurrence.
    nameAnns :: [Annotation],
    -- | What the resolver found for this occurrence. 'Nothing' when the
    -- resolver does not resolve a name in this position.
    nameResolution :: Maybe ResolutionResult,
    -- | What the type checker found for this occurrence, in the order that
    -- it found it.
    nameTc :: [TcInfo]
  }
  deriving (Eq, Show)

-- | An unqualified identifier or operator.
-- Examples: @map@, @Just@, @:+:@.
data UnqualifiedName = UnqualifiedName
  { unqualifiedNameType :: NameType,
    unqualifiedNameText :: Text,
    unqualifiedNameAnns :: [Annotation],
    -- | What the resolver found for this binder or occurrence. 'Nothing'
    -- when the resolver does not resolve a name in this position.
    unqualifiedNameResolution :: Maybe ResolutionResult,
    -- | What the type checker found for this binder or occurrence.
    unqualifiedNameTc :: [TcInfo]
  }
  deriving (Eq, Show)

-- | A whole Haskell module.
-- Example: @module M (x) where import Data.List; x = 1@.
data Module = Module
  { moduleAnns :: [Annotation],
    -- | What the type checker found for the whole module.
    moduleTc :: [TcInfo],
    moduleHead :: Maybe ModuleHead,
    moduleLanguagePragmas :: [ExtensionSetting],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Eq, Show)

-- | The optional @module ... where@ header.
-- Example: @module M (x, type T) where@.
data ModuleHead = ModuleHead
  { moduleHeadAnns :: [Annotation],
    moduleHeadName :: Text,
    moduleHeadWarningPragma :: Maybe Pragma,
    moduleHeadExports :: Maybe [ExportSpec]
  }
  deriving (Eq, Show)

-- | A member bundled with a parent import or export.
-- Example: @Just@ in @Maybe(Just, Nothing)@.
data IEBundledMember = IEBundledMember
  { ieBundledMemberNamespace :: Maybe IEBundledNamespace,
    ieBundledMemberName :: Name
  }
  deriving (Eq, Show)

-- | One item in a module export list.
data ExportSpec
  = -- | @module Data.List@
    ExportModule (Maybe Pragma) Text
  | -- | @map@ or @type T@
    ExportVar (Maybe Pragma) (Maybe IEEntityNamespace) Name
  | -- | @Maybe@
    ExportAbs (Maybe Pragma) (Maybe IEEntityNamespace) Name
  | -- | @Maybe(..)@
    ExportAll (Maybe Pragma) (Maybe IEEntityNamespace) Name
  | -- | @Maybe(Just, Nothing)@
    ExportWith (Maybe Pragma) (Maybe IEEntityNamespace) Name [IEBundledMember]
  | -- | @Maybe(Just, .., Nothing)@
    ExportWithAll (Maybe Pragma) (Maybe IEEntityNamespace) Name Int [IEBundledMember]
  | -- | Metadata for the whole export item.
    ExportAnn Annotation ExportSpec
  deriving (Eq, Show)

-- | A module import declaration.
-- Example: @import qualified Data.Map as Map hiding (lookup)@.
data ImportDecl = ImportDecl
  { importDeclAnns :: [Annotation],
    -- | The error when the imported module is not found or is ambiguous.
    importDeclError :: Maybe ResolveError,
    importDeclLevel :: Maybe ImportLevel,
    importDeclPackage :: Maybe Text,
    importDeclSourcePragma :: Maybe Pragma,
    importDeclSafe :: Bool,
    importDeclQualified :: Bool,
    importDeclQualifiedPost :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Eq, Show)

-- | An optional import list or hiding list.
-- Examples: @(map, foldr)@ and @hiding (map)@.
data ImportSpec = ImportSpec
  { importSpecAnns :: [Annotation],
    importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Eq, Show)

-- | One item in an import list.
data ImportItem
  = -- | @map@ or @type T@
    ImportItemVar (Maybe IEEntityNamespace) UnqualifiedName
  | -- | @Maybe@
    ImportItemAbs (Maybe IEEntityNamespace) UnqualifiedName
  | -- | @Maybe(..)@
    ImportItemAll (Maybe IEEntityNamespace) UnqualifiedName
  | -- | @Maybe(Just, Nothing)@
    ImportItemWith (Maybe IEEntityNamespace) UnqualifiedName [IEBundledMember]
  | -- | @Maybe(Just, .., Nothing)@
    ImportItemAllWith (Maybe IEEntityNamespace) UnqualifiedName Int [IEBundledMember]
  | -- | Metadata for the whole import item.
    ImportAnn Annotation ImportItem
  | -- | The item names something that the module does not export.
    ImportItemError ResolveError ImportItem
  deriving (Eq, Show)

-- | A top-level declaration.
data Decl
  = -- | Metadata for the whole declaration.
    DeclAnn Annotation Decl
  | -- | What the type checker found for the whole declaration.
    DeclTc TcInfo Decl
  | -- | A resolve error for the whole declaration.
    DeclError ResolveError Decl
  | -- | @f x = x@ or @x = 1@
    DeclValue ValueDecl
  | -- | @?x = e@ in a @let@ or @where@ group
    DeclImplicitParam Text Expr (Maybe [Decl])
  | -- | @f, g :: Int -> Int@
    DeclTypeSig [BinderName] Type
  | -- | @pattern P x = Just x@
    DeclPatSyn PatSynDecl
  | -- | @pattern P :: a -> Maybe a@
    DeclPatSynSig [BinderName] Type
  | -- | @type T :: Type -> Type@
    DeclStandaloneKindSig BinderName Type
  | -- | @infixr 5 :+:@
    DeclFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | -- | @type role T nominal representational@
    DeclRoleAnnotation RoleAnnotation
  | -- | @type Pair a = (a, a)@
    DeclTypeSyn TypeSynDecl
  | -- | @type data Nat = Z | S Nat@
    DeclTypeData DataDecl
  | -- | @data Maybe a = Nothing | Just a@
    DeclData DataDecl
  | -- | @newtype Identity a = Identity a@
    DeclNewtype NewtypeDecl
  | -- | @class Functor f where fmap :: (a -> b) -> f a -> f b@
    DeclClass ClassDecl
  | -- | @instance Show a => Show [a] where ...@
    DeclInstance InstanceDecl
  | -- | @deriving stock instance Show a => Show (T a)@
    DeclStandaloneDeriving StandaloneDerivingDecl
  | -- | @default (Integer, Double)@
    DeclDefault [Type]
  | -- | @$decl@ or @$(decl)@
    DeclSplice Expr
  | -- | @foreign import ccall "puts" puts :: CString -> IO CInt@
    DeclForeign ForeignDecl
  | -- | @type family F a :: Type@
    DeclTypeFamilyDecl TypeFamilyDecl
  | -- | @data family DF a@
    DeclDataFamilyDecl DataFamilyDecl
  | -- | @type instance F Int = Bool@
    DeclTypeFamilyInst TypeFamilyInst
  | -- | @data instance DF Int = DFInt@
    DeclDataFamilyInst DataFamilyInst
  | -- | A standalone pragma declaration such as @{-# INLINE f #-}@.
    DeclPragma Pragma
  | -- | @{-# RULES "map/map" forall f g xs. map f (map g xs) = map (f . g) xs #-}@
    DeclRules [RuleDecl]
  deriving (Eq, Show)

-- | One rewrite rule of a @RULES@ pragma.
-- Example: @"map/map" [2] forall f g xs. map f (map g xs) = map (f . g) xs@.
data RuleDecl = RuleDecl
  { ruleAnns :: [Annotation],
    -- | The type that the type checker found for the rule.
    ruleTc :: [TcInfo],
    -- | The error when the left-hand side does not apply a top-level
    -- variable.
    ruleError :: Maybe ResolveError,
    -- | The name in double quotes. It only identifies the rule in reports.
    ruleName :: Text,
    -- | The phase control after the name, such as @[2]@ or @[~2]@.
    ruleActivation :: Maybe RuleActivation,
    -- | The type variables of a leading @forall a b.@ that a second
    -- @forall@ follows. A rule with one @forall@ binds only term variables.
    ruleTypeBinders :: [TyVarBinder],
    -- | The pattern variables bound by @forall@.
    ruleBinders :: [RuleBinder],
    -- | The left-hand side, matched against the program.
    ruleLhs :: Expr,
    -- | The right-hand side, put in place of a match.
    ruleRhs :: Expr
  }
  deriving (Eq, Show)

-- | One pattern variable of a rule, bound by @forall@.
-- Examples: @xs@ and @(g :: forall b. (a -> b -> b) -> b -> b)@.
data RuleBinder = RuleBinder
  { ruleBinderAnns :: [Annotation],
    -- | The type that the type checker found for the binder.
    ruleBinderTc :: [TcInfo],
    ruleBinderName :: UnqualifiedName,
    ruleBinderType :: Maybe Type
  }
  deriving (Eq, Show)

-- | Multiplicity tag on a let/where pattern binding.
-- Corresponds to @x = e@ (no tag), @%1 x = e@ (linear), @%m x = e@ (explicit).
data MultiplicityTag
  = -- | @x = e@
    NoMultiplicityTag
  | -- | @%1 x = e@
    LinearMultiplicityTag
  | -- | @%m x = e@
    ExplicitMultiplicityTag Type
  deriving (Eq, Show)

-- | A value binding.
data ValueDecl
  = -- | @f x y = body@
    FunctionBind BinderName [Match]
  | -- | @pat = body@ or @%1 pat = body@
    PatternBind MultiplicityTag Pattern (Rhs Expr)
  deriving (Eq, Show)

-- | One equation of a function or pattern synonym.
-- Examples: @f x = x@ and @x :+: y = z@.
data Match = Match
  { matchAnns :: [Annotation],
    -- | What the type checker found for the equation.
    matchTc :: [TcInfo],
    matchHeadForm :: MatchHeadForm,
    matchPats :: [Pattern],
    matchRhs :: Rhs Expr
  }
  deriving (Eq, Show)

-- | Pattern synonym declaration direction.
data PatSynDir
  = -- | @pattern P x <- pat@
    PatSynUnidirectional
  | -- | @pattern P x = pat@
    PatSynBidirectional
  | -- | @pattern P x <- pat where P x = expr@
    PatSynExplicitBidirectional [Match]
  deriving (Eq, Show)

-- | Pattern synonym declaration.
-- Example: @pattern JustOne x = Just [x]@.
data PatSynDecl = PatSynDecl
  { patSynDeclName :: UnqualifiedName,
    patSynDeclArgs :: PatSynArgs,
    patSynDeclPat :: Pattern,
    patSynDeclDir :: PatSynDir
  }
  deriving (Eq, Show)

-- | The right-hand side of a binding, case alternative, or lambda case.
data Rhs body
  = -- | @= body@ with an optional @where@ clause. The second list holds
    -- what the type checker found for the right-hand side.
    UnguardedRhs [Annotation] [TcInfo] body (Maybe [Decl])
  | -- | Guarded equations such as @| x > 0 = body@.
    GuardedRhss [Annotation] [TcInfo] [GuardedRhs body] (Maybe [Decl])
  deriving (Eq, Show)

-- | One guarded branch.
-- Example: @| x > 0 = x@.
data GuardedRhs body = GuardedRhs
  { guardedRhsAnns :: [Annotation],
    guardedRhsGuards :: [GuardQualifier],
    guardedRhsBody :: body
  }
  deriving (Eq, Show)

-- | A single qualifier inside a guarded right-hand side.
data GuardQualifier
  = -- | Metadata for the whole guard qualifier (typically a 'SourceSpan' via 'mkSpanAnnotation').
    GuardAnn Annotation GuardQualifier
  | -- | @x > 0@
    GuardExpr Expr
  | -- | @Just y <- mx@
    GuardPat Pattern Expr
  | -- | @let y = f x@
    GuardLet [Decl]
  deriving (Eq, Show)

-- | One record field occurrence.
-- Examples: @x = 1@ in @T { x = 1 }@ and @x@ in @T { x }@.
data RecordField a = RecordField
  { recordFieldName :: Name,
    recordFieldValue :: a,
    recordFieldPun :: Bool
  }
  deriving (Eq, Show)

-- | The names that the match of a literal pattern uses.
data LiteralMatch
  = -- | A literal of a fixed type, such as @'x'@, or a string literal
    -- without @OverloadedStrings@. The match uses no name.
    LiteralMatchFixed
  | -- | A primitive literal, such as @1#@. The name is its primitive type.
    LiteralMatchPrimitive Name
  | -- | An overloaded literal, such as @1@ or @1.5@.
    LiteralMatchOverloaded OverloadedLiteralMatch
  deriving (Eq, Show)

-- | The names that the match of an overloaded literal pattern uses.
-- The pattern @-1@ matches @v@ when @v == negate (fromInteger 1)@.
data OverloadedLiteralMatch = OverloadedLiteralMatch
  { -- | The type of the literal before the conversion: @Integer@ for an
    -- integer literal. 'Nothing' when the built-in scope has no such type.
    overloadedLiteralType :: Maybe Name,
    -- | @fromInteger@, @fromRational@, or @fromString@.
    overloadedLiteralConversion :: Name,
    -- | @negate@. Only a negated literal pattern has it.
    overloadedLiteralNegate :: Maybe Name,
    -- | @==@, which compares the converted literal with the scrutinee.
    overloadedLiteralEquality :: Name
  }
  deriving (Eq, Show)

-- | The names that the match of an n+k pattern uses.
-- The Haskell 98 Report gives the match of @n+k@ against @v@ as the test
-- @v >= fromInteger k@. Then @n@ binds to @v - fromInteger k@.
data NPlusKMatch = NPlusKMatch
  { -- | @Integer@. 'Nothing' when the built-in scope has no such type.
    nPlusKType :: Maybe Name,
    nPlusKFromInteger :: Name,
    nPlusKGreaterEqual :: Name,
    nPlusKMinus :: Name
  }
  deriving (Eq, Show)

-- | The names of a list pattern under @OverloadedLists@.
-- The match applies @toList@ to the scrutinee. A bidirectional pattern
-- synonym builds its value with @fromListN@.
data ListMatch = ListMatch
  { listMatchToList :: Name,
    listMatchFromListN :: Name
  }
  deriving (Eq, Show)

-- | The names of the match of a literal pattern.
literalMatchNames :: LiteralMatch -> [Name]
literalMatchNames match =
  case match of
    LiteralMatchFixed -> []
    LiteralMatchPrimitive typeName -> [typeName]
    LiteralMatchOverloaded (OverloadedLiteralMatch literalType conversion negation equality) ->
      maybe [] pure literalType <> [conversion] <> maybe [] pure negation <> [equality]

-- | The names of the match of an n+k pattern.
nPlusKMatchNames :: NPlusKMatch -> [Name]
nPlusKMatchNames (NPlusKMatch literalType conversion comparison minus) =
  maybe [] pure literalType <> [conversion, comparison, minus]

-- | The names of the match of a list pattern under @OverloadedLists@.
listMatchNames :: ListMatch -> [Name]
listMatchNames (ListMatch toList fromListN) = [toList, fromListN]

-- | A pattern.
data Pattern
  = -- | Metadata for the whole pattern.
    PAnn Annotation Pattern
  | -- | What the type checker found for the whole pattern.
    PTc TcInfo Pattern
  | -- | @x@
    PVar UnqualifiedName
  | -- | @\@a@ in a required type argument pattern.
    PTypeBinder TyVarBinder
  | -- | @type T@ in a term-level pattern position.
    PTypeSyntax TypeSyntaxForm Type
  | -- | @_@
    PWildcard
  | -- | @1@ or @'x'@
    PLit LiteralMatch Literal
  | -- | @[p| body |]@
    PQuasiQuote Text Text
  | -- | @(p1, p2)@ or @(# p1, p2 #)@
    PTuple TupleFlavor [Pattern]
  | -- | @(# | p | #)@
    PUnboxedSum Int Int Pattern
  | -- | @[p1, p2]@
    PList (Maybe ListMatch) [Pattern]
  | -- | @Just x@ or @Proxy \@Type@
    PCon Name [Type] [Pattern]
  | -- | @(,) x y@ or @(#,#) x y@: a built-in constructor applied to
    -- invisible type arguments and patterns.
    PBuiltinCon BuiltinCon [Type] [Pattern]
  | -- | @x :+: y@
    PInfix Pattern Name Pattern
  | -- | @(view -> pat)@
    PView Expr Pattern
  | -- | @name\@pat@
    PAs UnqualifiedName Pattern
  | -- | @!pat@
    PStrict Pattern
  | -- | @~pat@
    PIrrefutable Pattern
  | -- | @-1@
    PNegLit LiteralMatch Literal
  | -- | @n+1@, an n+k pattern.  It needs the @NPlusKPatterns@ extension.
    -- The literal is always a non-negative integer literal.
    PNPlusK NPlusKMatch UnqualifiedName Literal
  | -- | @(pat)@
    PParen Pattern
  | -- | @T { x = p, y, .. }@
    PRecord Name [RecordField Pattern] Bool -- Bool: wildcard present
  | -- | @(pat :: ty)@
    PTypeSig Pattern Type
  | -- | @$pat@ or @$(pat)@
    PSplice Expr
  -- \$pat or $(pat) (TH pattern splice)
  deriving (Eq, Show)

-- | The binders introduced by a @forall@.
-- Example: @forall a b. a -> b -> a@.
data ForallTelescope = ForallTelescope
  { forallTelescopeVisibility :: ForallVis,
    forallTelescopeBinders :: [TyVarBinder]
  }
  deriving (Eq, Show)

-- | The kind of a function arrow.
-- @ArrowUnrestricted@ is the ordinary @->@.
-- @ArrowLinear@ is @%1 ->@ (or its Unicode equivalent @⊸@).
-- @ArrowExplicit ty@ is @%m ->@ for an arbitrary multiplicity type @m@.
data ArrowKind
  = ArrowUnrestricted
  | ArrowLinear
  | ArrowExplicit Type
  deriving (Eq, Show)

-- | A type.
data Type
  = -- | Metadata for the whole type.
    TAnn Annotation Type
  | -- | The resolution of built-in type syntax, such as a tuple or a list,
    -- or a resolve error.
    TResolved ResolutionResult Type
  | -- | @a@
    TVar UnqualifiedName
  | -- | @Maybe@ or @'Just@
    TCon Name TypePromotion
  | -- | @(,)@, @(# , #)@, @(->)@, @[]@, @(:)@, or a promoted form such as @'[]@
    TBuiltinCon BuiltinCon TypePromotion
  | -- | @(?x :: Int) => Int@
    TImplicitParam Text Type
  | -- | @1@, @"x"@, or @'c'@ at the type level.
    TTypeLit TypeLiteral
  | -- | @*@ as written source syntax.
    TStar Text
  | -- | @[t| body |]@
    TQuasiQuote Text Text
  | -- | @forall a. ty@
    TForall ForallTelescope Type
  | -- | @Maybe a@
    TApp Type Type
  | -- | @Proxy \@Type@
    TTypeApp Type Type
  | -- | @a :+: b@
    TInfix Type Name TypePromotion Type
  | -- | @a -> b@, @a %1 -> b@, or @a %m -> b@
    TFun ArrowKind Type Type
  | -- | @(a, b)@, @'(a, b)@, or @(# a, b #)@
    TTuple TupleFlavor TypePromotion [Type]
  | -- | @(# a | b #)@
    TUnboxedSum [Type]
  | -- | @[a]@ or @'[a, b]@
    TList TypePromotion [Type]
  | -- | @(ty)@
    TParen Type
  | -- | @(a :: Type)@
    TKindSig Type Type
  | -- | @(Show a, Eq a) => a -> String@
    TContext [Type] Type
  | -- | @$typ@ or @$(typ)@
    TSplice Expr
  | -- \$typ or $(typ) (TH type splice)
    -- \_ (wildcard type, used in type family instance patterns)
    TWildcard
  deriving (Eq, Show)

-- | One type-variable binder.
-- Examples: @a@, @(a :: Type)@, @{a}@, and @@k@.
data TyVarBinder = TyVarBinder
  { tyVarBinderAnns :: [Annotation],
    tyVarBinderName :: Text,
    -- | Optional kind annotation. Examples: @(a :: Type)@ and @{a :: Type}@.
    tyVarBinderKind :: Maybe Type,
    -- | Whether the binder was written as specified (@a@, @(a :: k)@)
    -- or inferred (@{a}@, @{a :: k}@).
    tyVarBinderSpecificity :: TyVarBSpecificity,
    -- | Whether the binder was written visibly (@a@) or invisibly (@@a@).
    tyVarBinderVisibility :: TyVarBVisibility
  }
  deriving (Eq, Show)

-- | The head of a declaration that binds a type constructor or class.
data BinderHead name
  = -- | @T a b@
    PrefixBinderHead name [TyVarBinder]
  | -- | @a :+: b@
    InfixBinderHead TyVarBinder name TyVarBinder [TyVarBinder]
  deriving (Eq, Show)

-- | A @type role@ declaration.
-- Example: @type role T nominal representational@.
data RoleAnnotation = RoleAnnotation
  { roleAnnotationName :: UnqualifiedName,
    roleAnnotationRoles :: [Role]
  }
  deriving (Eq, Show)

-- | A @type@ synonym declaration.
-- Example: @type Pair a = (a, a)@.
data TypeSynDecl = TypeSynDecl
  { typeSynHead :: BinderHead UnqualifiedName,
    typeSynBody :: Type
  }
  deriving (Eq, Show)

-- | Open or closed type synonym family declaration.
-- Used for top-level @type family F a@ and associated @type F a :: Kind@ in class bodies.
-- Examples: @type family F a :: Type@ and
-- @type family F a where F Int = Bool@.
data TypeFamilyDecl = TypeFamilyDecl
  { typeFamilyDeclHeadForm :: TypeHeadForm,
    typeFamilyDeclExplicitFamilyKeyword :: Bool,
    -- | Family head type. For simple families like @type family F a@, this is @TCon "F"@.
    -- For infix families like @type family l `And` r@, this is the full infix type.
    typeFamilyDeclHead :: Type,
    typeFamilyDeclParams :: [TyVarBinder],
    -- | Optional result signature, either an unnamed kind annotation (@:: Kind@)
    -- or a named result variable with injectivity annotation (@= r | r -> a@).
    typeFamilyDeclResultSig :: Maybe TypeFamilyResultSig,
    -- | @Nothing@ = open family; @Just eqs@ = closed family with equations
    typeFamilyDeclEquations :: Maybe [TypeFamilyEq]
  }
  deriving (Eq, Show)

-- | The result signature on a type family head.
data TypeFamilyResultSig
  = -- | @type family F a :: Type@
    TypeFamilyKindSig Type
  | -- | @type family F a = r@
    TypeFamilyTyVarSig TyVarBinder
  | -- | @type family F a = r | r -> a@
    TypeFamilyInjectiveSig TyVarBinder TypeFamilyInjectivity
  deriving (Eq, Show)

-- | One equation in a closed type family: @[forall binders.] LhsType = RhsType@
-- Example: @forall a. F [a] = Maybe a@.
data TypeFamilyEq = TypeFamilyEq
  { typeFamilyEqAnns :: [Annotation],
    typeFamilyEqForall :: [TyVarBinder],
    typeFamilyEqHeadForm :: TypeHeadForm,
    typeFamilyEqLhs :: Type,
    typeFamilyEqRhs :: Type
  }
  deriving (Eq, Show)

-- | Data family declaration (standalone or associated in a class body).
-- Example: @data family DF a :: Type@.
data DataFamilyDecl = DataFamilyDecl
  { dataFamilyDeclHead :: BinderHead UnqualifiedName,
    -- | Optional result kind annotation (@:: Kind@)
    dataFamilyDeclKind :: Maybe Type
  }
  deriving (Eq, Show)

-- | Type family instance: @type [instance] [forall binders.] LhsType = RhsType@
-- Example: @type instance F Int = Bool@.
data TypeFamilyInst = TypeFamilyInst
  { typeFamilyInstForall :: [TyVarBinder],
    typeFamilyInstHeadForm :: TypeHeadForm,
    typeFamilyInstLhs :: Type,
    typeFamilyInstRhs :: Type
  }
  deriving (Eq, Show)

-- | Data or newtype family instance (standalone or in an instance body).
-- Examples: @data instance DF Int = DFInt@ and
-- @newtype instance DF Bool = DFBool Bool@.
data DataFamilyInst = DataFamilyInst
  { -- | @True@ when declared with @newtype instance@
    dataFamilyInstIsNewtype :: Bool,
    dataFamilyInstForall :: [TyVarBinder],
    -- | The LHS type-application pattern (e.g. @GMap (Either a b) v@)
    dataFamilyInstHead :: Type,
    -- | Optional inline result kind annotation (@:: Kind@) before @=@ or @where@
    dataFamilyInstKind :: Maybe Type,
    dataFamilyInstConstructors :: [DataConDecl],
    dataFamilyInstDeriving :: [DerivingClause]
  }
  deriving (Eq, Show)

-- | A @data@ or @type data@ declaration body.
-- Examples: @data Maybe a = Nothing | Just a@ and @type data Nat = Z | S Nat@.
data DataDecl = DataDecl
  { dataDeclCTypePragma :: Maybe Pragma,
    dataDeclHead :: BinderHead UnqualifiedName,
    dataDeclContext :: [Type],
    -- | Optional inline kind annotation (@:: Kind@) before @=@ or @where@
    dataDeclKind :: Maybe Type,
    dataDeclConstructors :: [DataConDecl],
    dataDeclDeriving :: [DerivingClause]
  }
  deriving (Eq, Show)

-- | A @newtype@ declaration.
-- Example: @newtype Identity a = Identity a@.
data NewtypeDecl = NewtypeDecl
  { newtypeDeclCTypePragma :: Maybe Pragma,
    newtypeDeclHead :: BinderHead UnqualifiedName,
    newtypeDeclContext :: [Type],
    -- | Optional inline kind annotation (@:: Kind@) before @=@ or @where@
    newtypeDeclKind :: Maybe Type,
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: [DerivingClause]
  }
  deriving (Eq, Show)

-- | A data constructor declaration.
data DataConDecl
  = -- | Metadata for the whole constructor declaration (typically a 'SourceSpan' via 'mkSpanAnnotation').
    DataConAnn Annotation DataConDecl
  | -- | What the type checker found for the whole constructor declaration.
    DataConTc TcInfo DataConDecl
  | -- | The resolution of built-in constructor syntax, such as @[]@.
    DataConResolved ResolutionResult DataConDecl
  | -- | @Just a@
    PrefixCon [TyVarBinder] [Type] UnqualifiedName [BangType]
  | -- | @a :+: b@
    InfixCon [TyVarBinder] [Type] BangType UnqualifiedName BangType
  | -- | @MkT { x :: Int, y :: Bool }@
    RecordCon [TyVarBinder] [Type] UnqualifiedName [FieldDecl]
  | -- | GADT-style constructor: @Con :: forall a. Ctx => Type@
    -- The list of names supports multiple constructors: @T1, T2 :: Type@
    GadtCon [ForallTelescope] [Type] [UnqualifiedName] GadtBody
  | -- | Tuple-style constructor: @()@, @(a,b)@, @(# #)@, @(# a,b #)@
    TupleCon [TyVarBinder] [Type] TupleFlavor [BangType]
  | -- | Unboxed sum constructor: @(# a | #)@, @(# | b #)@
    -- Fields: forall vars, context, position (1-based), total arity, field type
    UnboxedSumCon [TyVarBinder] [Type] Int Int BangType
  | -- | List constructor: @[]@
    ListCon [TyVarBinder] [Type]
  deriving (Eq, Show)

-- | Body of a GADT constructor after the @::@ and optional forall/context
-- Examples: @MkT :: a -> T a@ and @MkT :: { field :: a } -> T a@.
data GadtBody
  = -- | Prefix body: each argument is paired with its outgoing arrow kind.
    -- E.g. @a -> b %1 -> T a@ gives @[(a, ArrowUnrestricted), (b, ArrowLinear)]@ with result @T a@.
    GadtPrefixBody [(BangType, ArrowKind)] Type
  | -- | Record body: @{ field :: Type } -> T a@
    GadtRecordBody [FieldDecl] Type
  deriving (Eq, Show)

-- | A field or constructor argument type with strictness/laziness markers.
-- Examples: @a@, @!a@, @~a@, and @{-# UNPACK #-} !Int@.
data BangType = BangType
  { bangAnns :: [Annotation],
    bangPragmas :: [Pragma],
    bangStrict :: Bool,
    bangLazy :: Bool,
    bangType :: Type
  }
  deriving (Eq, Show)

-- | A record field declaration.
-- Example: @x, y :: Int@ in @data T = MkT { x, y :: Int }@.
data FieldDecl = FieldDecl
  { fieldAnns :: [Annotation],
    fieldNames :: [UnqualifiedName],
    -- | Optional multiplicity annotation, e.g. @x %'Many :: a@.
    fieldMultiplicity :: Maybe Type,
    fieldType :: BangType
  }
  deriving (Eq, Show)

-- | One @deriving@ clause.
-- Examples: @deriving Show@ and @deriving stock (Eq, Ord)@.
data DerivingClause = DerivingClause
  { derivingStrategy :: Maybe DerivingStrategy,
    derivingClasses :: Either Name [Type]
  }
  deriving (Eq, Show)

-- | An explicit deriving strategy.
data DerivingStrategy
  = -- | @deriving stock Show@
    DerivingStock
  | -- | @deriving newtype Num@
    DerivingNewtype
  | -- | @deriving anyclass C@
    DerivingAnyclass
  | -- | @deriving via Wrapper T@
    DerivingVia Type
  deriving (Eq, Show)

-- | A standalone deriving declaration.
-- Example: @deriving stock instance Show a => Show (T a)@.
data StandaloneDerivingDecl = StandaloneDerivingDecl
  { standaloneDerivingStrategy :: Maybe DerivingStrategy,
    standaloneDerivingPragmas :: [Pragma],
    standaloneDerivingWarning :: Maybe Pragma,
    standaloneDerivingForall :: [TyVarBinder],
    standaloneDerivingContext :: [Type],
    standaloneDerivingHead :: Type
  }
  deriving (Eq, Show)

-- | A class declaration.
-- Example: @class (Eq a) => C a where f :: a -> Bool@.
data ClassDecl = ClassDecl
  { classDeclContext :: Maybe [Type],
    classDeclHead :: BinderHead UnqualifiedName,
    classDeclFundeps :: [FunctionalDependency],
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Eq, Show)

-- | An item inside a class body.
data ClassDeclItem
  = -- | Metadata for the whole class item.
    ClassItemAnn Annotation ClassDeclItem
  | -- | What the type checker found for the whole class item.
    ClassItemTc TcInfo ClassDeclItem
  | -- | A resolve error for the whole class item.
    ClassItemError ResolveError ClassDeclItem
  | -- | @f :: a -> Bool@
    ClassItemTypeSig [BinderName] Type
  | -- | @default f :: Show a => a -> String@
    ClassItemDefaultSig BinderName Type
  | -- | @infixr 5 :+:@
    ClassItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | -- | A default method body such as @f x = True@.
    ClassItemDefault ValueDecl
  | -- | @type family F a@
    ClassItemTypeFamilyDecl TypeFamilyDecl
  | -- | @data family DF a@
    ClassItemDataFamilyDecl DataFamilyDecl
  | -- | @type F Int = Bool@
    ClassItemDefaultTypeInst TypeFamilyInst
  | -- | A pragma inside the class body, such as @{-# MINIMAL f #-}@.
    ClassItemPragma Pragma
  deriving (Eq, Show)

-- | An instance declaration.
-- Example: @instance Show a => Show [a] where show = ...@.
data InstanceDecl = InstanceDecl
  { instanceDeclPragmas :: [Pragma],
    instanceDeclWarning :: Maybe Pragma,
    instanceDeclForall :: [TyVarBinder],
    instanceDeclContext :: [Type],
    instanceDeclHead :: Type,
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Eq, Show)

-- | An item inside an instance body.
data InstanceDeclItem
  = -- | Metadata for the whole instance item (typically a 'SourceSpan' via 'mkSpanAnnotation').
    InstanceItemAnn Annotation InstanceDeclItem
  | -- | What the type checker found for the whole instance item.
    InstanceItemTc TcInfo InstanceDeclItem
  | -- | A resolve error for the whole instance item.
    InstanceItemError ResolveError InstanceDeclItem
  | -- | A method body such as @show x = ...@
    InstanceItemBind ValueDecl
  | -- | @show :: T -> String@
    InstanceItemTypeSig [BinderName] Type
  | -- | @infixr 5 :+:@
    InstanceItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName]
  | -- | @type F Int = Bool@
    InstanceItemTypeFamilyInst TypeFamilyInst
  | -- | @data instance DF Int = DFInt@
    InstanceItemDataFamilyInst DataFamilyInst
  | -- pragma inside instance body (e.g. {-# SPECIALIZE instance ... #-})
    InstanceItemPragma Pragma
  deriving (Eq, Show)

-- | A foreign import or export declaration.
-- Example: @foreign import ccall "puts" puts :: CString -> IO CInt@.
data ForeignDecl = ForeignDecl
  { foreignDirection :: ForeignDirection,
    foreignCallConv :: CallConv,
    foreignSafety :: Maybe ForeignSafety,
    foreignEntity :: ForeignEntitySpec,
    foreignName :: UnqualifiedName,
    foreignType :: Type
  }
  deriving (Eq, Show)

-- | An expression.
data Expr
  = -- | Metadata for the whole expression.
    EAnn Annotation Expr
  | -- | What the type checker found for the whole expression.
    ETc TcInfo Expr
  | -- | A syntax term or type that the desugaring of the expression uses,
    -- such as @fromInteger@ for a literal, or a resolve error.
    EResolved ResolutionResult Expr
  | -- | @x@ or @Data.List.map@
    EVar Name
  | -- | @?x@
    EImplicitParam Text
  | -- | @type T@ in a term position.
    ETypeSyntax TypeSyntaxForm Type
  | -- | @1@, @1#@, @1#Word8@
    EInt Integer NumericType Text
  | -- | @1.0@, @1.0#@, @1.0##@
    EFloat Rational FloatType Text
  | -- | @'x'@
    EChar Char Text
  | -- | @'x'#@
    ECharHash Char Text
  | -- | @"hello"@
    EString Text Text
  | -- | @"hello"#@
    EStringHash Text Text
  | -- | @#name@
    EOverloadedLabel Text Text
  | -- | @[qq| body |]@
    EQuasiQuote Text Text
  | -- | @if c then t else e@
    EIf Expr Expr Expr
  | -- | @if | g1 -> e1 | g2 -> e2@
    EMultiWayIf [GuardedRhs Expr]
  | -- | @\x y -> body@
    ELambdaPats [Pattern] Expr
  | -- | @\case { p -> e }@
    ELambdaCase [CaseAlt Expr]
  | -- | @\cases { p q -> e }@
    ELambdaCases [LambdaCaseAlt]
  | -- | @a + b@
    EInfix Expr Name Expr
  | -- | @f -> pat@ inside parentheses: the view-pattern arrow.
    -- The arrow is grammar, not an operator that a scope can bind, so it
    -- gets its own constructor.  'checkPattern' turns this into 'PView'.
    EViewPat Expr Expr
  | -- | @-x@
    ENegate Expr
  | -- | @(x +)@
    ESectionL Expr Name
  | -- | @(+ x)@
    ESectionR Name Expr
  | -- | @let x = 1 in x@
    ELetDecls [Decl] Expr
  | -- | @case x of { Just y -> y }@
    ECase Expr [CaseAlt Expr]
  | -- | @do { x <- mx; pure x }@, @mdo { ... }@, or @M.do { ... }@
    EDo [DoStmt Expr] DoFlavor
  | -- | @[x | x <- xs, x > 0]@
    EListComp Expr [CompStmt]
  | -- | @[x + y | x <- xs | y <- ys]@
    EListCompParallel Expr [[CompStmt]]
  | -- | @[1,3 .. 9]@
    EArithSeq ArithSeq
  | -- | @T { x = 1, y, .. }@
    ERecordCon Name [RecordField Expr] Bool -- Bool: wildcard present
  | -- | @r { x = 1 }@
    ERecordUpd Expr [RecordField Expr]
  | -- | @a.b@
    EGetField Expr Name -- a.b (OverloadedRecordDot)
  | -- | @(.b)@ or @(.b.c)@
    EGetFieldProjection [Name] -- (.b) or (.b.c) projection section (OverloadedRecordDot)
  | -- | @(expr :: ty)@
    ETypeSig Expr Type
  | -- | @(expr)@
    EParen Expr
  | -- | @[x, y]@
    EList [Expr]
  | -- | @(x, y)@, @(, y)@, or @(# x | #)@
    ETuple TupleFlavor [Maybe Expr]
  | -- | @(# | x | #)@
    EUnboxedSum Int Int Expr
  | -- | @f \@Type@
    ETypeApp Expr Type
  | -- | @f x@
    EApp Expr Expr
  | -- Template Haskell quotes

    -- | @[| expr |]@ or @[e| expr |]@
    ETHExpQuote Expr
  | -- | @[|| expr ||]@ or @[e|| expr ||]@
    ETHTypedQuote Expr
  | -- | @[d| decls |]@
    ETHDeclQuote [Decl]
  | -- | @[t| type |]@
    ETHTypeQuote Type
  | -- | @[p| pat |]@
    ETHPatQuote Pattern
  | -- | @'expr@ in the term namespace.
    ETHNameQuote Expr
  | -- | @''Type@ in the type namespace.
    ETHTypeNameQuote Type
  | -- Template Haskell splices

    -- | @$expr@ or @$(expr)@
    ETHSplice Expr
  | -- | @$$expr@ or @$$(expr)@
    ETHTypedSplice Expr
  | -- Arrow notation (Arrows extension)

    -- | @proc pat -> cmd@
    EProc Pattern Cmd
  | -- | @{-# SCC foo #-} expr@
    EPragma Pragma Expr
  deriving (Eq, Show)

-- | One @case@ or @\case@ alternative.
-- Example: @Just x -> x@.
data CaseAlt body = CaseAlt
  { caseAltAnns :: [Annotation],
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs body
  }
  deriving (Eq, Show)

-- | One multi-argument @\cases@ alternative.
-- Example: @Just x y -> x + y@.
data LambdaCaseAlt = LambdaCaseAlt
  { lambdaCaseAltAnns :: [Annotation],
    lambdaCaseAltPats :: [Pattern],
    lambdaCaseAltRhs :: Rhs Expr
  }
  deriving (Eq, Show)

-- | A statement inside a @do@ block.
data DoStmt body
  = -- | Metadata for the whole do-statement (typically a 'SourceSpan' via 'mkSpanAnnotation').
    DoAnn Annotation (DoStmt body)
  | -- | What the type checker found for the whole statement.
    DoTc TcInfo (DoStmt body)
  | -- | A method that the desugaring of the statement uses, such as @>>=@.
    DoResolved ResolutionResult (DoStmt body)
  | -- | The knot variables of a @rec@ segment of an @mdo@ block.
    DoRecKnot RecKnotVariables (DoStmt body)
  | -- | @pat <- expr@
    DoBind Pattern body
  | -- | @let decls@
    DoLetDecls [Decl]
  | -- | A bare expression statement such as @print x@.
    DoExpr body
  | -- | @rec { stmts }@
    DoRecStmt [DoStmt body] -- rec { stmts }
  deriving (Eq, Show)

-- | Arrow command type (used inside 'proc' expressions).
-- Commands mirror expressions but live in a separate namespace so the
-- pretty-printer knows not to parenthesise @do { … }@ as an infix LHS.
data Cmd
  = -- | Metadata for the whole command (typically a 'SourceSpan' via 'mkSpanAnnotation').
    CmdAnn Annotation Cmd
  | -- | @exp -\< exp@ or @exp -\<\< exp@
    CmdArrApp Expr ArrAppType Expr
  | -- | Command-level infix: @cmd1 op cmd2@
    CmdInfix Cmd Name Cmd
  | -- | @do { cstmts }@
    CmdDo [DoStmt Cmd]
  | -- | @if exp then cmd else cmd@
    CmdIf Expr Cmd Cmd
  | -- | @case exp of { calts }@
    CmdCase Expr [CaseAlt Cmd]
  | -- | @let decls in cmd@
    CmdLet [Decl] Cmd
  | -- | @\\pats -> cmd@
    CmdLam [Pattern] Cmd
  | -- | Command application: @cmd exp@
    CmdApp Cmd Expr
  | -- | Parenthesised command: @(cmd)@
    CmdPar Cmd
  deriving (Eq, Show)

-- | A statement inside a list comprehension.
data CompStmt
  = -- | Metadata for the whole comprehension statement (typically a 'SourceSpan' via 'mkSpanAnnotation').
    CompAnn Annotation CompStmt
  | -- | What the type checker found for the whole statement.
    CompTc TcInfo CompStmt
  | -- | @x <- xs@
    CompGen Pattern Expr
  | -- | @x > 0@
    CompGuard Expr
  | -- | @let y = f x@
    CompLetDecls [Decl]
  | -- | @then f@ (TransformListComp)
    CompThen Expr
  | -- | @then f by e@ (TransformListComp)
    CompThenBy Expr Expr
  | -- | @then group using f@ (TransformListComp)
    CompGroupUsing Expr
  | -- | @then group by e using f@ (TransformListComp)
    CompGroupByUsing Expr Expr
  deriving (Eq, Show)

-- | An arithmetic sequence expression.
data ArithSeq
  = -- | Metadata for the whole arithmetic sequence (typically a 'SourceSpan' via 'mkSpanAnnotation').
    ArithSeqAnn Annotation ArithSeq
  | -- | What the type checker found for the whole arithmetic sequence.
    ArithSeqTc TcInfo ArithSeq
  | -- | The enumeration method that the sequence uses.
    ArithSeqResolved ResolutionResult ArithSeq
  | -- | @[expr ..]@
    ArithSeqFrom Expr
  | -- | @[start, thenExpr ..]@
    ArithSeqFromThen Expr Expr
  | -- | @[start .. end]@
    ArithSeqFromTo Expr Expr
  | -- | @[start, thenExpr .. end]@
    ArithSeqFromThenTo Expr Expr Expr
  deriving (Eq, Show)

-- | Wrap a declaration with its checked type.
annotateDecl :: TcAnnotation -> Decl -> Decl
annotateDecl ann = DeclTc (InfoCheckedType ann)

-- | Cast a right-hand side onto the right type of its wanted equality.
annotateRhsCast :: TcType -> EvVar -> Rhs body -> Rhs body
annotateRhsCast ty evidence rhs =
  let info = InfoCast (PendingTcCastAnnotation ty evidence CastToRight)
   in case rhs of
        UnguardedRhs annotations infos body locals -> UnguardedRhs annotations (info : infos) body locals
        GuardedRhss annotations infos bodies locals -> GuardedRhss annotations (info : infos) bodies locals

-- | Cast an expression onto the left type of its wanted equality. An
-- application argument is checked as @expected ~ actual@, so a given
-- equality that proves it runs backwards for the argument itself.
annotateExprCast :: TcType -> EvVar -> Expr -> Expr
annotateExprCast ty evidence =
  ETc (InfoCast (PendingTcCastAnnotation ty evidence CastToLeft))

-- | Cast an expression onto the right type of its wanted equality. The
-- function of an application whose type is not syntactically an arrow is
-- equated as @actual ~ (argument -> result)@, so the proof runs forwards
-- for the function itself.
annotateFunCast :: TcType -> EvVar -> Expr -> Expr
annotateFunCast ty evidence =
  ETc (InfoCast (PendingTcCastAnnotation ty evidence CastToRight))

-- | Cast an annotated expression onto its signature type. The expression
-- is equated as @actual ~ signature@, so the proof runs forwards. A given
-- equality can be what proves it: with the given @d ~ Maybe x@ the
-- expression @Proxy :: Proxy d@ has the type @Proxy (Maybe x)@ where it
-- stands, and FC needs the cast onto @Proxy d@.
annotateSigCast :: TcType -> EvVar -> Expr -> Expr
annotateSigCast ty evidence =
  ETc (InfoCast (PendingTcCastAnnotation ty evidence CastToRight))

-- | Cast a part of an @if@ expression onto the type that the expression
-- needs there. The part is equated as @actual ~ needed@, so the proof
-- runs forwards. A given equality can prove it: with the given
-- @N t ~ n@, the branches of @if c then lower s else param s@ can have
-- the types @N t@ and @n@.
annotateBranchCast :: TcType -> EvVar -> Expr -> Expr
annotateBranchCast ty evidence =
  ETc (InfoCast (PendingTcCastAnnotation ty evidence CastToRight))

-- | Cast the sequencing method of a @do@ statement onto the type the
-- statement uses it at. The method is equated as
-- @method ~ (action -> continuation -> block)@, so the proof runs forwards
-- for the method. The desugarer applies the cast to the method occurrence,
-- because the statement has no expression node for it.
annotateDoStmtCast :: TcType -> EvVar -> DoStmt body -> DoStmt body
annotateDoStmtCast ty evidence =
  DoTc (InfoCast (PendingTcCastAnnotation ty evidence CastToRight))

-- | The pending type of an item of information, if it is one.
infoPending :: TcInfo -> Maybe PendingTcAnnotation
infoPending item =
  case item of
    InfoType pending -> Just pending
    _ -> Nothing

-- | Whether an item of information is a pending type.
infoIsPending :: TcInfo -> Bool
infoIsPending item =
  case item of
    InfoType _ -> True
    _ -> False

-- | Whether an item of information is a type, pending or final.
infoHasType :: TcInfo -> Bool
infoHasType item =
  case item of
    InfoType _ -> True
    InfoCheckedType _ -> True
    _ -> False
