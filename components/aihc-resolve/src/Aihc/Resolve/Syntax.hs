{-# LANGUAGE OverloadedStrings #-}

-- | The syntax tree that name resolution gives.
--
-- This tree is a copy of the syntax tree of "Aihc.Parser.Syntax". The copy
-- has the results of name resolution as fields with explicit types:
--
-- * Each 'Name' and 'UnqualifiedName' has a 'ResolutionResult' field.
-- * The syntax terms that desugared syntax uses, such as @fromInteger@ for
--   a literal and @>>=@ for a @do@ statement, are in wrapper constructors
--   such as 'EResolved' and 'DoResolved'.
-- * The knot variables of an @mdo@ segment are in 'DoRecKnot'.
-- * The resolve errors of whole declarations, imports and items are in
--   wrapper constructors and fields such as 'DeclError'.
--
-- The tree keeps the 'Annotation' wrappers of the parser tree. They carry
-- the source spans, and the type checker can add its own annotations.
--
-- Types that cannot contain a name, such as 'Literal' and 'Pragma', are the
-- types of the parser. This module exports them again.
--
-- The tree does not use the trees-that-grow idiom. Each phase that needs a
-- different tree has its own copy.
module Aihc.Resolve.Syntax
  ( -- * Resolutions
    ResolutionResult (..),
    ResolutionAnnotation (..),
    ResolveError (..),
    RecKnotVariables (..),

    -- * Names
    Name (..),
    UnqualifiedName (..),
    BinderName,
    OperatorName,
    mkName,
    mkUnqualifiedName,
    renderName,
    renderUnqualifiedName,

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

    -- * Expressions
    Expr (..),
    CaseAlt (..),
    LambdaCaseAlt (..),
    DoStmt (..),
    Cmd (..),
    CompStmt (..),
    ArithSeq (..),

    -- * Wrapper peeling
    peelDeclAnn,
    peelExprAnn,
    peelPatternAnn,
    peelTypeAnn,
    peelDoStmtAnn,

    -- * Types of the parser tree
    Annotation,
    ArrAppType (..),
    BuiltinCon (..),
    CallConv (..),
    DoFlavor (..),
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
    Literal (..),
    MatchHeadForm (..),
    NameType (..),
    NumericType (..),
    PatSynArgs (..),
    Pragma (..),
    PragmaType (..),
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
    Literal (..),
    MatchHeadForm (..),
    NameType (..),
    NumericType (..),
    PatSynArgs (..),
    Pragma (..),
    PragmaType (..),
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
import Aihc.Resolve.Types
  ( RecKnotVariables (..),
    ResolutionAnnotation (..),
    ResolutionResult (..),
    ResolveError (..),
  )
import Data.Data (Data)
import Data.Text (Text)

type BinderName = UnqualifiedName

type OperatorName = UnqualifiedName

-- | A name with no annotation and no resolution.
mkName :: Maybe Text -> NameType -> Text -> Name
mkName qualifier ty txt = Name qualifier ty txt [] Nothing

-- | An unqualified name with no annotation and no resolution.
mkUnqualifiedName :: NameType -> Text -> UnqualifiedName
mkUnqualifiedName ty txt = UnqualifiedName ty txt [] Nothing

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

-- | Remove the annotation and error wrappers of a declaration.
peelDeclAnn :: Decl -> Decl
peelDeclAnn decl =
  case decl of
    DeclAnn _ inner -> peelDeclAnn inner
    DeclError _ inner -> peelDeclAnn inner
    _ -> decl

-- | Remove the annotation and resolution wrappers of an expression.
peelExprAnn :: Expr -> Expr
peelExprAnn expr =
  case expr of
    EAnn _ inner -> peelExprAnn inner
    EResolved _ inner -> peelExprAnn inner
    _ -> expr

-- | Remove the annotation and resolution wrappers of a pattern.
peelPatternAnn :: Pattern -> Pattern
peelPatternAnn pat =
  case pat of
    PAnn _ inner -> peelPatternAnn inner
    PResolved _ inner -> peelPatternAnn inner
    _ -> pat

-- | Remove the annotation and resolution wrappers of a type.
peelTypeAnn :: Type -> Type
peelTypeAnn ty =
  case ty of
    TAnn _ inner -> peelTypeAnn inner
    TResolved _ inner -> peelTypeAnn inner
    _ -> ty

-- | Remove the annotation, resolution and knot wrappers of a statement.
peelDoStmtAnn :: DoStmt body -> DoStmt body
peelDoStmtAnn stmt =
  case stmt of
    DoAnn _ inner -> peelDoStmtAnn inner
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
    nameResolution :: Maybe ResolutionResult
  }
  deriving (Data, Eq, Show)

-- | An unqualified identifier or operator.
-- Examples: @map@, @Just@, @:+:@.
data UnqualifiedName = UnqualifiedName
  { unqualifiedNameType :: NameType,
    unqualifiedNameText :: Text,
    unqualifiedNameAnns :: [Annotation],
    -- | What the resolver found for this binder or occurrence. 'Nothing'
    -- when the resolver does not resolve a name in this position.
    unqualifiedNameResolution :: Maybe ResolutionResult
  }
  deriving (Data, Eq, Show)

-- | A whole Haskell module.
-- Example: @module M (x) where import Data.List; x = 1@.
data Module = Module
  { moduleAnns :: [Annotation],
    moduleHead :: Maybe ModuleHead,
    moduleLanguagePragmas :: [ExtensionSetting],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Data, Eq, Show)

-- | The optional @module ... where@ header.
-- Example: @module M (x, type T) where@.
data ModuleHead = ModuleHead
  { moduleHeadAnns :: [Annotation],
    moduleHeadName :: Text,
    moduleHeadWarningPragma :: Maybe Pragma,
    moduleHeadExports :: Maybe [ExportSpec]
  }
  deriving (Data, Eq, Show)

-- | A member bundled with a parent import or export.
-- Example: @Just@ in @Maybe(Just, Nothing)@.
data IEBundledMember = IEBundledMember
  { ieBundledMemberNamespace :: Maybe IEBundledNamespace,
    ieBundledMemberName :: Name
  }
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | An optional import list or hiding list.
-- Examples: @(map, foldr)@ and @hiding (map)@.
data ImportSpec = ImportSpec
  { importSpecAnns :: [Annotation],
    importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | A top-level declaration.
data Decl
  = -- | Metadata for the whole declaration.
    DeclAnn Annotation Decl
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
  deriving (Data, Eq, Show)

-- | One rewrite rule of a @RULES@ pragma.
-- Example: @"map/map" [2] forall f g xs. map f (map g xs) = map (f . g) xs@.
data RuleDecl = RuleDecl
  { ruleAnns :: [Annotation],
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
  deriving (Data, Eq, Show)

-- | One pattern variable of a rule, bound by @forall@.
-- Examples: @xs@ and @(g :: forall b. (a -> b -> b) -> b -> b)@.
data RuleBinder = RuleBinder
  { ruleBinderAnns :: [Annotation],
    ruleBinderName :: UnqualifiedName,
    ruleBinderType :: Maybe Type
  }
  deriving (Data, Eq, Show)

-- | Multiplicity tag on a let/where pattern binding.
-- Corresponds to @x = e@ (no tag), @%1 x = e@ (linear), @%m x = e@ (explicit).
data MultiplicityTag
  = -- | @x = e@
    NoMultiplicityTag
  | -- | @%1 x = e@
    LinearMultiplicityTag
  | -- | @%m x = e@
    ExplicitMultiplicityTag Type
  deriving (Data, Eq, Show)

-- | A value binding.
data ValueDecl
  = -- | @f x y = body@
    FunctionBind BinderName [Match]
  | -- | @pat = body@ or @%1 pat = body@
    PatternBind MultiplicityTag Pattern (Rhs Expr)
  deriving (Data, Eq, Show)

-- | One equation of a function or pattern synonym.
-- Examples: @f x = x@ and @x :+: y = z@.
data Match = Match
  { matchAnns :: [Annotation],
    matchHeadForm :: MatchHeadForm,
    matchPats :: [Pattern],
    matchRhs :: Rhs Expr
  }
  deriving (Data, Eq, Show)

-- | Pattern synonym declaration direction.
data PatSynDir
  = -- | @pattern P x <- pat@
    PatSynUnidirectional
  | -- | @pattern P x = pat@
    PatSynBidirectional
  | -- | @pattern P x <- pat where P x = expr@
    PatSynExplicitBidirectional [Match]
  deriving (Data, Eq, Show)

-- | Pattern synonym declaration.
-- Example: @pattern JustOne x = Just [x]@.
data PatSynDecl = PatSynDecl
  { patSynDeclName :: UnqualifiedName,
    patSynDeclArgs :: PatSynArgs,
    patSynDeclPat :: Pattern,
    patSynDeclDir :: PatSynDir
  }
  deriving (Data, Eq, Show)

-- | The right-hand side of a binding, case alternative, or lambda case.
data Rhs body
  = -- | @= body@ with an optional @where@ clause.
    UnguardedRhs [Annotation] body (Maybe [Decl])
  | -- | Guarded equations such as @| x > 0 = body@.
    GuardedRhss [Annotation] [GuardedRhs body] (Maybe [Decl])
  deriving (Data, Eq, Show)

-- | One guarded branch.
-- Example: @| x > 0 = x@.
data GuardedRhs body = GuardedRhs
  { guardedRhsAnns :: [Annotation],
    guardedRhsGuards :: [GuardQualifier],
    guardedRhsBody :: body
  }
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | One record field occurrence.
-- Examples: @x = 1@ in @T { x = 1 }@ and @x@ in @T { x }@.
data RecordField a = RecordField
  { recordFieldName :: Name,
    recordFieldValue :: a,
    recordFieldPun :: Bool
  }
  deriving (Data, Eq, Show)

-- | A pattern.
data Pattern
  = -- | Metadata for the whole pattern.
    PAnn Annotation Pattern
  | -- | A syntax term or type that the match of the pattern uses, such as
    -- @fromInteger@ and @==@ for a literal pattern, or a resolve error.
    PResolved ResolutionResult Pattern
  | -- | @x@
    PVar UnqualifiedName
  | -- | @\@a@ in a required type argument pattern.
    PTypeBinder TyVarBinder
  | -- | @type T@ in a term-level pattern position.
    PTypeSyntax TypeSyntaxForm Type
  | -- | @_@
    PWildcard
  | -- | @1@ or @'x'@
    PLit Literal
  | -- | @[p| body |]@
    PQuasiQuote Text Text
  | -- | @(p1, p2)@ or @(# p1, p2 #)@
    PTuple TupleFlavor [Pattern]
  | -- | @(# | p | #)@
    PUnboxedSum Int Int Pattern
  | -- | @[p1, p2]@
    PList [Pattern]
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
    PNegLit Literal
  | -- | @n+1@, an n+k pattern.  It needs the @NPlusKPatterns@ extension.
    -- The literal is always a non-negative integer literal.
    PNPlusK UnqualifiedName Literal
  | -- | @(pat)@
    PParen Pattern
  | -- | @T { x = p, y, .. }@
    PRecord Name [RecordField Pattern] Bool -- Bool: wildcard present
  | -- | @(pat :: ty)@
    PTypeSig Pattern Type
  | -- | @$pat@ or @$(pat)@
    PSplice Expr
  -- \$pat or $(pat) (TH pattern splice)
  deriving (Data, Eq, Show)

-- | The binders introduced by a @forall@.
-- Example: @forall a b. a -> b -> a@.
data ForallTelescope = ForallTelescope
  { forallTelescopeVisibility :: ForallVis,
    forallTelescopeBinders :: [TyVarBinder]
  }
  deriving (Data, Eq, Show)

-- | The kind of a function arrow.
-- @ArrowUnrestricted@ is the ordinary @->@.
-- @ArrowLinear@ is @%1 ->@ (or its Unicode equivalent @⊸@).
-- @ArrowExplicit ty@ is @%m ->@ for an arbitrary multiplicity type @m@.
data ArrowKind
  = ArrowUnrestricted
  | ArrowLinear
  | ArrowExplicit Type
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | The head of a declaration that binds a type constructor or class.
data BinderHead name
  = -- | @T a b@
    PrefixBinderHead name [TyVarBinder]
  | -- | @a :+: b@
    InfixBinderHead TyVarBinder name TyVarBinder [TyVarBinder]
  deriving (Data, Eq, Show)

-- | A @type role@ declaration.
-- Example: @type role T nominal representational@.
data RoleAnnotation = RoleAnnotation
  { roleAnnotationName :: UnqualifiedName,
    roleAnnotationRoles :: [Role]
  }
  deriving (Data, Eq, Show)

-- | A @type@ synonym declaration.
-- Example: @type Pair a = (a, a)@.
data TypeSynDecl = TypeSynDecl
  { typeSynHead :: BinderHead UnqualifiedName,
    typeSynBody :: Type
  }
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | The result signature on a type family head.
data TypeFamilyResultSig
  = -- | @type family F a :: Type@
    TypeFamilyKindSig Type
  | -- | @type family F a = r@
    TypeFamilyTyVarSig TyVarBinder
  | -- | @type family F a = r | r -> a@
    TypeFamilyInjectiveSig TyVarBinder TypeFamilyInjectivity
  deriving (Data, Eq, Show)

-- | One equation in a closed type family: @[forall binders.] LhsType = RhsType@
-- Example: @forall a. F [a] = Maybe a@.
data TypeFamilyEq = TypeFamilyEq
  { typeFamilyEqAnns :: [Annotation],
    typeFamilyEqForall :: [TyVarBinder],
    typeFamilyEqHeadForm :: TypeHeadForm,
    typeFamilyEqLhs :: Type,
    typeFamilyEqRhs :: Type
  }
  deriving (Data, Eq, Show)

-- | Data family declaration (standalone or associated in a class body).
-- Example: @data family DF a :: Type@.
data DataFamilyDecl = DataFamilyDecl
  { dataFamilyDeclHead :: BinderHead UnqualifiedName,
    -- | Optional result kind annotation (@:: Kind@)
    dataFamilyDeclKind :: Maybe Type
  }
  deriving (Data, Eq, Show)

-- | Type family instance: @type [instance] [forall binders.] LhsType = RhsType@
-- Example: @type instance F Int = Bool@.
data TypeFamilyInst = TypeFamilyInst
  { typeFamilyInstForall :: [TyVarBinder],
    typeFamilyInstHeadForm :: TypeHeadForm,
    typeFamilyInstLhs :: Type,
    typeFamilyInstRhs :: Type
  }
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | A data constructor declaration.
data DataConDecl
  = -- | Metadata for the whole constructor declaration (typically a 'SourceSpan' via 'mkSpanAnnotation').
    DataConAnn Annotation DataConDecl
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
  deriving (Data, Eq, Show)

-- | Body of a GADT constructor after the @::@ and optional forall/context
-- Examples: @MkT :: a -> T a@ and @MkT :: { field :: a } -> T a@.
data GadtBody
  = -- | Prefix body: each argument is paired with its outgoing arrow kind.
    -- E.g. @a -> b %1 -> T a@ gives @[(a, ArrowUnrestricted), (b, ArrowLinear)]@ with result @T a@.
    GadtPrefixBody [(BangType, ArrowKind)] Type
  | -- | Record body: @{ field :: Type } -> T a@
    GadtRecordBody [FieldDecl] Type
  deriving (Data, Eq, Show)

-- | A field or constructor argument type with strictness/laziness markers.
-- Examples: @a@, @!a@, @~a@, and @{-# UNPACK #-} !Int@.
data BangType = BangType
  { bangAnns :: [Annotation],
    bangPragmas :: [Pragma],
    bangStrict :: Bool,
    bangLazy :: Bool,
    bangType :: Type
  }
  deriving (Data, Eq, Show)

-- | A record field declaration.
-- Example: @x, y :: Int@ in @data T = MkT { x, y :: Int }@.
data FieldDecl = FieldDecl
  { fieldAnns :: [Annotation],
    fieldNames :: [UnqualifiedName],
    -- | Optional multiplicity annotation, e.g. @x %'Many :: a@.
    fieldMultiplicity :: Maybe Type,
    fieldType :: BangType
  }
  deriving (Data, Eq, Show)

-- | One @deriving@ clause.
-- Examples: @deriving Show@ and @deriving stock (Eq, Ord)@.
data DerivingClause = DerivingClause
  { derivingStrategy :: Maybe DerivingStrategy,
    derivingClasses :: Either Name [Type]
  }
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | A class declaration.
-- Example: @class (Eq a) => C a where f :: a -> Bool@.
data ClassDecl = ClassDecl
  { classDeclContext :: Maybe [Type],
    classDeclHead :: BinderHead UnqualifiedName,
    classDeclFundeps :: [FunctionalDependency],
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Data, Eq, Show)

-- | An item inside a class body.
data ClassDeclItem
  = -- | Metadata for the whole class item.
    ClassItemAnn Annotation ClassDeclItem
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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | An item inside an instance body.
data InstanceDeclItem
  = -- | Metadata for the whole instance item (typically a 'SourceSpan' via 'mkSpanAnnotation').
    InstanceItemAnn Annotation InstanceDeclItem
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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | An expression.
data Expr
  = -- | Metadata for the whole expression.
    EAnn Annotation Expr
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
  deriving (Data, Eq, Show)

-- | One @case@ or @\case@ alternative.
-- Example: @Just x -> x@.
data CaseAlt body = CaseAlt
  { caseAltAnns :: [Annotation],
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs body
  }
  deriving (Data, Eq, Show)

-- | One multi-argument @\cases@ alternative.
-- Example: @Just x y -> x + y@.
data LambdaCaseAlt = LambdaCaseAlt
  { lambdaCaseAltAnns :: [Annotation],
    lambdaCaseAltPats :: [Pattern],
    lambdaCaseAltRhs :: Rhs Expr
  }
  deriving (Data, Eq, Show)

-- | A statement inside a @do@ block.
data DoStmt body
  = -- | Metadata for the whole do-statement (typically a 'SourceSpan' via 'mkSpanAnnotation').
    DoAnn Annotation (DoStmt body)
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
  deriving (Data, Eq, Show)

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
  deriving (Data, Eq, Show)

-- | A statement inside a list comprehension.
data CompStmt
  = -- | Metadata for the whole comprehension statement (typically a 'SourceSpan' via 'mkSpanAnnotation').
    CompAnn Annotation CompStmt
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
  deriving (Data, Eq, Show)

-- | An arithmetic sequence expression.
data ArithSeq
  = -- | Metadata for the whole arithmetic sequence (typically a 'SourceSpan' via 'mkSpanAnnotation').
    ArithSeqAnn Annotation ArithSeq
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
  deriving (Data, Eq, Show)
