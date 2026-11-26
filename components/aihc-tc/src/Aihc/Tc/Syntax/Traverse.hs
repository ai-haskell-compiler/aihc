{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- | A hand-written walk over the syntax tree of "Aihc.Tc.Syntax". The
-- walk visits the type checker information through the hooks
-- 'walkTcAnnotation', 'walkCheckedType', and 'walkFacts'.
--
-- The type checker reads and rewrites the annotations of a whole module
-- several times, and it rewrites some nodes.
-- A generic "Data.Data" walk does this work, but it visits every field of
-- every node and does a runtime type test at each one. This module walks
-- the same tree with one case for each constructor. The walk visits the
-- nodes in source order: the fields of a constructor from left to right,
-- and the elements of a list from first to last.
--
-- A 'Walk' holds the hooks of one walk. Each hook gets the walk of the
-- children of a node and the node itself. A hook that rewrites a node
-- bottom-up walks the children first and then rewrites the result. A hook
-- that only collects can look at the node and then walk the children.
--
-- The pattern matches are exhaustive without a wildcard. A new syntax
-- constructor therefore fails the build until its case is added.
module Aihc.Tc.Syntax.Traverse
  ( Walkable (..),
    Walk (..),
    idWalk,
    FactWalk (..),
    idFactWalk,
    collectTcAnnotations,
    traverseAnnotations,
    annotationList,
    collectAnnotations,
    collectResolutions,
    Collect,
    collected,
    runCollect,
  )
where

import Aihc.Tc.Annotations (TcAnnotation, TcCastAnnotation, TcClassAnnotation, TcDerivingPlan, TcInstanceAnnotation, TcInstanceMethodAnnotation, TcMatchArityCast, TcRecStmt (..))
import Aihc.Tc.Env (DataFamilyInstanceInfo, TypeFamilyInstanceInfo)
import Aihc.Tc.Syntax
import Aihc.Tc.Types (TcType)
import Control.Monad (join)
import Data.Maybe (catMaybes)

-- | The hooks of one walk. 'idWalk' walks every node and leaves it as it
-- is. A caller sets the hooks it needs.
data Walk f = Walk
  { -- | Every annotation, where it is attached.
    walkAnnotation :: Annotation -> f Annotation,
    -- | Every resolution result, where it is attached: on a name or on a
    -- binder.
    walkResolution :: ResolutionResult -> f ResolutionResult,
    -- | Every type annotation that a node holds in a typed field or in a
    -- wrapper, where it is attached.
    walkTcAnnotation :: TcAnnotation -> f TcAnnotation,
    -- | Every checked type that a node holds in a typed field of its own,
    -- where it is attached: on a rule, a rule binder, or a constructor
    -- declaration.
    walkCheckedType :: TcType -> f TcType,
    -- | The other information that the type checker puts in typed fields.
    walkFacts :: FactWalk f,
    -- | Every annotation list of a node that holds its annotations in a
    -- list, such as a name or a module. The argument walks the elements.
    walkAnnotationList :: ([Annotation] -> f [Annotation]) -> [Annotation] -> f [Annotation],
    walkName :: (Name -> f Name) -> Name -> f Name,
    walkExpr :: (Expr -> f Expr) -> Expr -> f Expr,
    walkPattern :: (Pattern -> f Pattern) -> Pattern -> f Pattern,
    walkType :: (Type -> f Type) -> Type -> f Type,
    walkDecl :: (Decl -> f Decl) -> Decl -> f Decl,
    walkDataConDecl :: (DataConDecl -> f DataConDecl) -> DataConDecl -> f DataConDecl,
    walkLiteral :: (Literal -> f Literal) -> Literal -> f Literal,
    walkGuardQualifier :: (GuardQualifier -> f GuardQualifier) -> GuardQualifier -> f GuardQualifier,
    walkDoStmt :: forall body. (Walkable body) => (DoStmt body -> f (DoStmt body)) -> DoStmt body -> f (DoStmt body),
    walkCompStmt :: (CompStmt -> f CompStmt) -> CompStmt -> f CompStmt,
    walkArithSeq :: (ArithSeq -> f ArithSeq) -> ArithSeq -> f ArithSeq,
    walkClassDeclItem :: (ClassDeclItem -> f ClassDeclItem) -> ClassDeclItem -> f ClassDeclItem,
    walkInstanceDeclItem :: (InstanceDeclItem -> f InstanceDeclItem) -> InstanceDeclItem -> f InstanceDeclItem,
    walkCmd :: (Cmd -> f Cmd) -> Cmd -> f Cmd,
    walkExportSpec :: (ExportSpec -> f ExportSpec) -> ExportSpec -> f ExportSpec,
    walkImportItem :: (ImportItem -> f ImportItem) -> ImportItem -> f ImportItem
  }

-- | The hooks for the information that the type checker puts in typed
-- fields, other than type annotations. 'idFactWalk' leaves each item as it
-- is. The finalization pass of the type checker sets these hooks.
data FactWalk f = FactWalk
  { -- | Every cast, where it is attached: in a wrapper, on a name, or on a
    -- right-hand side. 'Nothing' removes the cast.
    factCast :: TcCastAnnotation -> f (Maybe TcCastAnnotation),
    -- | The arity cast of an equation. 'Nothing' removes the cast.
    factMatchArityCast :: TcMatchArityCast -> f (Maybe TcMatchArityCast),
    factTypeFamilyInstance :: TypeFamilyInstanceInfo -> f TypeFamilyInstanceInfo,
    factDataFamilyInstance :: DataFamilyInstanceInfo -> f DataFamilyInstanceInfo,
    factClass :: TcClassAnnotation -> f TcClassAnnotation,
    factDerivingPlan :: TcDerivingPlan -> f TcDerivingPlan,
    factInstance :: TcInstanceAnnotation -> f TcInstanceAnnotation,
    factInstanceMethod :: TcInstanceMethodAnnotation -> f TcInstanceMethodAnnotation,
    -- | The checked equations of a pattern synonym. The argument walks the
    -- equations. 'idFactWalk' does not walk them, because they are not in
    -- the source text.
    factPatSynEquations :: (PatSynEquations -> f PatSynEquations) -> PatSynEquations -> f PatSynEquations
  }

-- | The hooks that change no information and do not walk the equations of
-- a pattern synonym.
idFactWalk :: (Applicative f) => FactWalk f
idFactWalk =
  FactWalk
    { factCast = pure . Just,
      factMatchArityCast = pure . Just,
      factTypeFamilyInstance = pure,
      factDataFamilyInstance = pure,
      factClass = pure,
      factDerivingPlan = pure,
      factInstance = pure,
      factInstanceMethod = pure,
      factPatSynEquations = \_ equations -> pure equations
    }

-- | The walk that changes nothing.
idWalk :: (Applicative f) => Walk f
idWalk =
  Walk
    { walkAnnotation = pure,
      walkResolution = pure,
      walkTcAnnotation = pure,
      walkCheckedType = pure,
      walkFacts = idFactWalk,
      walkAnnotationList = id,
      walkName = id,
      walkExpr = id,
      walkPattern = id,
      walkType = id,
      walkDecl = id,
      walkDataConDecl = id,
      walkLiteral = id,
      walkGuardQualifier = id,
      walkDoStmt = id,
      walkCompStmt = id,
      walkArithSeq = id,
      walkClassDeclItem = id,
      walkInstanceDeclItem = id,
      walkCmd = id,
      walkExportSpec = id,
      walkImportItem = id
    }

-- | Syntax that a 'Walk' can visit.
class Walkable a where
  -- | Apply the hooks of a walk to every node, in source order.
  walk :: (Applicative f) => Walk f -> a -> f a

-- | The annotation list of one node.
walkAnns :: (Applicative f) => Walk f -> [Annotation] -> f [Annotation]
walkAnns w = walkAnnotationList w (traverse (walkAnnotation w))

-- | The type annotations of a piece of syntax, in source order.
collectTcAnnotations :: (Walkable a) => a -> [TcAnnotation]
collectTcAnnotations = runCollect (walk idWalk {walkTcAnnotation = step})
  where
    step item = Collect (Just (item :))

-- | The information of an expression wrapper. 'Nothing' removes the
-- wrapper.
walkExprTc :: (Applicative f) => Walk f -> ExprTc -> f (Maybe ExprTc)
walkExprTc w item =
  case item of
    ExprInst annotation -> Just . ExprInst <$> walkTcAnnotation w annotation
    ExprAbs annotation -> Just . ExprAbs <$> walkTcAnnotation w annotation
    ExprCast cast -> fmap ExprCast <$> factCast (walkFacts w) cast

-- | The information of a pattern wrapper. 'Nothing' removes the wrapper.
walkPatternTc :: (Applicative f) => Walk f -> PatternTc -> f (Maybe PatternTc)
walkPatternTc w item =
  case item of
    PatternType annotation -> Just . PatternType <$> walkTcAnnotation w annotation
    PatternCast cast -> fmap PatternCast <$> factCast (walkFacts w) cast
    PatternInstantiation -> pure (Just item)

-- | Wrap a node with its information, if the information stays.
wrapTc :: (item -> node -> node) -> Maybe item -> node -> node
wrapTc wrap item node = maybe node (`wrap` node) item

-- | The casts of one node. A cast that the hook removes goes away.
walkCasts :: (Applicative f) => Walk f -> [TcCastAnnotation] -> f [TcCastAnnotation]
walkCasts w casts = catMaybes <$> traverse (factCast (walkFacts w)) casts

-- | An item that its hook can remove, if the node has one.
walkRemovable :: (Applicative f) => (item -> f (Maybe item)) -> Maybe item -> f (Maybe item)
walkRemovable hook item = join <$> traverse hook item

-- | The annotation of a declaration wrapper, a name, or a binder.
walkDeclTc :: (Applicative f) => Walk f -> TcAnnotation -> f TcAnnotation
walkDeclTc = walkTcAnnotation

-- | The annotations of a @rec@ statement: the tuple annotation, then the
-- occurrence annotation of each knot variable.
walkRecStmt :: (Applicative f) => Walk f -> TcRecStmt TcAnnotation -> f (TcRecStmt TcAnnotation)
walkRecStmt w (TcRecStmt tuple occurrences) =
  TcRecStmt <$> walkTcAnnotation w tuple <*> traverse (\(key, occurrence) -> (key,) <$> walkTcAnnotation w occurrence) occurrences

-- | The checked type of one node, if it has one.
walkCheckedTypeOf :: (Applicative f) => Walk f -> Maybe TcType -> f (Maybe TcType)
walkCheckedTypeOf w = traverse (walkCheckedType w)

-- | The resolution result of a name or a binder, if it has one.
walkMaybeResolution :: (Applicative f) => Walk f -> Maybe ResolutionResult -> f (Maybe ResolutionResult)
walkMaybeResolution w = traverse (walkResolution w)

-- | The values that one function selects from the resolution results of a
-- piece of syntax, in source order.
collectResolutions :: (Walkable a) => (ResolutionResult -> Maybe r) -> a -> [r]
collectResolutions select = runCollect (walk idWalk {walkResolution = step})
  where
    step result = Collect (fmap (:) (select result))

-- | Apply an effect to every annotation, in source order.
traverseAnnotations :: (Walkable a, Applicative f) => (Annotation -> f Annotation) -> a -> f a
traverseAnnotations f = walk idWalk {walkAnnotation = f}

-- | Every annotation of a piece of syntax, in source order.
annotationList :: (Walkable a) => a -> [Annotation]
annotationList = collectAnnotations Just

-- | The values that one function selects from the annotations of a piece
-- of syntax, in source order. An annotation that the function rejects adds
-- no value to the result.
--
-- Most callers keep few annotations of a module, or none. The walk thus
-- builds the result as a difference list and appends no lists. A walk that
-- keeps no annotation of a module allocates nothing.
collectAnnotations :: (Walkable a) => (Annotation -> Maybe r) -> a -> [r]
collectAnnotations select = runCollect (traverseAnnotations step)
  where
    step ann = Collect (fmap (:) (select ann))

-- | The applicative that a collecting walk runs in. It keeps the collected
-- values as a difference list and drops the syntax. 'Nothing' is a subtree
-- that gives no value. A walk that finds no value in a whole module thus
-- allocates nothing, because the newtype and the 'Nothing' cost no memory.
newtype Collect r a = Collect (Maybe ([r] -> [r]))

-- | Collect these values at the current node.
collected :: [r] -> Collect r a
collected [] = Collect Nothing
collected values = Collect (Just (values ++))

-- | The values that a collecting walk of a piece of syntax gives, in
-- source order.
runCollect :: (a -> Collect r a) -> a -> [r]
runCollect run value =
  case run value of
    Collect Nothing -> []
    Collect (Just build) -> build []

instance Functor (Collect r) where
  fmap _ (Collect build) = Collect build

instance Applicative (Collect r) where
  pure _ = Collect Nothing
  Collect left <*> Collect right = Collect (appendCollected left right)
  liftA2 _ (Collect left) (Collect right) = Collect (appendCollected left right)

-- | Put the values of the first subtree in front of the values of the
-- second subtree. The walk visits the first subtree first.
appendCollected :: Maybe ([r] -> [r]) -> Maybe ([r] -> [r]) -> Maybe ([r] -> [r])
appendCollected Nothing right = right
appendCollected left Nothing = left
appendCollected (Just left) (Just right) = Just (left . right)

instance Walkable Annotation where
  walk = walkAnnotation

instance (Walkable a) => Walkable [a] where
  walk w = traverse (walk w)

instance (Walkable a) => Walkable (Maybe a) where
  walk w = traverse (walk w)

instance (Walkable a, Walkable b) => Walkable (Either a b) where
  walk w = either (fmap Left . walk w) (fmap Right . walk w)

instance (Walkable a, Walkable b) => Walkable (a, b) where
  walk w (left, right) = (,) <$> walk w left <*> walk w right

-- Names

instance Walkable Name where
  walk w = walkName w $ \name ->
    (\anns resolution annotation cast -> name {nameAnns = anns, nameResolution = resolution, nameTc = annotation, nameCast = cast})
      <$> walkAnns w (nameAnns name)
      <*> walkMaybeResolution w (nameResolution name)
      <*> traverse (walkDeclTc w) (nameTc name)
      <*> walkRemovable (factCast (walkFacts w)) (nameCast name)

instance Walkable UnqualifiedName where
  walk w name =
    (\anns resolution annotation -> name {unqualifiedNameAnns = anns, unqualifiedNameResolution = resolution, unqualifiedNameTc = annotation})
      <$> walkAnns w (unqualifiedNameAnns name)
      <*> walkMaybeResolution w (unqualifiedNameResolution name)
      <*> traverse (walkDeclTc w) (unqualifiedNameTc name)

-- Modules, exports, and imports

instance Walkable Module where
  walk w (Module anns identity modHead pragmas imports decls diagnostics) =
    Module
      <$> walkAnns w anns
      <*> pure identity
      <*> walk w modHead
      <*> pure pragmas
      <*> walk w imports
      <*> walk w decls
      <*> pure diagnostics

instance Walkable ModuleHead where
  walk w (ModuleHead anns name warning exports) =
    ModuleHead <$> walkAnns w anns <*> pure name <*> pure warning <*> walk w exports

instance Walkable IEBundledMember where
  walk w (IEBundledMember namespace name) =
    IEBundledMember namespace <$> walk w name

instance Walkable ExportSpec where
  walk w = walkExportSpec w $ \case
    ExportModule pragma name -> pure (ExportModule pragma name)
    ExportVar pragma namespace name -> ExportVar pragma namespace <$> walk w name
    ExportAbs pragma namespace name -> ExportAbs pragma namespace <$> walk w name
    ExportAll pragma namespace name -> ExportAll pragma namespace <$> walk w name
    ExportWith pragma namespace name members ->
      ExportWith pragma namespace <$> walk w name <*> walk w members
    ExportWithAll pragma namespace name position members ->
      ExportWithAll pragma namespace <$> walk w name <*> pure position <*> walk w members
    ExportAnn ann inner -> ExportAnn <$> walkAnnotation w ann <*> walk w inner

instance Walkable ImportDecl where
  walk w decl =
    (\anns spec -> decl {importDeclAnns = anns, importDeclSpec = spec})
      <$> walkAnns w (importDeclAnns decl)
      <*> walk w (importDeclSpec decl)

instance Walkable ImportSpec where
  walk w (ImportSpec anns hiding items) =
    ImportSpec <$> walkAnns w anns <*> pure hiding <*> walk w items

instance Walkable ImportItem where
  walk w = walkImportItem w $ \case
    ImportItemVar namespace name -> ImportItemVar namespace <$> walk w name
    ImportItemAbs namespace name -> ImportItemAbs namespace <$> walk w name
    ImportItemAll namespace name -> ImportItemAll namespace <$> walk w name
    ImportItemWith namespace name members ->
      ImportItemWith namespace <$> walk w name <*> walk w members
    ImportItemAllWith namespace name position members ->
      ImportItemAllWith namespace <$> walk w name <*> pure position <*> walk w members
    ImportAnn ann inner -> ImportAnn <$> walkAnnotation w ann <*> walk w inner
    ImportItemError err inner -> ImportItemError err <$> walk w inner

-- Declarations

instance Walkable Decl where
  walk w = walkDecl w $ \case
    DeclAnn ann inner -> DeclAnn <$> walkAnnotation w ann <*> walk w inner
    DeclTc info inner -> DeclTc <$> walkDeclTc w info <*> walk w inner
    DeclError err inner -> DeclError err <$> walk w inner
    DeclValue value -> DeclValue <$> walk w value
    DeclImplicitParam name expr decls ->
      DeclImplicitParam name <$> walk w expr <*> walk w decls
    DeclTypeSig names ty -> DeclTypeSig <$> walk w names <*> walk w ty
    DeclPatSyn patSyn -> DeclPatSyn <$> walk w patSyn
    DeclPatSynSig names ty -> DeclPatSynSig <$> walk w names <*> walk w ty
    DeclStandaloneKindSig name ty -> DeclStandaloneKindSig <$> walk w name <*> walk w ty
    DeclFixity assoc namespace precedence operators ->
      DeclFixity assoc namespace precedence <$> walk w operators
    DeclRoleAnnotation roles -> DeclRoleAnnotation <$> walk w roles
    DeclTypeSyn synonym -> DeclTypeSyn <$> walk w synonym
    DeclTypeData dataDecl -> DeclTypeData <$> walk w dataDecl
    DeclData dataDecl -> DeclData <$> walk w dataDecl
    DeclNewtype newtypeDecl -> DeclNewtype <$> walk w newtypeDecl
    DeclClass classDecl -> DeclClass <$> walk w classDecl
    DeclInstance instanceDecl -> DeclInstance <$> walk w instanceDecl
    DeclStandaloneDeriving derivingDecl -> DeclStandaloneDeriving <$> walk w derivingDecl
    DeclDefault types -> DeclDefault <$> walk w types
    DeclSplice expr -> DeclSplice <$> walk w expr
    DeclForeign foreignDecl -> DeclForeign <$> walk w foreignDecl
    DeclTypeFamilyDecl familyDecl -> DeclTypeFamilyDecl <$> walk w familyDecl
    DeclDataFamilyDecl familyDecl -> DeclDataFamilyDecl <$> walk w familyDecl
    DeclTypeFamilyInst familyInst -> DeclTypeFamilyInst <$> walk w familyInst
    DeclDataFamilyInst familyInst -> DeclDataFamilyInst <$> walk w familyInst
    DeclPragma pragma -> pure (DeclPragma pragma)
    DeclRules rules -> DeclRules <$> walk w rules

instance Walkable RuleDecl where
  walk w rule =
    ( \anns infos typeBinders binders lhs rhs ->
        rule {ruleAnns = anns, ruleTc = infos, ruleTypeBinders = typeBinders, ruleBinders = binders, ruleLhs = lhs, ruleRhs = rhs}
    )
      <$> walkAnns w (ruleAnns rule)
      <*> walkCheckedTypeOf w (ruleTc rule)
      <*> walk w (ruleTypeBinders rule)
      <*> walk w (ruleBinders rule)
      <*> walk w (ruleLhs rule)
      <*> walk w (ruleRhs rule)

instance Walkable RuleBinder where
  walk w binder =
    (\anns infos name ty -> binder {ruleBinderAnns = anns, ruleBinderTc = infos, ruleBinderName = name, ruleBinderType = ty})
      <$> walkAnns w (ruleBinderAnns binder)
      <*> walkCheckedTypeOf w (ruleBinderTc binder)
      <*> walk w (ruleBinderName binder)
      <*> walk w (ruleBinderType binder)

instance Walkable ValueDecl where
  walk w value =
    case value of
      FunctionBind name matches -> FunctionBind <$> walk w name <*> walk w matches
      PatternBind multiplicity pat rhs ->
        PatternBind <$> walk w multiplicity <*> walk w pat <*> walk w rhs

instance Walkable MultiplicityTag where
  walk w tag =
    case tag of
      NoMultiplicityTag -> pure NoMultiplicityTag
      LinearMultiplicityTag -> pure LinearMultiplicityTag
      ExplicitMultiplicityTag ty -> ExplicitMultiplicityTag <$> walk w ty

instance Walkable Match where
  walk w (Match anns arityCast headForm pats rhs) =
    Match <$> walkAnns w anns <*> walkRemovable (factMatchArityCast (walkFacts w)) arityCast <*> pure headForm <*> walk w pats <*> walk w rhs

instance Walkable PatSynDecl where
  walk w (PatSynDecl name args pat direction equations) =
    PatSynDecl <$> walk w name <*> pure args <*> walk w pat <*> walk w direction <*> traverse (factPatSynEquations (walkFacts w) (walk w)) equations

instance Walkable PatSynEquations where
  walk w (PatSynEquations matcher builder selectors) =
    PatSynEquations <$> walk w matcher <*> walk w builder <*> traverse (\(label, match) -> (label,) <$> walk w match) selectors

instance Walkable PatSynDir where
  walk w direction =
    case direction of
      PatSynUnidirectional -> pure PatSynUnidirectional
      PatSynBidirectional -> pure PatSynBidirectional
      PatSynExplicitBidirectional matches -> PatSynExplicitBidirectional <$> walk w matches

instance (Walkable body) => Walkable (Rhs body) where
  walk w rhs =
    case rhs of
      UnguardedRhs anns infos body decls ->
        UnguardedRhs <$> walkAnns w anns <*> walkCasts w infos <*> walk w body <*> walk w decls
      GuardedRhss anns infos guarded decls ->
        GuardedRhss <$> walkAnns w anns <*> walkCasts w infos <*> walk w guarded <*> walk w decls

instance (Walkable body) => Walkable (GuardedRhs body) where
  walk w (GuardedRhs anns guards body) =
    GuardedRhs <$> walkAnns w anns <*> walk w guards <*> walk w body

instance Walkable GuardQualifier where
  walk w = walkGuardQualifier w $ \case
    GuardAnn ann inner -> GuardAnn <$> walkAnnotation w ann <*> walk w inner
    GuardExpr expr -> GuardExpr <$> walk w expr
    GuardPat pat expr -> GuardPat <$> walk w pat <*> walk w expr
    GuardLet decls -> GuardLet <$> walk w decls

instance Walkable Literal where
  walk w = walkLiteral w $ \literal ->
    case literal of
      LitAnn ann inner -> LitAnn <$> walkAnnotation w ann <*> walk w inner
      LitTc info inner -> LitTc <$> walkTcAnnotation w info <*> walk w inner
      LitInt {} -> pure literal
      LitFloat {} -> pure literal
      LitChar {} -> pure literal
      LitCharHash {} -> pure literal
      LitString {} -> pure literal
      LitStringHash {} -> pure literal

-- Patterns

instance (Walkable a) => Walkable (RecordField a) where
  walk w (RecordField name value pun) =
    RecordField <$> walk w name <*> walk w value <*> pure pun

instance Walkable LiteralMatch where
  walk w = \case
    LiteralMatchFixed -> pure LiteralMatchFixed
    LiteralMatchPrimitive name -> LiteralMatchPrimitive <$> walk w name
    LiteralMatchOverloaded match -> LiteralMatchOverloaded <$> walk w match

instance Walkable OverloadedLiteralMatch where
  walk w (OverloadedLiteralMatch ty conversion negation equality) =
    OverloadedLiteralMatch <$> walk w ty <*> walk w conversion <*> walk w negation <*> walk w equality

instance Walkable NPlusKMatch where
  walk w (NPlusKMatch ty fromInteger' greaterEqual minus) =
    NPlusKMatch <$> walk w ty <*> walk w fromInteger' <*> walk w greaterEqual <*> walk w minus

instance Walkable ListMatch where
  walk w (ListMatch toList' fromListN') =
    ListMatch <$> walk w toList' <*> walk w fromListN'

instance Walkable LiteralSyntax where
  walk w syntax =
    case syntax of
      LiteralSyntaxPrimitive typeName -> LiteralSyntaxPrimitive <$> walk w typeName
      LiteralSyntaxOverloaded ty conversion -> LiteralSyntaxOverloaded <$> walk w ty <*> walk w conversion

instance Walkable Pattern where
  walk w = walkPattern w $ \case
    PAnn ann inner -> PAnn <$> walkAnnotation w ann <*> walk w inner
    PTc info inner -> wrapTc PTc <$> walkPatternTc w info <*> walk w inner
    PVar name -> PVar <$> walk w name
    PTypeBinder binder -> PTypeBinder <$> walk w binder
    PTypeSyntax form ty -> PTypeSyntax form <$> walk w ty
    PWildcard -> pure PWildcard
    PLit match literal -> PLit <$> walk w match <*> walk w literal
    PQuasiQuote quoter body -> pure (PQuasiQuote quoter body)
    PTuple flavor items -> PTuple flavor <$> walk w items
    PUnboxedSum position arity inner -> PUnboxedSum position arity <$> walk w inner
    PList match items -> PList <$> walk w match <*> walk w items
    PCon name types pats -> PCon <$> walk w name <*> walk w types <*> walk w pats
    PBuiltinCon builtin types pats -> PBuiltinCon builtin <$> walk w types <*> walk w pats
    PInfix lhs name rhs -> PInfix <$> walk w lhs <*> walk w name <*> walk w rhs
    PView expr inner -> PView <$> walk w expr <*> walk w inner
    PAs name inner -> PAs <$> walk w name <*> walk w inner
    PStrict inner -> PStrict <$> walk w inner
    PIrrefutable inner -> PIrrefutable <$> walk w inner
    PNegLit match literal -> PNegLit <$> walk w match <*> walk w literal
    PNPlusK match name literal -> PNPlusK <$> walk w match <*> walk w name <*> walk w literal
    PParen inner -> PParen <$> walk w inner
    PRecord name fields wildcard -> PRecord <$> walk w name <*> walk w fields <*> pure wildcard
    PTypeSig inner ty -> PTypeSig <$> walk w inner <*> walk w ty
    PSplice expr -> PSplice <$> walk w expr

-- Types

instance Walkable ForallTelescope where
  walk w (ForallTelescope visibility binders) =
    ForallTelescope visibility <$> walk w binders

instance Walkable ArrowKind where
  walk w arrow =
    case arrow of
      ArrowUnrestricted -> pure ArrowUnrestricted
      ArrowLinear -> pure ArrowLinear
      ArrowExplicit ty -> ArrowExplicit <$> walk w ty

instance Walkable Type where
  walk w = walkType w $ \case
    TAnn ann inner -> TAnn <$> walkAnnotation w ann <*> walk w inner
    TVar name -> TVar <$> walk w name
    TCon name promotion -> TCon <$> walk w name <*> pure promotion
    TBuiltinCon builtin promotion -> pure (TBuiltinCon builtin promotion)
    TImplicitParam name payload -> TImplicitParam name <$> walk w payload
    TTypeLit literal -> pure (TTypeLit literal)
    TStar text -> pure (TStar text)
    TQuasiQuote quoter body -> pure (TQuasiQuote quoter body)
    TForall telescope inner -> TForall <$> walk w telescope <*> walk w inner
    TApp function argument -> TApp <$> walk w function <*> walk w argument
    TTypeApp function argument -> TTypeApp <$> walk w function <*> walk w argument
    TInfix lhs name promotion rhs ->
      TInfix <$> walk w lhs <*> walk w name <*> pure promotion <*> walk w rhs
    TFun arrow argument result ->
      TFun <$> walk w arrow <*> walk w argument <*> walk w result
    TTuple syntax flavor promotion items -> (\syntax' -> TTuple syntax' flavor promotion) <$> walk w syntax <*> walk w items
    TUnboxedSum items -> TUnboxedSum <$> walk w items
    TList syntax promotion items -> (\syntax' -> TList syntax' promotion) <$> walk w syntax <*> walk w items
    TParen inner -> TParen <$> walk w inner
    TKindSig inner kind -> TKindSig <$> walk w inner <*> walk w kind
    TContext context inner -> TContext <$> walk w context <*> walk w inner
    TSplice expr -> TSplice <$> walk w expr
    TWildcard -> pure TWildcard

instance Walkable TyVarBinder where
  walk w binder =
    (\anns kind -> binder {tyVarBinderAnns = anns, tyVarBinderKind = kind})
      <$> walkAnns w (tyVarBinderAnns binder)
      <*> walk w (tyVarBinderKind binder)

instance (Walkable name) => Walkable (BinderHead name) where
  walk w binderHead =
    case binderHead of
      PrefixBinderHead name params -> PrefixBinderHead <$> walk w name <*> walk w params
      InfixBinderHead lhs name rhs params ->
        InfixBinderHead
          <$> walk w lhs
          <*> walk w name
          <*> walk w rhs
          <*> walk w params

-- Type declarations

instance Walkable RoleAnnotation where
  walk w (RoleAnnotation name roles) =
    RoleAnnotation <$> walk w name <*> pure roles

instance Walkable TypeSynDecl where
  walk w (TypeSynDecl synHead body) =
    TypeSynDecl <$> walk w synHead <*> walk w body

instance Walkable TypeFamilyDecl where
  walk w (TypeFamilyDecl headForm explicitKeyword familyHead params resultSig equations) =
    TypeFamilyDecl headForm explicitKeyword
      <$> walk w familyHead
      <*> walk w params
      <*> walk w resultSig
      <*> walk w equations

instance Walkable TypeFamilyResultSig where
  walk w resultSig =
    case resultSig of
      TypeFamilyKindSig kind -> TypeFamilyKindSig <$> walk w kind
      TypeFamilyTyVarSig binder -> TypeFamilyTyVarSig <$> walk w binder
      TypeFamilyInjectiveSig binder injectivity ->
        TypeFamilyInjectiveSig <$> walk w binder <*> walk w injectivity

instance Walkable TypeFamilyInjectivity where
  walk w injectivity =
    (\anns -> injectivity {typeFamilyInjectivityAnns = anns})
      <$> walkAnns w (typeFamilyInjectivityAnns injectivity)

instance Walkable TypeFamilyEq where
  walk w (TypeFamilyEq anns binders headForm lhs rhs) =
    TypeFamilyEq
      <$> walkAnns w anns
      <*> walk w binders
      <*> pure headForm
      <*> walk w lhs
      <*> walk w rhs

instance Walkable DataFamilyDecl where
  walk w (DataFamilyDecl familyHead kind) =
    DataFamilyDecl <$> walk w familyHead <*> walk w kind

instance Walkable TypeFamilyInst where
  walk w (TypeFamilyInst binders headForm lhs rhs checked) =
    TypeFamilyInst <$> walk w binders <*> pure headForm <*> walk w lhs <*> walk w rhs <*> traverse (factTypeFamilyInstance (walkFacts w)) checked

instance Walkable DataFamilyInst where
  walk w (DataFamilyInst isNewtype binders instHead kind constructors derivings checked) =
    DataFamilyInst isNewtype
      <$> walk w binders
      <*> walk w instHead
      <*> walk w kind
      <*> walk w constructors
      <*> walk w derivings
      <*> traverse (factDataFamilyInstance (walkFacts w)) checked

instance Walkable DataDecl where
  walk w (DataDecl pragma dataHead context kind constructors derivings) =
    DataDecl pragma
      <$> walk w dataHead
      <*> walk w context
      <*> walk w kind
      <*> walk w constructors
      <*> walk w derivings

instance Walkable NewtypeDecl where
  walk w (NewtypeDecl pragma newtypeHead context kind constructor derivings) =
    NewtypeDecl pragma
      <$> walk w newtypeHead
      <*> walk w context
      <*> walk w kind
      <*> walk w constructor
      <*> walk w derivings

instance Walkable DataConDecl where
  walk w = walkDataConDecl w $ \case
    DataConAnn ann inner -> DataConAnn <$> walkAnnotation w ann <*> walk w inner
    PrefixCon tc binders context name fields ->
      PrefixCon <$> walkCheckedTypeOf w tc <*> walk w binders <*> walk w context <*> walk w name <*> walk w fields
    InfixCon tc binders context lhs name rhs ->
      InfixCon
        <$> walkCheckedTypeOf w tc
        <*> walk w binders
        <*> walk w context
        <*> walk w lhs
        <*> walk w name
        <*> walk w rhs
    RecordCon tc binders context name fields ->
      RecordCon <$> walkCheckedTypeOf w tc <*> walk w binders <*> walk w context <*> walk w name <*> walk w fields
    GadtCon tc telescopes context names body ->
      GadtCon <$> walkCheckedTypeOf w tc <*> walk w telescopes <*> walk w context <*> walk w names <*> walk w body
    TupleCon tc binders context flavor fields ->
      TupleCon <$> walkCheckedTypeOf w tc <*> walk w binders <*> walk w context <*> pure flavor <*> walk w fields
    UnboxedSumCon tc binders context position arity field ->
      UnboxedSumCon <$> walkCheckedTypeOf w tc <*> walk w binders <*> walk w context <*> pure position <*> pure arity <*> walk w field
    ListCon tc binders context syntax -> ListCon <$> walkCheckedTypeOf w tc <*> walk w binders <*> walk w context <*> walk w syntax

instance Walkable GadtBody where
  walk w body =
    case body of
      GadtPrefixBody arguments result -> GadtPrefixBody <$> walk w arguments <*> walk w result
      GadtRecordBody fields result -> GadtRecordBody <$> walk w fields <*> walk w result

instance Walkable BangType where
  walk w (BangType anns pragmas strict lazy ty) =
    BangType <$> walkAnns w anns <*> pure pragmas <*> pure strict <*> pure lazy <*> walk w ty

instance Walkable FieldDecl where
  walk w (FieldDecl anns names multiplicity ty selectors) =
    FieldDecl <$> walkAnns w anns <*> walk w names <*> walk w multiplicity <*> walk w ty <*> pure selectors

instance Walkable DerivingClause where
  walk w (DerivingClause strategy classes plans) =
    DerivingClause <$> walk w strategy <*> walk w classes <*> traverse (factDerivingPlan (walkFacts w)) plans

instance Walkable DerivingStrategy where
  walk w strategy =
    case strategy of
      DerivingStock -> pure DerivingStock
      DerivingNewtype -> pure DerivingNewtype
      DerivingAnyclass -> pure DerivingAnyclass
      DerivingVia ty -> DerivingVia <$> walk w ty

instance Walkable StandaloneDerivingDecl where
  walk w (StandaloneDerivingDecl strategy pragmas warning binders context instHead plan) =
    StandaloneDerivingDecl
      <$> walk w strategy
      <*> pure pragmas
      <*> pure warning
      <*> walk w binders
      <*> walk w context
      <*> walk w instHead
      <*> traverse (factDerivingPlan (walkFacts w)) plan

-- Classes and instances

instance Walkable ClassDecl where
  walk w (ClassDecl context classHead fundeps items checked) =
    ClassDecl
      <$> walk w context
      <*> walk w classHead
      <*> walk w fundeps
      <*> walk w items
      <*> traverse (factClass (walkFacts w)) checked

instance Walkable FunctionalDependency where
  walk w fundep =
    (\anns -> fundep {functionalDependencyAnns = anns})
      <$> walkAnns w (functionalDependencyAnns fundep)

instance Walkable ClassDeclItem where
  walk w = walkClassDeclItem w $ \case
    ClassItemAnn ann inner -> ClassItemAnn <$> walkAnnotation w ann <*> walk w inner
    ClassItemError err inner -> ClassItemError err <$> walk w inner
    ClassItemTypeSig names ty -> ClassItemTypeSig <$> walk w names <*> walk w ty
    ClassItemDefaultSig name ty -> ClassItemDefaultSig <$> walk w name <*> walk w ty
    ClassItemFixity assoc namespace precedence operators ->
      ClassItemFixity assoc namespace precedence <$> walk w operators
    ClassItemDefault tc value -> ClassItemDefault <$> traverse (factInstanceMethod (walkFacts w)) tc <*> walk w value
    ClassItemTypeFamilyDecl familyDecl -> ClassItemTypeFamilyDecl <$> walk w familyDecl
    ClassItemDataFamilyDecl familyDecl -> ClassItemDataFamilyDecl <$> walk w familyDecl
    ClassItemDefaultTypeInst familyInst -> ClassItemDefaultTypeInst <$> walk w familyInst
    ClassItemPragma pragma -> pure (ClassItemPragma pragma)

instance Walkable InstanceDecl where
  walk w (InstanceDecl pragmas warning binders context instHead items checked derived) =
    InstanceDecl pragmas warning
      <$> walk w binders
      <*> walk w context
      <*> walk w instHead
      <*> walk w items
      <*> traverse (factInstance (walkFacts w)) checked
      <*> pure derived

instance Walkable InstanceDeclItem where
  walk w = walkInstanceDeclItem w $ \case
    InstanceItemAnn ann inner -> InstanceItemAnn <$> walkAnnotation w ann <*> walk w inner
    InstanceItemError err inner -> InstanceItemError err <$> walk w inner
    InstanceItemBind tc value -> InstanceItemBind <$> traverse (factInstanceMethod (walkFacts w)) tc <*> walk w value
    InstanceItemTypeSig names ty -> InstanceItemTypeSig <$> walk w names <*> walk w ty
    InstanceItemFixity assoc namespace precedence operators ->
      InstanceItemFixity assoc namespace precedence <$> walk w operators
    InstanceItemTypeFamilyInst familyInst -> InstanceItemTypeFamilyInst <$> walk w familyInst
    InstanceItemDataFamilyInst familyInst -> InstanceItemDataFamilyInst <$> walk w familyInst
    InstanceItemPragma pragma -> pure (InstanceItemPragma pragma)

instance Walkable ForeignDecl where
  walk w decl =
    (\name ty -> decl {foreignName = name, foreignType = ty})
      <$> walk w (foreignName decl)
      <*> walk w (foreignType decl)

-- Expressions

instance Walkable Expr where
  walk w = walkExpr w $ \expr ->
    case expr of
      EAnn ann inner -> EAnn <$> walkAnnotation w ann <*> walk w inner
      ETc info inner -> wrapTc ETc <$> walkExprTc w info <*> walk w inner
      EVar name -> EVar <$> walk w name
      EImplicitParam tc name -> (`EImplicitParam` name) <$> traverse (walkTcAnnotation w) tc
      ETypeSyntax form ty -> ETypeSyntax form <$> walk w ty
      EInt tc syntax value numericType text -> (\tc' syntax' -> EInt tc' syntax' value numericType text) <$> traverse (walkTcAnnotation w) tc <*> walk w syntax
      EFloat tc syntax value floatType text -> (\tc' syntax' -> EFloat tc' syntax' value floatType text) <$> traverse (walkTcAnnotation w) tc <*> walk w syntax
      EChar tc value text -> (\tc' -> EChar tc' value text) <$> traverse (walkTcAnnotation w) tc
      ECharHash tc typeName value text -> (\tc' typeName' -> ECharHash tc' typeName' value text) <$> traverse (walkTcAnnotation w) tc <*> walk w typeName
      EString tc conversion value text -> (\tc' conversion' -> EString tc' conversion' value text) <$> traverse (walkTcAnnotation w) tc <*> walk w conversion
      EStringHash tc typeName value text -> (\tc' typeName' -> EStringHash tc' typeName' value text) <$> traverse (walkTcAnnotation w) tc <*> walk w typeName
      EOverloadedLabel {} -> pure expr
      EQuasiQuote tc quoter body -> (\tc' -> EQuasiQuote tc' quoter body) <$> walkCheckedTypeOf w tc
      EIf ty method condition thenExpr elseExpr ->
        EIf <$> walkCheckedTypeOf w ty <*> walk w method <*> walk w condition <*> walk w thenExpr <*> walk w elseExpr
      EMultiWayIf ty alternatives -> EMultiWayIf <$> walkCheckedTypeOf w ty <*> walk w alternatives
      ELambdaPats ty pats body -> ELambdaPats <$> walkCheckedTypeOf w ty <*> walk w pats <*> walk w body
      ELambdaCase ty alternatives -> ELambdaCase <$> walkCheckedTypeOf w ty <*> walk w alternatives
      ELambdaCases ty alternatives -> ELambdaCases <$> walkCheckedTypeOf w ty <*> walk w alternatives
      EInfix lhs name rhs -> EInfix <$> walk w lhs <*> walk w name <*> walk w rhs
      EViewPat lhs rhs -> EViewPat <$> walk w lhs <*> walk w rhs
      ENegate tc method inner -> ENegate <$> walkCheckedTypeOf w tc <*> walk w method <*> walk w inner
      ESectionL tc inner name -> ESectionL <$> traverse (walkTcAnnotation w) tc <*> walk w inner <*> walk w name
      ESectionR tc name inner -> ESectionR <$> traverse (walkTcAnnotation w) tc <*> walk w name <*> walk w inner
      ELetDecls decls body -> ELetDecls <$> walk w decls <*> walk w body
      ECase ty scrutinee alternatives -> ECase <$> walkCheckedTypeOf w ty <*> walk w scrutinee <*> walk w alternatives
      EDo ty statements flavor -> EDo <$> walkCheckedTypeOf w ty <*> walk w statements <*> pure flavor
      EListComp tc body statements -> EListComp <$> traverse (walkTcAnnotation w) tc <*> walk w body <*> walk w statements
      EListCompParallel tc body branches -> EListCompParallel <$> traverse (walkTcAnnotation w) tc <*> walk w body <*> walk w branches
      EArithSeq ty method sequence' -> EArithSeq <$> walkCheckedTypeOf w ty <*> walk w method <*> walk w sequence'
      ERecordCon name fields wildcard -> ERecordCon <$> walk w name <*> walk w fields <*> pure wildcard
      ERecordUpd record fields -> ERecordUpd <$> walk w record <*> walk w fields
      EGetField tc method record field -> EGetField <$> walkCheckedTypeOf w tc <*> walk w method <*> walk w record <*> walk w field
      EGetFieldProjection tc method fields -> EGetFieldProjection <$> traverse (walkTcAnnotation w) tc <*> walk w method <*> walk w fields
      ETypeSig inner ty -> ETypeSig <$> walk w inner <*> walk w ty
      EParen inner -> EParen <$> walk w inner
      EList tc syntax method items -> EList <$> traverse (walkTcAnnotation w) tc <*> walk w syntax <*> walk w method <*> walk w items
      ETuple tc syntax flavor items -> (\tc' syntax' -> ETuple tc' syntax' flavor) <$> traverse (walkTcAnnotation w) tc <*> walk w syntax <*> walk w items
      EUnboxedSum tc position arity inner -> (\tc' -> EUnboxedSum tc' position arity) <$> traverse (walkTcAnnotation w) tc <*> walk w inner
      ETypeApp function ty -> ETypeApp <$> walk w function <*> walk w ty
      EApp function argument -> EApp <$> walk w function <*> walk w argument
      ETHExpQuote tc inner -> ETHExpQuote <$> walkCheckedTypeOf w tc <*> walk w inner
      ETHTypedQuote tc inner -> ETHTypedQuote <$> walkCheckedTypeOf w tc <*> walk w inner
      ETHDeclQuote tc decls -> ETHDeclQuote <$> walkCheckedTypeOf w tc <*> walk w decls
      ETHTypeQuote tc ty -> ETHTypeQuote <$> walkCheckedTypeOf w tc <*> walk w ty
      ETHPatQuote tc pat -> ETHPatQuote <$> walkCheckedTypeOf w tc <*> walk w pat
      ETHNameQuote tc inner -> ETHNameQuote <$> walkCheckedTypeOf w tc <*> walk w inner
      ETHTypeNameQuote tc ty -> ETHTypeNameQuote <$> walkCheckedTypeOf w tc <*> walk w ty
      ETHSplice inner -> ETHSplice <$> walk w inner
      ETHTypedSplice inner -> ETHTypedSplice <$> walk w inner
      EProc pat command -> EProc <$> walk w pat <*> walk w command
      EPragma pragma inner -> EPragma pragma <$> walk w inner

instance (Walkable body) => Walkable (CaseAlt body) where
  walk w (CaseAlt anns pat rhs) =
    CaseAlt <$> walkAnns w anns <*> walk w pat <*> walk w rhs

instance Walkable LambdaCaseAlt where
  walk w (LambdaCaseAlt anns pats rhs) =
    LambdaCaseAlt <$> walkAnns w anns <*> walk w pats <*> walk w rhs

instance (Walkable body) => Walkable (DoStmt body) where
  walk w = walkDoStmt w $ \case
    DoAnn ann inner -> DoAnn <$> walkAnnotation w ann <*> walk w inner
    DoRecKnot knot inner -> DoRecKnot knot <$> walk w inner
    DoBind methods pat body -> DoBind <$> walk w methods <*> walk w pat <*> walk w body
    DoLetDecls decls -> DoLetDecls <$> walk w decls
    DoExpr method body -> DoExpr <$> walk w method <*> walk w body
    DoRecStmt tc methods statements -> DoRecStmt <$> traverse (walkRecStmt w) tc <*> walk w methods <*> walk w statements

instance Walkable DoBindMethods where
  walk w (DoBindMethods method failure) =
    DoBindMethods <$> walk w method <*> walk w failure

instance Walkable DoRecMethods where
  walk w (DoRecMethods mfix return' bind) =
    DoRecMethods <$> walk w mfix <*> walk w return' <*> walk w bind

instance Walkable Cmd where
  walk w = walkCmd w $ \case
    CmdAnn ann inner -> CmdAnn <$> walkAnnotation w ann <*> walk w inner
    CmdArrApp function appType argument ->
      CmdArrApp <$> walk w function <*> pure appType <*> walk w argument
    CmdInfix lhs name rhs -> CmdInfix <$> walk w lhs <*> walk w name <*> walk w rhs
    CmdDo statements -> CmdDo <$> walk w statements
    CmdIf condition thenCmd elseCmd ->
      CmdIf <$> walk w condition <*> walk w thenCmd <*> walk w elseCmd
    CmdCase scrutinee alternatives -> CmdCase <$> walk w scrutinee <*> walk w alternatives
    CmdLet decls inner -> CmdLet <$> walk w decls <*> walk w inner
    CmdLam pats inner -> CmdLam <$> walk w pats <*> walk w inner
    CmdApp inner argument -> CmdApp <$> walk w inner <*> walk w argument
    CmdPar inner -> CmdPar <$> walk w inner

instance Walkable CompStmt where
  walk w = walkCompStmt w $ \case
    CompAnn ann inner -> CompAnn <$> walkAnnotation w ann <*> walk w inner
    CompGen tc pat expr -> CompGen <$> traverse (walkTcAnnotation w) tc <*> walk w pat <*> walk w expr
    CompGuard tc expr -> CompGuard <$> traverse (walkTcAnnotation w) tc <*> walk w expr
    CompLetDecls tc decls -> CompLetDecls <$> traverse (walkTcAnnotation w) tc <*> walk w decls
    CompThen tc expr -> CompThen <$> traverse (walkTcAnnotation w) tc <*> walk w expr
    CompThenBy tc function expr -> CompThenBy <$> traverse (walkTcAnnotation w) tc <*> walk w function <*> walk w expr
    CompGroupUsing tc function -> CompGroupUsing <$> traverse (walkTcAnnotation w) tc <*> walk w function
    CompGroupByUsing tc expr function -> CompGroupByUsing <$> traverse (walkTcAnnotation w) tc <*> walk w expr <*> walk w function

instance Walkable ArithSeq where
  walk w = walkArithSeq w $ \case
    ArithSeqAnn ann inner -> ArithSeqAnn <$> walkAnnotation w ann <*> walk w inner
    ArithSeqFrom method from -> ArithSeqFrom <$> walk w method <*> walk w from
    ArithSeqFromThen method from next -> ArithSeqFromThen <$> walk w method <*> walk w from <*> walk w next
    ArithSeqFromTo method from to -> ArithSeqFromTo <$> walk w method <*> walk w from <*> walk w to
    ArithSeqFromThenTo method from next to ->
      ArithSeqFromThenTo <$> walk w method <*> walk w from <*> walk w next <*> walk w to
