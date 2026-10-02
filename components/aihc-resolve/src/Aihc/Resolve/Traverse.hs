{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | A hand-written walk over the parser syntax tree.
--
-- The resolver and the type checker read and rewrite the annotations of a
-- whole module several times, and the type checker rewrites some nodes.
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
module Aihc.Resolve.Traverse
  ( Walkable (..),
    Walk (..),
    idWalk,
    traverseAnnotations,
    annotationList,
    collectAnnotations,
    Collect,
    collected,
    runCollect,
  )
where

import Aihc.Parser.Syntax

-- | The hooks of one walk. 'idWalk' walks every node and leaves it as it
-- is. A caller sets the hooks it needs.
data Walk f = Walk
  { -- | Every annotation, where it is attached.
    walkAnnotation :: Annotation -> f Annotation,
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

-- | The walk that changes nothing.
idWalk :: (Applicative f) => Walk f
idWalk =
  Walk
    { walkAnnotation = pure,
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
  walk w = walkName w $ \name -> (\anns -> name {nameAnns = anns}) <$> walkAnns w (nameAnns name)

instance Walkable UnqualifiedName where
  walk w name = (\anns -> name {unqualifiedNameAnns = anns}) <$> walkAnns w (unqualifiedNameAnns name)

-- Modules, exports, and imports

instance Walkable Module where
  walk w (Module anns modHead pragmas imports decls) =
    Module
      <$> walkAnns w anns
      <*> walk w modHead
      <*> pure pragmas
      <*> walk w imports
      <*> walk w decls

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

-- Declarations

instance Walkable Decl where
  walk w = walkDecl w $ \case
    DeclAnn ann inner -> DeclAnn <$> walkAnnotation w ann <*> walk w inner
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
    ( \anns typeBinders binders lhs rhs ->
        rule {ruleAnns = anns, ruleTypeBinders = typeBinders, ruleBinders = binders, ruleLhs = lhs, ruleRhs = rhs}
    )
      <$> walkAnns w (ruleAnns rule)
      <*> walk w (ruleTypeBinders rule)
      <*> walk w (ruleBinders rule)
      <*> walk w (ruleLhs rule)
      <*> walk w (ruleRhs rule)

instance Walkable RuleBinder where
  walk w binder =
    (\anns name ty -> binder {ruleBinderAnns = anns, ruleBinderName = name, ruleBinderType = ty})
      <$> walkAnns w (ruleBinderAnns binder)
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
  walk w (Match anns headForm pats rhs) =
    Match <$> walkAnns w anns <*> pure headForm <*> walk w pats <*> walk w rhs

instance Walkable PatSynDecl where
  walk w (PatSynDecl name args pat direction) =
    PatSynDecl <$> walk w name <*> pure args <*> walk w pat <*> walk w direction

instance Walkable PatSynDir where
  walk w direction =
    case direction of
      PatSynUnidirectional -> pure PatSynUnidirectional
      PatSynBidirectional -> pure PatSynBidirectional
      PatSynExplicitBidirectional matches -> PatSynExplicitBidirectional <$> walk w matches

instance (Walkable body) => Walkable (Rhs body) where
  walk w rhs =
    case rhs of
      UnguardedRhs anns body decls ->
        UnguardedRhs <$> walkAnns w anns <*> walk w body <*> walk w decls
      GuardedRhss anns guarded decls ->
        GuardedRhss <$> walkAnns w anns <*> walk w guarded <*> walk w decls

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

instance Walkable Pattern where
  walk w = walkPattern w $ \case
    PAnn ann inner -> PAnn <$> walkAnnotation w ann <*> walk w inner
    PVar name -> PVar <$> walk w name
    PTypeBinder binder -> PTypeBinder <$> walk w binder
    PTypeSyntax form ty -> PTypeSyntax form <$> walk w ty
    PWildcard -> pure PWildcard
    PLit literal -> PLit <$> walk w literal
    PQuasiQuote quoter body -> pure (PQuasiQuote quoter body)
    PTuple flavor items -> PTuple flavor <$> walk w items
    PUnboxedSum position arity inner -> PUnboxedSum position arity <$> walk w inner
    PList items -> PList <$> walk w items
    PCon name types pats -> PCon <$> walk w name <*> walk w types <*> walk w pats
    PBuiltinCon builtin types pats -> PBuiltinCon builtin <$> walk w types <*> walk w pats
    PInfix lhs name rhs -> PInfix <$> walk w lhs <*> walk w name <*> walk w rhs
    PView expr inner -> PView <$> walk w expr <*> walk w inner
    PAs name inner -> PAs <$> walk w name <*> walk w inner
    PStrict inner -> PStrict <$> walk w inner
    PIrrefutable inner -> PIrrefutable <$> walk w inner
    PNegLit literal -> PNegLit <$> walk w literal
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
    TTuple flavor promotion items -> TTuple flavor promotion <$> walk w items
    TUnboxedSum items -> TUnboxedSum <$> walk w items
    TList promotion items -> TList promotion <$> walk w items
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
  walk w (TypeFamilyInst binders headForm lhs rhs) =
    TypeFamilyInst <$> walk w binders <*> pure headForm <*> walk w lhs <*> walk w rhs

instance Walkable DataFamilyInst where
  walk w (DataFamilyInst isNewtype binders instHead kind constructors derivings) =
    DataFamilyInst isNewtype
      <$> walk w binders
      <*> walk w instHead
      <*> walk w kind
      <*> walk w constructors
      <*> walk w derivings

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
    PrefixCon binders context name fields ->
      PrefixCon <$> walk w binders <*> walk w context <*> walk w name <*> walk w fields
    InfixCon binders context lhs name rhs ->
      InfixCon
        <$> walk w binders
        <*> walk w context
        <*> walk w lhs
        <*> walk w name
        <*> walk w rhs
    RecordCon binders context name fields ->
      RecordCon <$> walk w binders <*> walk w context <*> walk w name <*> walk w fields
    GadtCon telescopes context names body ->
      GadtCon <$> walk w telescopes <*> walk w context <*> walk w names <*> walk w body
    TupleCon binders context flavor fields ->
      TupleCon <$> walk w binders <*> walk w context <*> pure flavor <*> walk w fields
    UnboxedSumCon binders context position arity field ->
      UnboxedSumCon <$> walk w binders <*> walk w context <*> pure position <*> pure arity <*> walk w field
    ListCon binders context -> ListCon <$> walk w binders <*> walk w context

instance Walkable GadtBody where
  walk w body =
    case body of
      GadtPrefixBody arguments result -> GadtPrefixBody <$> walk w arguments <*> walk w result
      GadtRecordBody fields result -> GadtRecordBody <$> walk w fields <*> walk w result

instance Walkable BangType where
  walk w (BangType anns pragmas strict lazy ty) =
    BangType <$> walkAnns w anns <*> pure pragmas <*> pure strict <*> pure lazy <*> walk w ty

instance Walkable FieldDecl where
  walk w (FieldDecl anns names multiplicity ty) =
    FieldDecl <$> walkAnns w anns <*> walk w names <*> walk w multiplicity <*> walk w ty

instance Walkable DerivingClause where
  walk w (DerivingClause strategy classes) =
    DerivingClause <$> walk w strategy <*> walk w classes

instance Walkable DerivingStrategy where
  walk w strategy =
    case strategy of
      DerivingStock -> pure DerivingStock
      DerivingNewtype -> pure DerivingNewtype
      DerivingAnyclass -> pure DerivingAnyclass
      DerivingVia ty -> DerivingVia <$> walk w ty

instance Walkable StandaloneDerivingDecl where
  walk w (StandaloneDerivingDecl strategy pragmas warning binders context instHead) =
    StandaloneDerivingDecl
      <$> walk w strategy
      <*> pure pragmas
      <*> pure warning
      <*> walk w binders
      <*> walk w context
      <*> walk w instHead

-- Classes and instances

instance Walkable ClassDecl where
  walk w (ClassDecl context classHead fundeps items) =
    ClassDecl
      <$> walk w context
      <*> walk w classHead
      <*> walk w fundeps
      <*> walk w items

instance Walkable FunctionalDependency where
  walk w fundep =
    (\anns -> fundep {functionalDependencyAnns = anns})
      <$> walkAnns w (functionalDependencyAnns fundep)

instance Walkable ClassDeclItem where
  walk w = walkClassDeclItem w $ \case
    ClassItemAnn ann inner -> ClassItemAnn <$> walkAnnotation w ann <*> walk w inner
    ClassItemTypeSig names ty -> ClassItemTypeSig <$> walk w names <*> walk w ty
    ClassItemDefaultSig name ty -> ClassItemDefaultSig <$> walk w name <*> walk w ty
    ClassItemFixity assoc namespace precedence operators ->
      ClassItemFixity assoc namespace precedence <$> walk w operators
    ClassItemDefault value -> ClassItemDefault <$> walk w value
    ClassItemTypeFamilyDecl familyDecl -> ClassItemTypeFamilyDecl <$> walk w familyDecl
    ClassItemDataFamilyDecl familyDecl -> ClassItemDataFamilyDecl <$> walk w familyDecl
    ClassItemDefaultTypeInst familyInst -> ClassItemDefaultTypeInst <$> walk w familyInst
    ClassItemPragma pragma -> pure (ClassItemPragma pragma)

instance Walkable InstanceDecl where
  walk w (InstanceDecl pragmas warning binders context instHead items) =
    InstanceDecl pragmas warning
      <$> walk w binders
      <*> walk w context
      <*> walk w instHead
      <*> walk w items

instance Walkable InstanceDeclItem where
  walk w = walkInstanceDeclItem w $ \case
    InstanceItemAnn ann inner -> InstanceItemAnn <$> walkAnnotation w ann <*> walk w inner
    InstanceItemBind value -> InstanceItemBind <$> walk w value
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
      EVar name -> EVar <$> walk w name
      EImplicitParam {} -> pure expr
      ETypeSyntax form ty -> ETypeSyntax form <$> walk w ty
      EInt {} -> pure expr
      EFloat {} -> pure expr
      EChar {} -> pure expr
      ECharHash {} -> pure expr
      EString {} -> pure expr
      EStringHash {} -> pure expr
      EOverloadedLabel {} -> pure expr
      EQuasiQuote {} -> pure expr
      EIf condition thenExpr elseExpr ->
        EIf <$> walk w condition <*> walk w thenExpr <*> walk w elseExpr
      EMultiWayIf alternatives -> EMultiWayIf <$> walk w alternatives
      ELambdaPats pats body -> ELambdaPats <$> walk w pats <*> walk w body
      ELambdaCase alternatives -> ELambdaCase <$> walk w alternatives
      ELambdaCases alternatives -> ELambdaCases <$> walk w alternatives
      EInfix lhs name rhs -> EInfix <$> walk w lhs <*> walk w name <*> walk w rhs
      EViewPat lhs rhs -> EViewPat <$> walk w lhs <*> walk w rhs
      ENegate inner -> ENegate <$> walk w inner
      ESectionL inner name -> ESectionL <$> walk w inner <*> walk w name
      ESectionR name inner -> ESectionR <$> walk w name <*> walk w inner
      ELetDecls decls body -> ELetDecls <$> walk w decls <*> walk w body
      ECase scrutinee alternatives -> ECase <$> walk w scrutinee <*> walk w alternatives
      EDo statements flavor -> EDo <$> walk w statements <*> pure flavor
      EListComp body statements -> EListComp <$> walk w body <*> walk w statements
      EListCompParallel body branches -> EListCompParallel <$> walk w body <*> walk w branches
      EArithSeq sequence' -> EArithSeq <$> walk w sequence'
      ERecordCon name fields wildcard -> ERecordCon <$> walk w name <*> walk w fields <*> pure wildcard
      ERecordUpd record fields -> ERecordUpd <$> walk w record <*> walk w fields
      EGetField record field -> EGetField <$> walk w record <*> walk w field
      EGetFieldProjection fields -> EGetFieldProjection <$> walk w fields
      ETypeSig inner ty -> ETypeSig <$> walk w inner <*> walk w ty
      EParen inner -> EParen <$> walk w inner
      EList items -> EList <$> walk w items
      ETuple flavor items -> ETuple flavor <$> walk w items
      EUnboxedSum position arity inner -> EUnboxedSum position arity <$> walk w inner
      ETypeApp function ty -> ETypeApp <$> walk w function <*> walk w ty
      EApp function argument -> EApp <$> walk w function <*> walk w argument
      ETHExpQuote inner -> ETHExpQuote <$> walk w inner
      ETHTypedQuote inner -> ETHTypedQuote <$> walk w inner
      ETHDeclQuote decls -> ETHDeclQuote <$> walk w decls
      ETHTypeQuote ty -> ETHTypeQuote <$> walk w ty
      ETHPatQuote pat -> ETHPatQuote <$> walk w pat
      ETHNameQuote inner -> ETHNameQuote <$> walk w inner
      ETHTypeNameQuote ty -> ETHTypeNameQuote <$> walk w ty
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
    DoBind pat body -> DoBind <$> walk w pat <*> walk w body
    DoLetDecls decls -> DoLetDecls <$> walk w decls
    DoExpr body -> DoExpr <$> walk w body
    DoRecStmt statements -> DoRecStmt <$> walk w statements

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
    CompGen pat expr -> CompGen <$> walk w pat <*> walk w expr
    CompGuard expr -> CompGuard <$> walk w expr
    CompLetDecls decls -> CompLetDecls <$> walk w decls
    CompThen expr -> CompThen <$> walk w expr
    CompThenBy function expr -> CompThenBy <$> walk w function <*> walk w expr
    CompGroupUsing function -> CompGroupUsing <$> walk w function
    CompGroupByUsing expr function -> CompGroupByUsing <$> walk w expr <*> walk w function

instance Walkable ArithSeq where
  walk w = walkArithSeq w $ \case
    ArithSeqAnn ann inner -> ArithSeqAnn <$> walkAnnotation w ann <*> walk w inner
    ArithSeqFrom from -> ArithSeqFrom <$> walk w from
    ArithSeqFromThen from next -> ArithSeqFromThen <$> walk w from <*> walk w next
    ArithSeqFromTo from to -> ArithSeqFromTo <$> walk w from <*> walk w to
    ArithSeqFromThenTo from next to ->
      ArithSeqFromThenTo <$> walk w from <*> walk w next <*> walk w to
