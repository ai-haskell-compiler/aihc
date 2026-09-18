{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Core type representation for the type checker.
module Aihc.Tc.Types
  ( TcTermKey (..),
    tyConTermKey,
    tyConMemberTermKey,
    termKeyName,
    Unique (..),
    TyVarId (TyVarId, tvName, tvUnique),
    mkTyVarId,
    tvKind,
    sameTyVar,
    TypeShape,
    PredShape,
    typeShape,
    predShape,
    sameType,
    samePred,
    setTyVarKind,
    TcType (..),
    isPolyType,
    TcTypeKey (..),
    TcAxiomKey (..),
    TcKindEnv,
    TyCon (TyCon, tyConName, tyConArity),
    tyConKey,
    tyConPackageId,
    tyConModuleName,
    TcKinds (..),
    isEqualityTyCon,
    mkAppTy,
    tyConNamespace,
    mkTyConWithOrigin,
    mkTyConWithNamespace,
    TypeScheme (.., ForAll),
    specifiedScheme,
    traverseScheme,
    defaultMethodWorkerScheme,
    typeSchemeInferred,
    typeSchemeSpecified,
    TyLit (..),
    tyLitKind,
    tyLitKindTyCon,
    typeKindInEnv,
    TcTypeApplicationKinds (..),
    typeApplicationKinds,
    typeKind,
    constraintKind,
    runtimeRepKind,
    mkTYPEKind,
    liftedRep,
    tupleRep,
    sumRep,
    intRep,
    int8Rep,
    int16Rep,
    int32Rep,
    int64Rep,
    wordRep,
    word8Rep,
    word16Rep,
    word32Rep,
    word64Rep,
    addrRep,
    runtimeRepFromKind,
    isFixedRuntimeRep,
    runtimeRepOfTypeInEnv,
    isUnliftedTypeInEnv,
    pattern KTYPE,
    pattern KConstraint,
    pattern KRuntimeRep,
    pattern KLevity,
    pattern KVecCount,
    pattern KVecElem,
    pattern KFun,
    pattern KMeta,
    pattern KType,
    pattern BoxedRep,
    pattern TupleRep,
    pattern SumRep,
    pattern VecRep,
    pattern Lifted,
    pattern Unlifted,
    pattern IntRep,
    pattern Int8Rep,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern WordRep,
    pattern Word8Rep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern AddrRep,
    pattern FloatRep,
    pattern DoubleRep,
    typeSchemeBody,
    applySubst,
    applySubstPred,
    typeMentionsTyVar,
    predicateMentionsTyVar,
    typeMentionsMeta,
    predicateMentionsMeta,
    kindMentionsUnique,
    Pred (..),
    constraintTypeToPred,
    collectForAllTypes,
    collectTypeApplications,
    isImplicitParamTyConName,
  )
where

import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Control.DeepSeq (NFData (..))
import Control.Monad (zipWithM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data TcTermKey
  = TcTermLocal !Int
  | TcTermGlobal !PackageId !Text !Text
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TcTermKey

-- | The term key with the name and origin of a type constructor.
tyConTermKey :: TyCon -> TcTermKey
tyConTermKey tyCon = tyConMemberTermKey tyCon (tyConName tyCon)

-- | The term key of a constructor, record selector, or class method.
-- The type constructor supplies the package and module.
tyConMemberTermKey :: TyCon -> Text -> TcTermKey
tyConMemberTermKey tyCon = TcTermGlobal (tyConPackageId tyCon) (tyConModuleName tyCon)

-- | The name of a term key for a diagnostic.
termKeyName :: TcTermKey -> Text
termKeyName key =
  case key of
    TcTermGlobal _ _ name -> name
    TcTermLocal unique -> T.pack ("<local " <> show unique <> ">")

newtype Unique = Unique Int
  deriving (Eq, Ord, Show, Read, Generic)

-- | Every field of these types is strict and holds a value that is already
-- in normal form once it is in weak head normal form -- a 'Text' is a
-- byte array and an offset, a namespace is a nullary constructor -- so
-- forcing the constructor forces the whole value. The derived instances
-- went through the generic representation of the type instead, and these
-- are forced often enough for that to show.
instance NFData Unique where
  rnf unique = unique `seq` ()

-- | A type variable and its type-level kind. Equality is structural: two
-- occurrences of one variable whose kinds differ (a GADT match can refine
-- the kind of a variable in scope) are two values. Code that asks whether
-- two occurrences are the same variable uses 'sameTyVar'.
data TyVarId = TyVarIdInternal !Text !Unique !TcType
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TyVarId where
  rnf (TyVarIdInternal _ _ kind) = rnf kind

-- The identity of a variable: its name and unique, without its kind.
tyVarIdentity :: TyVarId -> (Unique, Text)
tyVarIdentity (TyVarIdInternal name unique _) = (unique, name)

-- | A type variable is matched by its name and its unique. Its kind is
-- read with 'tvKind' and given with 'mkTyVarId': the module knows no kind
-- vocabulary of its own, so it has no kind to default to.
pattern TyVarId :: Text -> Unique -> TyVarId
pattern TyVarId {tvName, tvUnique} <- TyVarIdInternal tvName tvUnique _

{-# COMPLETE TyVarId #-}

-- | A type variable of one name, unique and kind.
mkTyVarId :: Text -> Unique -> TcType -> TyVarId
mkTyVarId = TyVarIdInternal

tvKind :: TyVarId -> TcType
tvKind (TyVarIdInternal _ _ kind) = kind

setTyVarKind :: TcType -> TyVarId -> TyVarId
setTyVarKind kind (TyVarIdInternal name unique _) = TyVarIdInternal name unique kind

-- | A type-constructor identity. Kind schemes live in the type-constructor environment.
data TyCon = TyConInternal !Text !PackageId !Text !ResolutionNamespace !Int
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TyCon where
  rnf tyCon = tyCon `seq` ()

pattern TyCon :: Text -> Int -> TyCon
pattern TyCon {tyConName, tyConArity} <- TyConInternal tyConName _ _ _ tyConArity

{-# COMPLETE TyCon #-}

-- | The identity a type constructor is registered under: everything a
-- 'TyCon' carries except its arity, which two constructors of one identity
-- may differ in.
--
-- The derived 'Ord' compares the fields in the order they are declared, and
-- the name comes first deliberately: it is what discriminates, where a
-- package id is a long 'Text' that a whole package shares.
data TcTypeKey = TcTypeKey
  { typeKeyName :: !Text,
    typeKeyPackage :: !PackageId,
    typeKeyModule :: !Text,
    typeKeyNamespace :: !ResolutionNamespace
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TcTypeKey where
  rnf key = key `seq` ()

-- | Package, module, and axiom name. This identity is unique across modules.
data TcAxiomKey = TcAxiomKey
  { axiomKeyPackage :: !PackageId,
    axiomKeyModule :: !Text,
    axiomKeyName :: !Text
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TcAxiomKey where
  rnf key = key `seq` ()

type TcKindEnv = Map TcTypeKey TypeScheme

tyConPackageId :: TyCon -> PackageId
tyConPackageId (TyConInternal _ packageId _ _ _) = packageId

tyConModuleName :: TyCon -> Text
tyConModuleName (TyConInternal _ _ moduleName _ _) = moduleName

-- | Apply a type to an argument.
--
-- An application of a type constructor stays a constructor application,
-- and a saturated arrow becomes the function type. The arrow is a form of
-- its own rather than a type constructor, so recognising it here is a
-- pattern match: this module needs to know no library to normalise.
mkAppTy :: TcType -> TcType -> TcType
mkAppTy function argument =
  case function of
    TcTyCon tyCon arguments -> TcTyCon tyCon (arguments <> [argument])
    TcAppTy TcArrowTy domain -> TcFunTy domain argument
    _ -> TcAppTy function argument

tyConNamespace :: TyCon -> ResolutionNamespace
tyConNamespace (TyConInternal _ _ _ namespace _) = namespace

tyConKey :: TyCon -> TcTypeKey
tyConKey tyCon = TcTypeKey (tyConName tyCon) (tyConPackageId tyCon) (tyConModuleName tyCon) (tyConNamespace tyCon)

mkTyConWithOrigin :: PackageId -> Text -> Text -> Int -> TyCon
mkTyConWithOrigin packageId moduleName name =
  TyConInternal name packageId moduleName ResolutionNamespaceType

mkTyConWithNamespace :: ResolutionNamespace -> PackageId -> Text -> Text -> Int -> TyCon
mkTyConWithNamespace namespace packageId moduleName name =
  TyConInternal name packageId moduleName namespace

-- | Internal types. Kinds use this same representation.
data TcType
  = TcTyVar !TyVarId
  | TcMetaTv !Unique
  | TcTyCon !TyCon ![TcType]
  | -- | The function arrow @(->)@ itself, unapplied. It is a form of its
    -- own rather than a type constructor so that the module can recognise
    -- an arrow without being told which type constructor the arrow is: a
    -- saturated one is 'TcFunTy' and a partial one is @'TcAppTy'
    -- 'TcArrowTy' domain@, and nothing else may spell either.
    TcArrowTy
  | TcFunTy !TcType !TcType
  | TcForAllTy !TyVarId !TcType
  | TcQualTy ![Pred] !TcType
  | TcAppTy !TcType !TcType
  | -- | A type-level literal: @3@, @"abc"@ or @'x'@. Its kind is the
    -- literal's sort, which is a wired-in type constructor rather than
    -- anything the literal carries.
    TcTyLit !TyLit
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TcType where
  rnf ty = case ty of
    TcTyVar tyVar -> rnf tyVar
    TcMetaTv {} -> ()
    TcTyCon _ arguments -> rnf arguments
    TcArrowTy -> ()
    TcFunTy argument result -> rnf argument `seq` rnf result
    TcForAllTy tyVar body -> rnf tyVar `seq` rnf body
    TcQualTy predicates body -> rnf predicates `seq` rnf body
    TcAppTy function argument -> rnf function `seq` rnf argument
    TcTyLit {} -> ()

-- | A type-level literal, by sort. A natural stands at kind
-- @GHC.Num.Natural.Natural@, a symbol at @GHC.Types.Symbol@ and a
-- character at @GHC.Types.Char@, exactly as in GHC.
data TyLit
  = TyLitNat !Integer
  | TyLitSymbol !Text
  | TyLitChar !Char
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TyLit where
  rnf literal = literal `seq` ()

-- | A type scheme. The first binders are the *inferred* ones: variables
-- the checker invented, such as the kind of a parameter the source left
-- unannotated. A visible type application skips them, as GHC's
-- @forall {k}@ says. The *specified* binders follow, in source order; an
-- explicit @\@@ argument instantiates the first of those. Instantiation
-- allocates all of them, inferred first, since a specified binder's kind
-- may mention an inferred one.
data TypeScheme = Scheme ![TyVarId] ![TyVarId] ![Pred] !TcType
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData TypeScheme where
  rnf (Scheme inferred specified predicates body) =
    rnf inferred `seq` rnf specified `seq` rnf predicates `seq` rnf body

-- | Every binder of a scheme, inferred first. This is how a scheme is
-- read; building one names the two kinds of binder apart.
pattern ForAll :: [TyVarId] -> [Pred] -> TcType -> TypeScheme
pattern ForAll tyVars predicates body <- (schemeBinders -> (tyVars, predicates, body))

{-# COMPLETE ForAll #-}

schemeBinders :: TypeScheme -> ([TyVarId], [Pred], TcType)
schemeBinders (Scheme inferred specified predicates body) = (inferred <> specified, predicates, body)

-- | A scheme whose binders the source all wrote.
specifiedScheme :: [TyVarId] -> [Pred] -> TcType -> TypeScheme
specifiedScheme = Scheme []

-- | Rebuild a scheme part by part, keeping which binders are inferred.
traverseScheme :: (Applicative f) => (TyVarId -> f TyVarId) -> (Pred -> f Pred) -> (TcType -> f TcType) -> TypeScheme -> f TypeScheme
traverseScheme onTyVar onPred onBody (Scheme inferred specified predicates body) =
  Scheme <$> traverse onTyVar inferred <*> traverse onTyVar specified <*> traverse onPred predicates <*> onBody body

-- | The scheme of a default method's worker: the default signature's,
-- with the class predicate of the ordinary method signature in front.
defaultMethodWorkerScheme :: TypeScheme -> TypeScheme -> TypeScheme
defaultMethodWorkerScheme ordinaryScheme scheme@(Scheme inferred specified predicates body) =
  case ordinaryScheme of
    ForAll _ (classPredicate : _) _ -> Scheme inferred specified (classPredicate : predicates) body
    _ -> scheme

typeSchemeInferred :: TypeScheme -> [TyVarId]
typeSchemeInferred (Scheme inferred _ _ _) = inferred

typeSchemeSpecified :: TypeScheme -> [TyVarId]
typeSchemeSpecified (Scheme _ specified _ _) = specified

-- | Whether a type is a polytype: a leading quantifier or context. A
-- meta-variable never stands for a polytype, so an argument of such a
-- type is checked against it rather than inferred. A quantifier nested
-- under an arrow or a constructor does not make a type a polytype.
isPolyType :: TcType -> Bool
isPolyType TcForAllTy {} = True
isPolyType TcQualTy {} = True
isPolyType _ = False

-- | The body of a type scheme. This helper does not allocate skolem variables.
typeSchemeBody :: TypeScheme -> TcType
typeSchemeBody (ForAll _ _ body) = body

data Pred
  = ClassPred !TyCon ![TcType]
  | EqPred !TcType !TcType
  | QuantifiedPred ![TyVarId] ![Pred] !Pred
  | -- | An implicit parameter such as @?x :: Int@. The name keeps its @?@ prefix.
    IParamPred !Text !TcType
  | -- | A constraint whose head is a type family, and so says nothing yet
    -- about which class or equality it demands: @Assert (1 <=? n) msg@ with
    -- @n@ still a variable. It is kept whole until its arguments are known
    -- enough for the family to reduce, and is then reclassified.
    IrredPred !TcType
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData Pred where
  rnf predicate = case predicate of
    ClassPred _ arguments -> rnf arguments
    EqPred left right -> rnf left `seq` rnf right
    QuantifiedPred variables antecedents consequent ->
      rnf variables `seq` rnf antecedents `seq` rnf consequent
    IParamPred _ payload -> rnf payload
    IrredPred constraint -> rnf constraint

-- | Convert a constraint-kinded type to a predicate.
constraintTypeToPred :: TcKinds -> TcType -> Maybe Pred
constraintTypeToPred kinds ty =
  case collectForAllTypes ty of
    (variables@(_ : _), qualified) -> do
      let (antecedents, consequentType) =
            case qualified of
              TcQualTy predicates body -> (predicates, body)
              body -> ([], body)
      consequent <- atomicConstraintTypeToPred kinds consequentType
      pure (QuantifiedPred variables antecedents consequent)
    ([], body) -> atomicConstraintTypeToPred kinds body

atomicConstraintTypeToPred :: TcKinds -> TcType -> Maybe Pred
atomicConstraintTypeToPred kinds ty =
  case collectTypeApplications ty of
    (TcTyCon tyCon headArgs, arguments)
      | isEqualityTyCon kinds tyCon,
        [left, right] <- headArgs <> arguments ->
          Just (EqPred left right)
    (TcTyCon tyCon [payload], [])
      | isImplicitParamTyConName (tyConName tyCon) -> Just (IParamPred (tyConName tyCon) payload)
    (TcTyCon tyCon headArgs, arguments) ->
      Just (ClassPred tyCon (headArgs <> arguments))
    _ -> Nothing

-- | Whether a type constructor is the nominal equality constraint @~@.
--
-- A source module may declare a class of that name -- @class a ~ b@ is
-- accepted with TypeOperators -- and such a class imposes no equality, so
-- this compares identities rather than names.
isEqualityTyCon :: TcKinds -> TyCon -> Bool
isEqualityTyCon kinds tyCon =
  tyCon == kindsEqualityTyCon kinds
    || tyCon == mkTyConWithOrigin (tyConPackageId equality) (tyConModuleName equality) "~~" 2
  where
    equality = kindsEqualityTyCon kinds

-- | The name of the constraint type constructor for one implicit parameter.
isImplicitParamTyConName :: Text -> Bool
isImplicitParamTyConName = T.isPrefixOf "?"

collectForAllTypes :: TcType -> ([TyVarId], TcType)
collectForAllTypes (TcForAllTy variable body) =
  let (variables, result) = collectForAllTypes body
   in (variable : variables, result)
collectForAllTypes ty = ([], ty)

collectTypeApplications :: TcType -> (TcType, [TcType])
collectTypeApplications ty =
  case ty of
    TcAppTy function argument ->
      let (headType, arguments) = collectTypeApplications function
       in (headType, arguments <> [argument])
    _ -> (ty, [])

-- | The kind vocabulary, resolved to the module that declares it.
--
-- The type checker knows the names GHC gives these constructors --
-- @TYPE@, @Constraint@, @BoxedRep@, @IntRep@ and the rest -- but not the
-- package or module that declares them, and it must not guess: a kind it
-- built and a kind it resolved from an interface have to be the same
-- type. 'Aihc.Tc.Wiring.mkTcKinds' builds this table from the compiler's
-- wiring, and every kind below is built from it.
data TcKinds = TcKinds
  { -- | A kind constructor of one name and arity, such as @TYPE@.
    kindsTyCon :: Text -> Int -> TyCon,
    -- | A promoted constructor of the kind vocabulary of one name and
    -- arity, such as @BoxedRep@, @Lifted@ or @IntRep@.
    kindsDataCon :: Text -> Int -> TyCon,
    -- | The nominal equality constraint @~@.
    kindsEqualityTyCon :: TyCon,
    -- | The type constructor that 'TcArrowTy' denotes. Nothing inside the
    -- type checker needs it -- an arrow is recognised by its form -- but a
    -- partially applied arrow that leaves for the desugarer has to be
    -- named there like any other type constructor.
    kindsArrowTyCon :: TyCon,
    -- | The list type constructor, and the declaration that defines it.
    kindsListTyCon :: TyCon,
    kindsListDeclaration :: TyCon,
    -- | The promoted list constructors that a @TupleRep@ or @SumRep@ kind
    -- lists its fields with.
    kindsNilDataCon :: TyCon,
    kindsConsDataCon :: TyCon,
    -- | The kind of each sort of type-level literal: @Natural@, @Symbol@
    -- and @Char@.
    kindsNaturalTyCon :: TyCon,
    kindsSymbolTyCon :: TyCon,
    kindsCharTyCon :: TyCon
  }

-- | The kind of a type-level literal, which its sort alone decides.
tyLitKind :: TcKinds -> TyLit -> TcType
tyLitKind kinds literal = TcTyCon (tyLitKindTyCon kinds literal) []

-- | The type constructor that is the kind of a type-level literal.
tyLitKindTyCon :: TcKinds -> TyLit -> TyCon
tyLitKindTyCon kinds literal =
  case literal of
    TyLitNat {} -> kindsNaturalTyCon kinds
    TyLitSymbol {} -> kindsSymbolTyCon kinds
    TyLitChar {} -> kindsCharTyCon kinds

-- | The tables are functions, so a table shows as its name alone, as
-- 'Aihc.Tc.Wiring.TcWiring' does.
instance Show TcKinds where
  show _ = "TcKinds"

-- | The kind of ordinary lifted types, @TYPE (BoxedRep Lifted)@.
typeKind :: TcKinds -> TcType
typeKind kinds = mkTYPEKind kinds (liftedRep kinds)

-- | The kind of constraints.
constraintKind :: TcKinds -> TcType
constraintKind kinds = TcTyCon (kindsTyCon kinds "Constraint" 0) []

-- | The kind of runtime representations.
runtimeRepKind :: TcKinds -> TcType
runtimeRepKind kinds = TcTyCon (kindsTyCon kinds "RuntimeRep" 0) []

-- | The kind of types of one runtime representation.
mkTYPEKind :: TcKinds -> TcType -> TcType
mkTYPEKind kinds representation =
  TcTyCon (kindsTyCon kinds "TYPE" 1) [representation]

nullaryRep :: TcKinds -> Text -> TcType
nullaryRep kinds name = TcTyCon (kindsDataCon kinds name 0) []

-- | The lifted runtime representation, @BoxedRep Lifted@.
liftedRep :: TcKinds -> TcType
liftedRep kinds = boxedRep kinds (nullaryRep kinds "Lifted")

intRep, int8Rep, int16Rep, int32Rep, int64Rep :: TcKinds -> TcType
wordRep, word8Rep, word16Rep, word32Rep, word64Rep, addrRep :: TcKinds -> TcType
intRep kinds = nullaryRep kinds "IntRep"
int8Rep kinds = nullaryRep kinds "Int8Rep"
int16Rep kinds = nullaryRep kinds "Int16Rep"
int32Rep kinds = nullaryRep kinds "Int32Rep"
int64Rep kinds = nullaryRep kinds "Int64Rep"

wordRep kinds = nullaryRep kinds "WordRep"

word8Rep kinds = nullaryRep kinds "Word8Rep"

word16Rep kinds = nullaryRep kinds "Word16Rep"

word32Rep kinds = nullaryRep kinds "Word32Rep"

word64Rep kinds = nullaryRep kinds "Word64Rep"

addrRep kinds = nullaryRep kinds "AddrRep"

boxedRep :: TcKinds -> TcType -> TcType
boxedRep kinds levity = TcTyCon (kindsDataCon kinds "BoxedRep" 1) [levity]

-- | The runtime representation of an unboxed tuple of these fields.
tupleRep :: TcKinds -> [TcType] -> TcType
tupleRep kinds fields =
  TcTyCon (kindsDataCon kinds "TupleRep" 1) [dataConstructorList kinds fields]

-- | The runtime representation of an unboxed sum of these fields.
sumRep :: TcKinds -> [TcType] -> TcType
sumRep kinds fields =
  TcTyCon (kindsDataCon kinds "SumRep" 1) [dataConstructorList kinds fields]

dataConstructorList :: TcKinds -> [TcType] -> TcType
dataConstructorList kinds = foldr cons nil
  where
    nil = TcTyCon (kindsNilDataCon kinds) []
    cons field rest = TcTyCon (kindsConsDataCon kinds) [field, rest]

-- | Get a type kind from the complete type-constructor identity table.
typeKindInEnv :: TcKinds -> TcKindEnv -> TcType -> Either String TcType
typeKindInEnv kinds kindEnv = go
  where
    go rawType =
      case rawType of
        TcTyVar tyVar -> Right (tvKind tyVar)
        TcMetaTv {} -> Left "type still has a meta variable"
        TcTyCon tyCon arguments -> do
          scheme <-
            maybe
              (Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon)))
              Right
              (Map.lookup (tyConKey tyCon) kindEnv)
          applyArguments scheme arguments
        TcArrowTy -> Right (KFun (typeKind kinds) (KFun (typeKind kinds) (typeKind kinds)))
        TcFunTy {} -> Right (typeKind kinds)
        TcForAllTy _ body -> go body
        TcQualTy _ body -> go body
        TcAppTy function argument -> do
          functionKind <- go function
          applyKind functionKind argument
        TcTyLit literal -> Right (tyLitKind kinds literal)

    applyArguments (ForAll quantified _ body) = applyMany (map tvUnique quantified) body

    applyMany _ kind [] = Right kind
    applyMany quantified kind (argument : rest) = do
      kind' <- applyKindWith quantified kind argument
      applyMany quantified kind' rest

    applyKind = applyKindWith []

    applyKindWith quantified (TcFunTy formal result) argument = do
      actual <- go argument
      substitution <- matchKinds quantified (argumentKindVariables argument) formal actual
      Right (applySubst substitution result)
    applyKindWith _ kind _ = Left ("type application uses a non-function kind: " <> show kind)

    argumentKindVariables argument =
      case argument of
        TcTyCon tyCon _ ->
          case Map.lookup (tyConKey tyCon) kindEnv of
            Just (ForAll variables _ _) -> map tvUnique variables
            Nothing -> []
        TcAppTy function _ -> argumentKindVariables function
        _ -> []

    matchKinds quantified argumentQuantified formal actual =
      case (formal, actual) of
        (TcTyVar tyVar, _)
          | tvUnique tyVar `elem` quantified -> Right (Map.singleton (tvUnique tyVar) actual)
        (_, TcTyVar tyVar)
          | tvUnique tyVar `elem` argumentQuantified -> Right Map.empty
        (KTYPE formalRep, KTYPE actualRep) -> recur formalRep actualRep
        (TcFunTy left right, TcFunTy left' right') ->
          Map.union <$> recur left left' <*> recur right right'
        (TcTyCon left leftArguments, TcTyCon right rightArguments)
          | left == right,
            length leftArguments == length rightArguments ->
              Map.unions <$> zipWithM recur leftArguments rightArguments
        _
          | formal == actual -> Right Map.empty
          | otherwise -> Left ("kind mismatch: expected " <> show formal <> ", got " <> show actual)
      where
        recur = matchKinds quantified argumentQuantified

runtimeRepOfTypeInEnv :: TcKinds -> TcKindEnv -> TcType -> Either String TcType
runtimeRepOfTypeInEnv kinds kindEnv ty = typeKindInEnv kinds kindEnv ty >>= runtimeRepFromKind

isUnliftedTypeInEnv :: TcKinds -> TcKindEnv -> TcType -> Bool
isUnliftedTypeInEnv kinds kindEnv ty =
  case runtimeRepOfTypeInEnv kinds kindEnv ty of
    Right representation -> not (matchesLiftedRuntimeRep representation)
    Left _ -> False

-- | Apply a type-variable substitution to a type.
applySubst :: Map Unique TcType -> TcType -> TcType
applySubst substitution = go
  where
    go ty =
      case ty of
        TcTyVar tyVar -> Map.findWithDefault (TcTyVar (setTyVarKind (go (tvKind tyVar)) tyVar)) (tvUnique tyVar) substitution
        TcMetaTv {} -> ty
        TcArrowTy -> ty
        TcTyLit {} -> ty
        TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments)
        TcFunTy argument result -> TcFunTy (go argument) (go result)
        TcForAllTy tyVar body ->
          TcForAllTy (setTyVarKind (go (tvKind tyVar)) tyVar) (applySubst (Map.delete (tvUnique tyVar) substitution) body)
        TcQualTy predicates body -> TcQualTy (map (applySubstPred substitution) predicates) (go body)
        TcAppTy function argument -> mkAppTy (go function) (go argument)

-- | Apply a type-variable substitution to a predicate.
applySubstPred :: Map Unique TcType -> Pred -> Pred
applySubstPred substitution predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className (map (applySubst substitution) arguments)
    EqPred left right -> EqPred (applySubst substitution left) (applySubst substitution right)
    IParamPred name payload -> IParamPred name (applySubst substitution payload)
    IrredPred constraint -> IrredPred (applySubst substitution constraint)
    QuantifiedPred variables antecedents consequent ->
      let scopedSubstitution = foldr (Map.delete . tvUnique) substitution variables
       in QuantifiedPred
            [setTyVarKind (applySubst scopedSubstitution (tvKind variable)) variable | variable <- variables]
            (map (applySubstPred scopedSubstitution) antecedents)
            (applySubstPred scopedSubstitution consequent)

-- | Whether a type mentions a meta-variable, without building the list of
-- them. A caller that only asks whether there are any pays for the walk
-- alone; 'Aihc.Tc.Generalize.collectMetaVars' answers the same question
-- with a list, and these two must agree on where they look.
typeMentionsMeta :: TcType -> Bool
typeMentionsMeta ty =
  case ty of
    TcMetaTv {} -> True
    TcTyVar {} -> False
    TcArrowTy -> False
    TcTyLit {} -> False
    TcTyCon _ arguments -> any typeMentionsMeta arguments
    TcFunTy argument result -> typeMentionsMeta argument || typeMentionsMeta result
    TcForAllTy _ body -> typeMentionsMeta body
    TcQualTy predicates body -> any predicateMentionsMeta predicates || typeMentionsMeta body
    TcAppTy function argument -> typeMentionsMeta function || typeMentionsMeta argument

-- | Whether a predicate mentions a meta-variable.
predicateMentionsMeta :: Pred -> Bool
predicateMentionsMeta predicate =
  case predicate of
    ClassPred _ arguments -> any typeMentionsMeta arguments
    EqPred left right -> typeMentionsMeta left || typeMentionsMeta right
    IParamPred _ payload -> typeMentionsMeta payload
    IrredPred constraint -> typeMentionsMeta constraint
    QuantifiedPred variables antecedents consequent ->
      any (typeMentionsMeta . tvKind) variables
        || any predicateMentionsMeta antecedents
        || predicateMentionsMeta consequent

-- | Whether a type mentions one type variable.
--
-- A type variable can occur in the kind of another type variable. An
-- escape check must find that occurrence, so the walk looks in each kind
-- that it finds.
typeMentionsTyVar :: TyVarId -> TcType -> Bool
typeMentionsTyVar target ty =
  case ty of
    TcTyVar tyVar -> sameTyVar tyVar target || kindMentionsUnique (tvUnique target) (tvKind tyVar)
    TcMetaTv {} -> False
    TcArrowTy -> False
    TcTyLit {} -> False
    TcTyCon _ arguments -> any (typeMentionsTyVar target) arguments
    TcFunTy argument result -> typeMentionsTyVar target argument || typeMentionsTyVar target result
    TcForAllTy tyVar body -> not (sameTyVar tyVar target) && typeMentionsTyVar target body
    TcQualTy predicates body -> any (predicateMentionsTyVar target) predicates || typeMentionsTyVar target body
    TcAppTy function argument -> typeMentionsTyVar target function || typeMentionsTyVar target argument

-- | Whether a predicate mentions one type variable.
predicateMentionsTyVar :: TyVarId -> Pred -> Bool
predicateMentionsTyVar target predicate =
  case predicate of
    ClassPred _ arguments -> any (typeMentionsTyVar target) arguments
    EqPred left right -> typeMentionsTyVar target left || typeMentionsTyVar target right
    IParamPred _ payload -> typeMentionsTyVar target payload
    IrredPred constraint -> typeMentionsTyVar target constraint
    QuantifiedPred variables antecedents consequent ->
      not (any (sameTyVar target) variables)
        && (any (predicateMentionsTyVar target) antecedents || predicateMentionsTyVar target consequent)

-- | Whether two occurrences are one variable. Occurrences of a variable can
-- carry different kinds (a given kind refinement rewrites the kinds of the
-- occurrences it reaches), so this asks about the identity, not 'Eq'.
sameTyVar :: TyVarId -> TyVarId -> Bool
sameTyVar left right = tyVarIdentity left == tyVarIdentity right

-- | A type with the kinds of its variables left out: what the solver means
-- by one type. Two types of one shape are the same type however their
-- variable occurrences are kinded, so a shape is the key to use where types
-- are matched against one another. 'Eq' on 'TcType' is finer: it tells two
-- differently kinded occurrences of a variable apart.
data TypeShape
  = ShapeTyVar !Unique !Text
  | ShapeMetaTv !Unique
  | ShapeTyCon !TyCon ![TypeShape]
  | ShapeArrowTy
  | ShapeFunTy !TypeShape !TypeShape
  | ShapeForAllTy !Unique !Text !TypeShape
  | ShapeQualTy ![PredShape] !TypeShape
  | ShapeAppTy !TypeShape !TypeShape
  | ShapeTyLit !TyLit
  deriving (Eq, Ord, Show)

-- | A predicate with the kinds of its variables left out.
data PredShape
  = ShapeClassPred !TyCon ![TypeShape]
  | ShapeEqPred !TypeShape !TypeShape
  | ShapeQuantifiedPred ![(Unique, Text)] ![PredShape] !PredShape
  | ShapeIParamPred !Text !TypeShape
  | ShapeIrredPred !TypeShape
  deriving (Eq, Ord, Show)

typeShape :: TcType -> TypeShape
typeShape ty =
  case ty of
    TcTyVar tyVar -> ShapeTyVar (tvUnique tyVar) (tvName tyVar)
    TcMetaTv unique -> ShapeMetaTv unique
    TcTyCon tyCon arguments -> ShapeTyCon tyCon (map typeShape arguments)
    TcArrowTy -> ShapeArrowTy
    TcFunTy argument result -> ShapeFunTy (typeShape argument) (typeShape result)
    TcForAllTy tyVar body -> ShapeForAllTy (tvUnique tyVar) (tvName tyVar) (typeShape body)
    TcQualTy predicates body -> ShapeQualTy (map predShape predicates) (typeShape body)
    TcAppTy function argument -> ShapeAppTy (typeShape function) (typeShape argument)
    TcTyLit literal -> ShapeTyLit literal

predShape :: Pred -> PredShape
predShape predicate =
  case predicate of
    ClassPred tyCon arguments -> ShapeClassPred tyCon (map typeShape arguments)
    EqPred left right -> ShapeEqPred (typeShape left) (typeShape right)
    QuantifiedPred variables antecedents consequent ->
      ShapeQuantifiedPred (map tyVarIdentity variables) (map predShape antecedents) (predShape consequent)
    IParamPred name payload -> ShapeIParamPred name (typeShape payload)
    IrredPred constraint -> ShapeIrredPred (typeShape constraint)

-- | Whether two types are one type, whatever kinds their variable
-- occurrences carry.
sameType :: TcType -> TcType -> Bool
sameType left right = typeShape left == typeShape right

-- | Whether two predicates are one predicate, whatever kinds their
-- variable occurrences carry.
samePred :: Pred -> Pred -> Bool
samePred left right = predShape left == predShape right

-- | Whether a kind mentions one unique. A type variable matches by its
-- unique alone, because a kind can hold a different copy of it.
kindMentionsUnique :: Unique -> TcType -> Bool
kindMentionsUnique target kind =
  case kind of
    TcTyVar tyVar -> tvUnique tyVar == target || kindMentionsUnique target (tvKind tyVar)
    TcMetaTv unique -> unique == target
    TcArrowTy -> False
    TcTyLit {} -> False
    TcTyCon _ arguments -> any (kindMentionsUnique target) arguments
    TcFunTy argument result -> kindMentionsUnique target argument || kindMentionsUnique target result
    TcForAllTy tyVar body -> tvUnique tyVar /= target && kindMentionsUnique target body
    TcQualTy predicates body -> any (predicateMentionsUnique target) predicates || kindMentionsUnique target body
    TcAppTy function argument -> kindMentionsUnique target function || kindMentionsUnique target argument
  where
    predicateMentionsUnique unique predicate =
      case predicate of
        ClassPred _ arguments -> any (kindMentionsUnique unique) arguments
        EqPred left right -> kindMentionsUnique unique left || kindMentionsUnique unique right
        IParamPred _ payload -> kindMentionsUnique unique payload
        IrredPred constraint -> kindMentionsUnique unique constraint
        QuantifiedPred variables antecedents consequent ->
          all ((/= unique) . tvUnique) variables
            && (any (predicateMentionsUnique unique) antecedents || predicateMentionsUnique unique consequent)

-- The kind patterns recognise a kind by its namespace and its name, so
-- that a kind built here and a kind resolved from an interface match each
-- other. They only match: a kind is built from a 'TcKinds'.
pattern KTYPE :: TcType -> TcType
pattern KTYPE representation <- (matchTYPEKind -> Just representation)

pattern KConstraint, KRuntimeRep, KLevity, KVecCount, KVecElem, KType :: TcType
pattern KConstraint <- (matchesNullary ResolutionNamespaceType "Constraint" -> True)
pattern KRuntimeRep <- (matchesNullary ResolutionNamespaceType "RuntimeRep" -> True)
pattern KLevity <- (matchesNullary ResolutionNamespaceType "Levity" -> True)
pattern KVecCount <- (matchesNullary ResolutionNamespaceType "VecCount" -> True)
pattern KVecElem <- (matchesNullary ResolutionNamespaceType "VecElem" -> True)
pattern KType <- (matchesLiftedTypeKind -> True)

pattern KFun :: TcType -> TcType -> TcType
pattern KFun argument result = TcFunTy argument result

pattern KMeta :: Unique -> TcType
pattern KMeta unique = TcMetaTv unique

matchTYPEKind :: TcType -> Maybe TcType
matchTYPEKind kind =
  case kind of
    TcTyCon tyCon [representation]
      | tyConName tyCon == "TYPE" -> Just representation
    _ -> Nothing

matchesLiftedTypeKind :: TcType -> Bool
matchesLiftedTypeKind = maybe False matchesLiftedRuntimeRep . matchTYPEKind

matchesLiftedRuntimeRep :: TcType -> Bool
matchesLiftedRuntimeRep representation =
  case representation of
    TcTyCon boxed [TcTyCon levity []] ->
      tyConNamespace boxed == ResolutionNamespaceTerm
        && tyConName boxed == "BoxedRep"
        && tyConNamespace levity == ResolutionNamespaceTerm
        && tyConName levity == "Lifted"
    _ -> False

pattern BoxedRep :: TcType -> TcType
pattern BoxedRep levity <- (matchUnaryRep "BoxedRep" -> Just levity)

pattern TupleRep :: [TcType] -> TcType
pattern TupleRep fields <- (matchListRep "TupleRep" -> Just fields)

pattern SumRep :: [TcType] -> TcType
pattern SumRep fields <- (matchListRep "SumRep" -> Just fields)

pattern VecRep :: TcType -> TcType -> TcType
pattern VecRep count element <- (matchBinaryRep "VecRep" -> Just (count, element))

pattern Lifted, Unlifted :: TcType
pattern Lifted <- (matchesNullary ResolutionNamespaceTerm "Lifted" -> True)
pattern Unlifted <- (matchesNullary ResolutionNamespaceTerm "Unlifted" -> True)

pattern IntRep, Int8Rep, Int16Rep, Int32Rep, Int64Rep :: TcType

pattern WordRep, Word8Rep, Word16Rep, Word32Rep, Word64Rep :: TcType

pattern AddrRep, FloatRep, DoubleRep :: TcType

pattern IntRep <- (matchesNullary ResolutionNamespaceTerm "IntRep" -> True)

pattern Int8Rep <- (matchesNullary ResolutionNamespaceTerm "Int8Rep" -> True)

pattern Int16Rep <- (matchesNullary ResolutionNamespaceTerm "Int16Rep" -> True)

pattern Int32Rep <- (matchesNullary ResolutionNamespaceTerm "Int32Rep" -> True)

pattern Int64Rep <- (matchesNullary ResolutionNamespaceTerm "Int64Rep" -> True)

pattern WordRep <- (matchesNullary ResolutionNamespaceTerm "WordRep" -> True)

pattern Word8Rep <- (matchesNullary ResolutionNamespaceTerm "Word8Rep" -> True)

pattern Word16Rep <- (matchesNullary ResolutionNamespaceTerm "Word16Rep" -> True)

pattern Word32Rep <- (matchesNullary ResolutionNamespaceTerm "Word32Rep" -> True)

pattern Word64Rep <- (matchesNullary ResolutionNamespaceTerm "Word64Rep" -> True)

pattern AddrRep <- (matchesNullary ResolutionNamespaceTerm "AddrRep" -> True)

pattern FloatRep <- (matchesNullary ResolutionNamespaceTerm "FloatRep" -> True)

pattern DoubleRep <- (matchesNullary ResolutionNamespaceTerm "DoubleRep" -> True)

matchUnaryRep :: Text -> TcType -> Maybe TcType
matchUnaryRep expected (TcTyCon tyCon [argument])
  | tyConNamespace tyCon == ResolutionNamespaceTerm,
    tyConName tyCon == expected =
      Just argument
matchUnaryRep _ _ = Nothing

matchBinaryRep :: Text -> TcType -> Maybe (TcType, TcType)
matchBinaryRep expected (TcTyCon tyCon [left, right])
  | tyConNamespace tyCon == ResolutionNamespaceTerm,
    tyConName tyCon == expected =
      Just (left, right)
matchBinaryRep _ _ = Nothing

matchListRep :: Text -> TcType -> Maybe [TcType]
matchListRep expected (TcTyCon tyCon [listType])
  | tyConNamespace tyCon == ResolutionNamespaceTerm,
    tyConName tyCon == expected =
      decodeDataConstructorList listType
matchListRep _ _ = Nothing

decodeDataConstructorList :: TcType -> Maybe [TcType]
decodeDataConstructorList ty =
  case ty of
    TcTyCon tyCon []
      | tyConNamespace tyCon == ResolutionNamespaceTerm,
        tyConName tyCon == "[]" ->
          Just []
    TcTyCon tyCon [field, rest]
      | tyConNamespace tyCon == ResolutionNamespaceTerm,
        tyConName tyCon == ":" ->
          (field :) <$> decodeDataConstructorList rest
    _ -> Nothing

matchesNullary :: ResolutionNamespace -> Text -> TcType -> Bool
matchesNullary namespace expected (TcTyCon tyCon []) =
  tyConNamespace tyCon == namespace && tyConName tyCon == expected
matchesNullary _ _ _ = False

runtimeRepFromKind :: TcType -> Either String TcType
runtimeRepFromKind kind =
  case kind of
    TcTyCon tyCon [representation]
      | tyConName tyCon == "TYPE" -> Right representation
    _ -> Left ("type does not have a runtime representation: " <> show kind)

-- | Test whether a runtime representation has a fixed machine layout.
isFixedRuntimeRep :: TcType -> Bool
isFixedRuntimeRep representation =
  case representation of
    BoxedRep levity -> promotedConstructorIsOneOf ["Lifted", "Unlifted"] levity
    TupleRep fields -> all isFixedRuntimeRep fields
    SumRep fields -> all isFixedRuntimeRep fields
    VecRep count element ->
      promotedConstructorIsOneOf ["Vec2", "Vec4", "Vec8", "Vec16", "Vec32", "Vec64"] count
        && promotedConstructorIsOneOf
          [ "Int8ElemRep",
            "Int16ElemRep",
            "Int32ElemRep",
            "Int64ElemRep",
            "Word8ElemRep",
            "Word16ElemRep",
            "Word32ElemRep",
            "Word64ElemRep",
            "FloatElemRep",
            "DoubleElemRep"
          ]
          element
    IntRep -> True
    Int8Rep -> True
    Int16Rep -> True
    Int32Rep -> True
    Int64Rep -> True
    WordRep -> True
    Word8Rep -> True
    Word16Rep -> True
    Word32Rep -> True
    Word64Rep -> True
    AddrRep -> True
    FloatRep -> True
    DoubleRep -> True
    _ -> False
  where
    -- The representation matchers this function is built from compare a
    -- namespace and a name. Recognise a promoted constructor the same way,
    -- so that the whole function reads one identity the same way.
    promotedConstructorIsOneOf names ty =
      case ty of
        TcTyCon tyCon [] ->
          tyConNamespace tyCon == ResolutionNamespaceTerm
            && tyConName tyCon `elem` names
        _ -> False

-- | Kind arguments for a checked type constructor application.
data TcTypeApplicationKinds = TcTypeApplicationKinds
  { tcInvisibleKindSubstitution :: Map Unique TcType,
    tcVisibleArgumentKinds :: [TcType],
    -- | Why an argument contributed nothing to the substitution. An
    -- argument whose kind cannot be worked out is skipped rather than
    -- rejected, because the kinds of the other arguments often bind
    -- every variable on their own. When they do not, the caller reports
    -- a kind variable it cannot infer, and these are the reasons it
    -- could not: without them the report names a variable but not the
    -- argument that failed to bind it.
    tcSkippedArguments :: [String]
  }
  deriving (Eq, Show, Read)

typeApplicationKinds :: TcKinds -> TcKindEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String TcTypeApplicationKinds
typeApplicationKinds kinds kindEnv tyCon arguments expectedKind = do
  scheme <- maybe (Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon))) Right (Map.lookup (tyConKey tyCon) kindEnv)
  let ForAll quantified _ resultKind = scheme
  let quantifiedUniques = map tvUnique quantified
      (argumentSubstitution, remainingKind, skipped) = go quantifiedUniques 0 Map.empty resultKind arguments
      resultSubstitution =
        case expectedKind of
          Just expected -> matchKind quantifiedUniques remainingKind expected
          Nothing -> Map.empty
      substitution = argumentSubstitution <> resultSubstitution
  pure (TcTypeApplicationKinds substitution (argumentKinds (applySubst substitution resultKind)) skipped)
  where
    argumentKinds (KFun argument result) = argument : argumentKinds result
    argumentKinds _ = []
    go quantifiedUniques position substitution (KFun formal result) (argument : rest) =
      case typeKindInEnv kinds kindEnv argument of
        Right argumentKind ->
          let found = matchKind quantifiedUniques (applySubst substitution formal) argumentKind
           in go quantifiedUniques (position + 1) (substitution <> found) (applySubst found result) rest
        Left reason ->
          let (substitution', kind, skipped) = go quantifiedUniques (position + 1) substitution result rest
           in (substitution', kind, skippedArgument position reason : skipped)
    go _ _ substitution kind _ = (substitution, applySubst substitution kind, [])

    -- The caller prints the arguments themselves, so name the one that
    -- failed by position rather than repeating it.
    skippedArgument position reason =
      "argument " <> show (position + 1 :: Int) <> " has no usable kind: " <> reason

    matchKind quantifiedUniques (TcTyVar tyVar) actual
      | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) actual
    matchKind quantifiedUniques (KTYPE (TcTyVar tyVar)) (KTYPE runtimeRep)
      | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) runtimeRep
    matchKind quantifiedUniques (KFun left right) (KFun left' right') =
      matchKind quantifiedUniques left left' <> matchKind quantifiedUniques right right'
    matchKind quantifiedUniques (TcTyCon left formalArguments) (TcTyCon right actualArguments)
      | left == right,
        length formalArguments == length actualArguments =
          Map.unions (zipWith (matchKind quantifiedUniques) formalArguments actualArguments)
    matchKind _ _ _ = Map.empty
