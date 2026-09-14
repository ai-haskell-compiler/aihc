-- | Give the type variables of an imported interface uniques of this run.
--
-- A 'Aihc.Tc.Types.TyVarId' is a name and a unique, and its kind lives at
-- the binder that introduces it. Reading an occurrence's kind therefore
-- means looking the variable up by its unique, and that only works when a
-- unique names one variable. Uniques are allocated per run, so two modules
-- of an imported interface can each have numbered a variable 6, and a
-- checker that reads them both would see one variable with two kinds.
--
-- Each fact of an interface is closed: its free type variables are the ones
-- its own binders introduce. Renumbering a fact therefore settles it on its
-- own. Only the facts that share a unique with another fact that means a
-- different variable need it, and those are few, so the pass finds them
-- first and rebuilds only those.
module Aihc.Tc.Rename
  ( renameInterfaceTyVars,
  )
where

import Aihc.Tc.Env
import Aihc.Tc.Types
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)

-- | The renaming state: the variables renumbered in the current fact, the
-- binders the renumbered facts introduced, and the next unique to hand out.
data RenameState = RenameState
  { renameSeen :: !(Map Unique Unique),
    renameBinders :: ![TcTyVarBinder],
    renameNext :: !Int
  }

type Rename = State RenameState

-- | Renumber the type variables of the facts that need it, and give back
-- the kind of every variable the interface binds together with the first
-- unique that is still free.
--
-- The kinds come back with the interface because finding the clashes means
-- reading every binder already, and that table is what the checker reads an
-- occurrence's kind from.
renameInterfaceTyVars ::
  Int ->
  Map key TypeScheme ->
  Map key2 TyConInfo ->
  Map key3 DataTypeInfo ->
  Map key4 ClassInfo ->
  Map key5 InstanceInfo ->
  Map key6 DataFamilyInstanceInfo ->
  Map key7 TypeFamilyInstanceInfo ->
  Map key8 PatSynInfo ->
  ( Map key TypeScheme,
    Map key2 TyConInfo,
    Map key3 DataTypeInfo,
    Map key4 ClassInfo,
    Map key5 InstanceInfo,
    Map key6 DataFamilyInstanceInfo,
    Map key7 TypeFamilyInstanceInfo,
    Map key8 PatSynInfo,
    TcTyVarKinds,
    Int
  )
renameInterfaceTyVars nextUnique terms tyCons dataTypes classes instances dataFamilies typeFamilies patSyns =
  ( terms',
    tyCons',
    dataTypes',
    classes',
    instances',
    dataFamilies',
    typeFamilies',
    patSyns',
    kinds,
    renameNext final
  )
  where
    ( (terms', tyCons', dataTypes', classes', instances', dataFamilies', typeFamilies', patSyns'),
      final
      ) =
        runState action (RenameState Map.empty [] (ceiling' + 1))

    action = do
      renamedTerms <- for terms (maybeRename schemeBinders renameScheme)
      renamedTyCons <- for tyCons (maybeRename tyConBinders renameTyConInfo)
      renamedDataTypes <- for dataTypes (maybeRename dataTypeBinders renameDataTypeInfo)
      renamedClasses <- for classes (maybeRename classBinders renameClassInfo)
      renamedInstances <- for instances (maybeRename instanceBinders renameInstanceInfo)
      renamedDataFamilies <- for dataFamilies (maybeRename dataFamilyBinders renameDataFamilyInfo)
      renamedTypeFamilies <- for typeFamilies (maybeRename typeFamilyBinders renameTypeFamilyInfo)
      renamedPatSyns <- for patSyns (maybeRename patSynBinders renamePatSynInfo)
      pure
        ( renamedTerms,
          renamedTyCons,
          renamedDataTypes,
          renamedClasses,
          renamedInstances,
          renamedDataFamilies,
          renamedTypeFamilies,
          renamedPatSyns
        )

    -- Only a fact that uses a clashing unique is rebuilt. Each fact has a
    -- renaming of its own: a unique names one variable inside a fact, but
    -- not across facts.
    maybeRename :: (a -> [TcTyVarBinder]) -> (a -> Rename a) -> a -> Rename a
    maybeRename binders rename value
      | any ((`Set.member` clashing) . tvbUnique) (binders value) = do
          _ <- state (\current -> ((), current {renameSeen = Map.empty}))
          rename value
      | otherwise = pure value

    -- Every binder of every fact. Reading them all is what finds the
    -- clashes, and what gives the kinds the checker reads.
    allBinders =
      concatMap schemeBinders (Map.elems terms)
        <> concatMap tyConBinders (Map.elems tyCons)
        <> concatMap dataTypeBinders (Map.elems dataTypes)
        <> concatMap classBinders (Map.elems classes)
        <> concatMap instanceBinders (Map.elems instances)
        <> concatMap dataFamilyBinders (Map.elems dataFamilies)
        <> concatMap typeFamilyBinders (Map.elems typeFamilies)
        <> concatMap patSynBinders (Map.elems patSyns)

    -- A unique that two facts give different kinds names two variables, and
    -- every fact that uses it is renumbered. A unique that they agree on
    -- names one variable however many facts wrote it down.
    (settled, clashing) = foldr collect (Map.empty, Set.empty) allBinders
    collect binder (known, clashes) =
      case Map.lookup (tvbUnique binder) known of
        Just kind
          | kind /= tvbKind binder -> (known, Set.insert (tvbUnique binder) clashes)
        _ -> (Map.insert (tvbUnique binder) (tvbKind binder) known, clashes)

    -- The renumbered facts take uniques above every one the interface uses,
    -- so the numbers they take are free.
    ceiling' = foldr (max . uniqueValue . tvbUnique) (nextUnique - 1) allBinders
    uniqueValue (Unique value) = value

    kinds =
      Map.union
        (Map.fromList [(tvbUnique binder, tvbKind binder) | binder <- renameBinders final])
        (Map.withoutKeys settled clashing)

-- | The binders of each kind of fact, in the order the renaming visits
-- them. A binder's own kind can bind further variables, as @(a :: k)@ does.
factBinders :: [TcTyVarBinder] -> [TcTyVarBinder]
factBinders = concatMap (\binder -> binder : typeBinders (tvbKind binder))

schemeBinders :: TypeScheme -> [TcTyVarBinder]
schemeBinders (ForAll variables predicates body) =
  factBinders variables <> concatMap predBinders predicates <> typeBinders body

tyConBinders :: TyConInfo -> [TcTyVarBinder]
tyConBinders info =
  schemeBinders (tciKindScheme info) <> maybe [] synonymBinders (tciTypeSynonym info)

synonymBinders :: TypeSynonymInfo -> [TcTyVarBinder]
synonymBinders synonym = factBinders (tsiParams synonym) <> foldMap typeBinders (tsiBody synonym)

dataTypeBinders :: DataTypeInfo -> [TcTyVarBinder]
dataTypeBinders info =
  factBinders (dtiTyVars info)
    <> typeBinders (dtiResultKind info)
    <> concatMap dataConBinders (dtiConstructors info)

dataConBinders :: DataConInfo -> [TcTyVarBinder]
dataConBinders info =
  factBinders (dciUnivTyVars info)
    <> factBinders (dciExTyVars info)
    <> concatMap predBinders (dciTheta info)
    <> concatMap (typeBinders . dcfiType) (dciFields info)
    <> typeBinders (dciResTy info)

classBinders :: ClassInfo -> [TcTyVarBinder]
classBinders info =
  factBinders (ciKindTyVars info)
    <> factBinders (ciTyVars info)
    <> concatMap typeBinders (ciSuperClassTypes info)
    <> concatMap (schemeBinders . snd) (ciMethods info)
    <> concatMap (schemeBinders . snd) (ciDefaultSignatures info)
    <> concatMap (foldMap typeFamilyBinders . atiDefault) (ciAssociatedTypes info)

instanceBinders :: InstanceInfo -> [TcTyVarBinder]
instanceBinders info =
  factBinders (iiTyVars info)
    <> typeBinders (iiDictType info)
    <> concatMap predBinders (iiContext info)
    <> concatMap typeBinders (iiHead info)

dataFamilyBinders :: DataFamilyInstanceInfo -> [TcTyVarBinder]
dataFamilyBinders info = factBinders (dfiiTyVars info) <> typeBinders (dfiiFamilyType info)

typeFamilyBinders :: TypeFamilyInstanceInfo -> [TcTyVarBinder]
typeFamilyBinders info =
  factBinders (tfiiTyVars info) <> typeBinders (tfiiLeft info) <> typeBinders (tfiiRight info)

patSynBinders :: PatSynInfo -> [TcTyVarBinder]
patSynBinders info =
  schemeBinders (psiScheme info)
    <> concatMap predBinders (psiReqTheta info)
    <> concatMap predBinders (psiProvTheta info)

typeBinders :: TcType -> [TcTyVarBinder]
typeBinders ty =
  case ty of
    TcTyVar {} -> []
    TcMetaTv {} -> []
    TcArrowTy -> []
    TcTyCon _ arguments -> concatMap typeBinders arguments
    TcFunTy argument result -> typeBinders argument <> typeBinders result
    TcForAllTy quantified body -> factBinders [quantified] <> typeBinders body
    TcQualTy predicates body -> concatMap predBinders predicates <> typeBinders body
    TcAppTy function argument -> typeBinders function <> typeBinders argument

predBinders :: Pred -> [TcTyVarBinder]
predBinders predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeBinders arguments
    EqPred left right -> typeBinders left <> typeBinders right
    IParamPred _ payload -> typeBinders payload
    QuantifiedPred variables antecedents consequent ->
      factBinders variables <> concatMap predBinders antecedents <> predBinders consequent

tyVar :: TyVarId -> Rename TyVarId
tyVar variable = state $ \current ->
  case Map.lookup (tvUnique variable) (renameSeen current) of
    Just fresh -> (mkTyVarId (tvName variable) fresh, current)
    Nothing ->
      let fresh = Unique (renameNext current)
       in ( mkTyVarId (tvName variable) fresh,
            current
              { renameSeen = Map.insert (tvUnique variable) fresh (renameSeen current),
                renameNext = renameNext current + 1
              }
          )

renameTyVarBinder :: TcTyVarBinder -> Rename TcTyVarBinder
renameTyVarBinder value = do
  renamed <- mkTyVarBinder <$> tyVar (tvbTyVar value) <*> renameType (tvbKind value)
  state (\current -> (renamed, current {renameBinders = renamed : renameBinders current}))

renameType :: TcType -> Rename TcType
renameType ty =
  case ty of
    TcTyVar variable -> TcTyVar <$> tyVar variable
    TcMetaTv {} -> pure ty
    TcArrowTy -> pure ty
    TcTyCon tyCon arguments -> TcTyCon tyCon <$> traverse renameType arguments
    TcFunTy argument result -> TcFunTy <$> renameType argument <*> renameType result
    TcForAllTy value body -> TcForAllTy <$> renameTyVarBinder value <*> renameType body
    TcQualTy predicates body -> TcQualTy <$> traverse renamePred predicates <*> renameType body
    TcAppTy function argument -> TcAppTy <$> renameType function <*> renameType argument

renamePred :: Pred -> Rename Pred
renamePred predicate =
  case predicate of
    ClassPred tyCon arguments -> ClassPred tyCon <$> traverse renameType arguments
    EqPred left right -> EqPred <$> renameType left <*> renameType right
    IParamPred name payload -> IParamPred name <$> renameType payload
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred
        <$> traverse renameTyVarBinder variables
        <*> traverse renamePred antecedents
        <*> renamePred consequent

renameScheme :: TypeScheme -> Rename TypeScheme
renameScheme (ForAll variables predicates body) =
  ForAll <$> traverse renameTyVarBinder variables <*> traverse renamePred predicates <*> renameType body

renameTyConInfo :: TyConInfo -> Rename TyConInfo
renameTyConInfo info = do
  scheme <- renameScheme (tciKindScheme info)
  synonym <- traverse renameSynonym (tciTypeSynonym info)
  pure info {tciKindScheme = scheme, tciTypeSynonym = synonym}

renameSynonym :: TypeSynonymInfo -> Rename TypeSynonymInfo
renameSynonym info = do
  params <- traverse renameTyVarBinder (tsiParams info)
  body <- traverse renameType (tsiBody info)
  pure info {tsiParams = params, tsiBody = body}

renameDataTypeInfo :: DataTypeInfo -> Rename DataTypeInfo
renameDataTypeInfo info = do
  tyVars <- traverse renameTyVarBinder (dtiTyVars info)
  resultKind <- renameType (dtiResultKind info)
  constructors <- traverse renameDataConInfo (dtiConstructors info)
  pure info {dtiTyVars = tyVars, dtiResultKind = resultKind, dtiConstructors = constructors}

renameDataConInfo :: DataConInfo -> Rename DataConInfo
renameDataConInfo info = do
  univTyVars <- traverse renameTyVarBinder (dciUnivTyVars info)
  exTyVars <- traverse renameTyVarBinder (dciExTyVars info)
  theta <- traverse renamePred (dciTheta info)
  fields <- traverse renameField (dciFields info)
  resTy <- renameType (dciResTy info)
  pure
    info
      { dciUnivTyVars = univTyVars,
        dciExTyVars = exTyVars,
        dciTheta = theta,
        dciFields = fields,
        dciResTy = resTy
      }
  where
    renameField field = do
      fieldType <- renameType (dcfiType field)
      pure field {dcfiType = fieldType}

renameClassInfo :: ClassInfo -> Rename ClassInfo
renameClassInfo info = do
  kindTyVars <- traverse renameTyVarBinder (ciKindTyVars info)
  tyVars <- traverse renameTyVarBinder (ciTyVars info)
  superClasses <- traverse renameType (ciSuperClassTypes info)
  methods <- traverse (traverse renameScheme) (ciMethods info)
  defaultSignatures <- traverse (traverse renameScheme) (ciDefaultSignatures info)
  associated <- traverse renameAssociated (ciAssociatedTypes info)
  pure
    info
      { ciKindTyVars = kindTyVars,
        ciTyVars = tyVars,
        ciSuperClassTypes = superClasses,
        ciMethods = methods,
        ciDefaultSignatures = defaultSignatures,
        ciAssociatedTypes = associated
      }
  where
    renameAssociated associated = do
      equation <- traverse renameTypeFamilyInfo (atiDefault associated)
      pure associated {atiDefault = equation}

renameInstanceInfo :: InstanceInfo -> Rename InstanceInfo
renameInstanceInfo info = do
  tyVars <- traverse renameTyVarBinder (iiTyVars info)
  dictType <- renameType (iiDictType info)
  context <- traverse renamePred (iiContext info)
  head' <- traverse renameType (iiHead info)
  pure info {iiTyVars = tyVars, iiDictType = dictType, iiContext = context, iiHead = head'}

renameDataFamilyInfo :: DataFamilyInstanceInfo -> Rename DataFamilyInstanceInfo
renameDataFamilyInfo info = do
  tyVars <- traverse renameTyVarBinder (dfiiTyVars info)
  familyType <- renameType (dfiiFamilyType info)
  pure info {dfiiTyVars = tyVars, dfiiFamilyType = familyType}

renameTypeFamilyInfo :: TypeFamilyInstanceInfo -> Rename TypeFamilyInstanceInfo
renameTypeFamilyInfo info = do
  tyVars <- traverse renameTyVarBinder (tfiiTyVars info)
  left <- renameType (tfiiLeft info)
  right <- renameType (tfiiRight info)
  pure info {tfiiTyVars = tyVars, tfiiLeft = left, tfiiRight = right}

renamePatSynInfo :: PatSynInfo -> Rename PatSynInfo
renamePatSynInfo info = do
  scheme <- renameScheme (psiScheme info)
  required <- traverse renamePred (psiReqTheta info)
  provided <- traverse renamePred (psiProvTheta info)
  pure info {psiScheme = scheme, psiReqTheta = required, psiProvTheta = provided}
