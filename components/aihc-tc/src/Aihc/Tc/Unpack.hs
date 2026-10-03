-- | Decide the heap layout of constructor fields.
--
-- The defining module decides each layout once, after its constructors are
-- registered. An importer reads the stored layout.
module Aihc.Tc.Unpack
  ( decideConstructorRepresentations,
  )
where

import Aihc.Resolve (GlobalName, PackageId)
import Aihc.Tc.Env
  ( DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConInfo (..),
    DataConSourceForm (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    FieldRep (..),
    TyConFlavor (..),
    TyConInfo (..),
    applySubstRep,
    repHasUnpack,
    repLeaves,
  )
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (expandTcTypeSynonyms)
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcEnv, TcM, TcResult, TcState (..), emitError, emitWarning, getKinds, lookupDataType)
import Aihc.Tc.Types
  ( TcAxiomKey,
    TcType (..),
    TyCon,
    applySubst,
    isUnliftedTypeInEnv,
  )
import Aihc.Tc.Zonk (defaultTypeKinds, zonkType)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, modify')
import Control.Monad.Trans.State.Strict qualified as Memo
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Package, module, and constructor name.
type ConKey = (PackageId, Text, Text)

-- | The memo for one unpack decision. 'TcM' is a synonym, so this stack
-- names the same transformers directly.
type MemoM a = Memo.StateT (Map ConKey [FieldRep]) (ReaderT TcEnv (StateT TcState TcResult)) a

type FieldId = (ConKey, Int)

data KnownCon = KnownCon
  { knownNewtype :: !Bool,
    knownLocal :: !Bool,
    knownInfo :: !DataConInfo
  }

data TypeHead
  = HeadOther
  | HeadNewtype !DataConInfo !TyCon ![TcType] !TcType
  | HeadProduct !DataConInfo

-- | Fill 'dcfiRep' for every constructor this component declares.
--
-- The two maps are the data types and the data-family instances that were
-- already present before the component. This pass does not change them.
decideConstructorRepresentations :: Map GlobalName DataTypeInfo -> Map TcAxiomKey DataFamilyInstanceInfo -> TcM ()
decideConstructorRepresentations previousTypes previousFamilies = do
  state <- lift get
  let localTypes = Map.difference (tcsDataTypes state) previousTypes
      localFamilies = Map.difference (tcsDataFamilyInstances state) previousFamilies
      known =
        Map.union
          (Map.union (typeConstructors True localTypes) (familyConstructors True localFamilies))
          (Map.union (typeConstructors False (tcsDataTypes state)) (familyConstructors False (tcsDataFamilyInstances state)))
      localKeys = [key | (key, item) <- Map.toList known, knownLocal item]
  edges <- mapM (fieldTargets known) localKeys
  let cyclic = cyclicFields edges
  decided <- Memo.evalStateT (mapM (\key -> (,) key <$> decideCon known cyclic Set.empty key) localKeys) Map.empty
  let reps = Map.fromList decided
      rewrite info =
        case Map.lookup (conKey info) reps of
          Just fieldReps -> info {dciFields = zipWith (\field rep -> field {dcfiRep = rep}) (dciFields info) fieldReps}
          Nothing -> info
  mapM_ (checkWidth . rewrite) [knownInfo item | key <- localKeys, Just item <- [Map.lookup key known]]
  lift $
    modify' $ \current ->
      current
        { tcsDataTypes = Map.map (\info -> info {dtiConstructors = map rewrite (dtiConstructors info)}) (tcsDataTypes current),
          tcsDataFamilyInstances = Map.map (\info -> info {dfiiConstructors = map rewrite (dfiiConstructors info)}) (tcsDataFamilyInstances current)
        }

typeConstructors :: Bool -> Map GlobalName DataTypeInfo -> Map ConKey KnownCon
typeConstructors local types =
  Map.fromList
    [ (conKey info, KnownCon (dtiFlavor dataType == NewtypeTyCon) local info)
    | dataType <- Map.elems types,
      info <- dtiConstructors dataType
    ]

familyConstructors :: Bool -> Map TcAxiomKey DataFamilyInstanceInfo -> Map ConKey KnownCon
familyConstructors local families =
  Map.fromList
    [ (conKey info, KnownCon (dfiiIsNewtype family) local info)
    | family <- Map.elems families,
      info <- dfiiConstructors family
    ]

conKey :: DataConInfo -> ConKey
conKey info =
  let (package, moduleName) = dciOrigin info
   in (package, moduleName, dciName info)

-- | The product constructor each unpacked field names, after newtype erasure.
fieldTargets :: Map ConKey KnownCon -> ConKey -> TcM (ConKey, [(Int, Maybe ConKey)])
fieldTargets known key =
  case Map.lookup key known of
    Nothing -> pure (key, [])
    Just item
      | knownNewtype item -> pure (key, [])
      | otherwise -> do
          targets <- mapM fieldTarget (zip [0 ..] (dciFields (knownInfo item)))
          pure (key, targets)

fieldTarget :: (Int, DataConFieldInfo) -> TcM (Int, Maybe ConKey)
fieldTarget (index, field)
  | dcfiUnpack field == UnpackField && not (dcfiLazy field) = do
      target <- productTarget Set.empty (dcfiType field)
      pure (index, target)
  | otherwise = pure (index, Nothing)

-- | The product at the end of one unpack walk.
--
-- 'Nothing' means the walk found no algebraic product. 'Just' the same
-- constructor means the walk met that constructor again.
productTarget :: Set ConKey -> TcType -> TcM (Maybe ConKey)
productTarget seen ty = do
  (_, classified) <- classifyType ty
  case classified of
    HeadOther -> pure Nothing
    HeadProduct info -> pure (Just (conKey info))
    HeadNewtype info _ _ inner
      | conKey info `Set.member` seen -> pure (Just (conKey info))
      | otherwise -> productTarget (Set.insert (conKey info) seen) inner

-- | Fields whose unpack walk meets a constructor that is already on the walk.
cyclicFields :: [(ConKey, [(Int, Maybe ConKey)])] -> Set FieldId
cyclicFields edges =
  Set.fromList
    [ (owner, index)
    | (owner, targets) <- edges,
      (index, Just target) <- targets,
      owner == target || sameCycle owner target
    ]
  where
    graph = [(key, key, [target | (_, Just target) <- targets, Map.member target nodes]) | (key, targets) <- edges]
    nodes = Map.fromList [(key, ()) | (key, _) <- edges]
    components = zip [0 :: Int ..] (stronglyConnComp graph)
    mark = Map.fromList [(key, (number, cyclic)) | (number, component) <- components, (key, cyclic) <- componentKeys component]
    componentKeys (AcyclicSCC key) = [(key, False)]
    componentKeys (CyclicSCC keys) = [(key, True) | key <- keys]
    sameCycle owner target =
      case (Map.lookup owner mark, Map.lookup target mark) of
        (Just (left, True), Just (right, _)) -> left == right
        _ -> False

decideCon :: Map ConKey KnownCon -> Set FieldId -> Set ConKey -> ConKey -> MemoM [FieldRep]
decideCon known cyclic seen key = do
  memo <- Memo.get
  case Map.lookup key memo of
    Just reps -> pure reps
    Nothing ->
      case Map.lookup key known of
        Nothing -> pure []
        Just item
          | not (knownLocal item) -> pure (map dcfiRep (dciFields (knownInfo item)))
          | key `Set.member` seen -> pure (map storedField (dciFields (knownInfo item)))
          | otherwise -> do
              let next = Set.insert key seen
              reps <- mapM (decideField known cyclic next key item) (zip [0 ..] (dciFields (knownInfo item)))
              Memo.modify' (Map.insert key reps)
              pure reps

storedField :: DataConFieldInfo -> FieldRep
storedField field = RepStored (dcfiType field) (dcfiStrict field)

decideField :: Map ConKey KnownCon -> Set FieldId -> Set ConKey -> ConKey -> KnownCon -> (Int, DataConFieldInfo) -> MemoM FieldRep
decideField known cyclic seen key item (index, field) = do
  prepared <- lift (prepareType (dcfiType field))
  unlifted <- lift (typeIsUnlifted prepared)
  let wantUnpack = dcfiUnpack field == UnpackField && not (dcfiLazy field)
      constructorName = T.unpack (dciName (knownInfo item))
      stored = RepStored (dcfiType field)
      -- A concrete unlifted field of a heap constructor is strict.
      -- An unboxed tuple or an unboxed sum is not a heap object.
      strictUnlifted = unlifted && heapConstructor (knownInfo item)
  if knownNewtype item
    then do
      when (dcfiUnpack field == UnpackField) $
        lift (emitWarning Nothing (OtherError ("The UNPACK pragma on " <> constructorName <> " has no effect because a newtype has no heap field.")))
      pure (stored (dcfiStrict field || strictUnlifted || (dcfiUnpack field == UnpackField && not (dcfiLazy field))))
    else
      if dcfiLazy field && dcfiUnpack field == UnpackField
        then do
          lift (emitWarning Nothing (OtherError ("The UNPACK pragma on " <> constructorName <> " has no effect because the field is lazy.")))
          pure (stored False)
        else
          if not wantUnpack
            then pure (stored (dcfiStrict field || strictUnlifted))
            else
              if (key, index) `Set.member` cyclic
                then do
                  lift (emitWarning Nothing (OtherError ("The UNPACK pragma on " <> constructorName <> " has no effect because the field type is recursive.")))
                  pure (stored True)
                else do
                  rep <- representationOf known cyclic seen prepared
                  case rep of
                    Just layout
                      | repHasUnpack layout -> pure layout
                    Just layout -> do
                      lift (emitWarning Nothing (OtherError ("The UNPACK pragma on " <> constructorName <> " has no effect because the field type is not one product.")))
                      pure layout
                    Nothing -> do
                      lift (emitWarning Nothing (OtherError ("The UNPACK pragma on " <> constructorName <> " has no effect because the field type is not one product.")))
                      pure (stored True)

representationOf :: Map ConKey KnownCon -> Set FieldId -> Set ConKey -> TcType -> MemoM (Maybe FieldRep)
representationOf known cyclic seen ty = do
  (prepared, classified) <- lift (classifyType ty)
  case classified of
    HeadOther -> pure Nothing
    HeadNewtype info tyCon arguments inner
      | conKey info `Set.member` seen -> pure Nothing
      | otherwise -> do
          innerRep <- representationOf known cyclic (Set.insert (conKey info) seen) inner
          body <- case innerRep of
            Just layout -> pure layout
            Nothing -> pure (RepStored inner True)
          pure (Just (RepCast tyCon arguments body))
    HeadProduct info
      | conKey info `Set.member` seen -> pure Nothing
      | otherwise -> do
          reps <- decideCon known cyclic seen (conKey info)
          let substitution = fromMaybe Map.empty (matchTypes [dciResTy info] [prepared])
              leaves = concatMap (repLeaves . applySubstRep substitution) reps
          pure (Just (RepUnpack (conKey info) (map flattenLeaf leaves)))

-- | A leaf stored for an outer product. The outer case binds these arguments.
flattenLeaf :: (TcType, Bool) -> FieldRep
flattenLeaf (ty, strict) = RepStored ty strict

classifyType :: TcType -> TcM (TcType, TypeHead)
classifyType ty = do
  prepared <- prepareType ty
  case splitTyCon prepared of
    Nothing -> pure (prepared, HeadOther)
    Just (tyCon, arguments) -> do
      maybeType <- lookupDataType tyCon
      pure (prepared, typeHead tyCon arguments maybeType)

typeHead :: TyCon -> [TcType] -> Maybe DataTypeInfo -> TypeHead
typeHead tyCon arguments maybeType =
  case maybeType of
    Just dataType
      | dtiFlavor dataType == NewtypeTyCon,
        [info] <- dtiConstructors dataType,
        [field] <- dciFields info,
        null (dciExTyVars info),
        null (dciTheta info) ->
          HeadNewtype info tyCon arguments (instantiate info (tyConType tyCon arguments) (dcfiType field))
    Just dataType
      | dtiFlavor dataType == DataTyCon,
        [info] <- dtiConstructors dataType,
        unpackableProduct info ->
          HeadProduct info
    _ -> HeadOther

unpackableProduct :: DataConInfo -> Bool
unpackableProduct info =
  null (dciExTyVars info)
    && null (dciTheta info)
    && case dciSourceForm info of
      UnboxedTupleDataCon -> False
      UnboxedSumDataCon {} -> False
      _ -> True

instantiate :: DataConInfo -> TcType -> TcType -> TcType
instantiate info useTy fieldTy =
  case matchTypes [dciResTy info] [useTy] of
    Just substitution -> applySubst substitution fieldTy
    Nothing -> fieldTy

tyConType :: TyCon -> [TcType] -> TcType
tyConType = TcTyCon

splitTyCon :: TcType -> Maybe (TyCon, [TcType])
splitTyCon ty =
  case ty of
    TcTyCon tyCon arguments -> Just (tyCon, arguments)
    TcKindedTyCon tyCon _ -> Just (tyCon, [])
    TcAppTy function argument ->
      case splitTyCon function of
        Just (tyCon, arguments) -> Just (tyCon, arguments <> [argument])
        Nothing -> Nothing
    _ -> Nothing

prepareType :: TcType -> TcM TcType
prepareType ty = do
  zonked <- zonkType ty
  expanded <- expandTcTypeSynonyms Set.empty zonked
  defaultTypeKinds expanded

typeIsUnlifted :: TcType -> TcM Bool
typeIsUnlifted ty = do
  kinds <- getKinds
  state <- lift get
  let kindEnv = Map.map tciKindScheme (tcsGlobalTyCons state)
  pure (isUnliftedTypeInEnv kinds kindEnv ty)

-- | Reject a heap object that needs more than 255 machine fields.
checkWidth :: DataConInfo -> TcM ()
checkWidth info
  | not (heapConstructor info) = pure ()
  | otherwise = do
      counts <- mapM (machineWords . fst) (concatMap (repLeaves . dcfiRep) (dciFields info))
      let total = length (dciTheta info) + sum counts
      when (total > 255) $
        emitError Nothing (OtherError ("The constructor " <> T.unpack (dciName info) <> " has more than 255 fields."))

heapConstructor :: DataConInfo -> Bool
heapConstructor info =
  case dciSourceForm info of
    UnboxedTupleDataCon -> False
    UnboxedSumDataCon {} -> False
    _ -> True

-- | The machine fields of one representation argument.
--
-- An unboxed tuple is more than one word when its arity is known.
-- The lowering pass still rejects an object that exceeds the limit.
machineWords :: TcType -> TcM Int
machineWords ty =
  case splitTyCon ty of
    Just (tyCon, _) -> do
      maybeType <- lookupDataType tyCon
      pure $
        case maybeType of
          Just dataType
            | [constructor] <- dtiConstructors dataType,
              UnboxedTupleDataCon <- dciSourceForm constructor ->
                length (dciFields constructor)
          _ -> 1
    Nothing -> pure 1
