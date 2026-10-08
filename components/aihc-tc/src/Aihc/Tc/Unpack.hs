-- | Decide the heap layout of constructor fields.
--
-- The defining module decides each layout once, after its constructors are
-- registered. An importer reads the stored layout.
module Aihc.Tc.Unpack
  ( decideConstructorRepresentations,
    unpackableConstructor,
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
    applySubstRep,
    repLeaves,
  )
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (expandTcTypeSynonyms)
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcEnv, TcM, TcResult, TcState (..), emitError, emitWarning, lookupDataType)
import Aihc.Tc.Types
  ( TcAxiomKey,
    TcType (..),
    TyCon,
    applySubst,
  )
import Aihc.Tc.Zonk (defaultTypeKinds, zonkType)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, modify')
import Control.Monad.Trans.State.Strict qualified as Memo
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
      local = typeConstructors True localTypes <> familyConstructors True localFamilies
      known = local <> typeConstructors False (tcsDataTypes state) <> familyConstructors False (tcsDataFamilyInstances state)
      localKeys = Map.keys local
  decided <- Memo.evalStateT (mapM (\key -> (,) key <$> decideCon known key) localKeys) Map.empty
  let reps = Map.fromList decided
      rewrite info =
        case Map.lookup (conKey info) reps of
          Just fieldReps -> info {dciFields = zipWith (\field rep -> field {dcfiRep = rep}) (dciFields info) fieldReps}
          Nothing -> info
  mapM_ (checkWidth . rewrite . knownInfo) (Map.elems local)
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

-- | The field layouts of one constructor. A constructor from an earlier
-- component keeps its stored layouts.
decideCon :: Map ConKey KnownCon -> ConKey -> MemoM [FieldRep]
decideCon known key = do
  memo <- Memo.get
  case (Map.lookup key memo, Map.lookup key known) of
    (Just reps, _) -> pure reps
    (Nothing, Nothing) -> pure []
    (Nothing, Just item)
      | not (knownLocal item) -> pure (map dcfiRep (dciFields (knownInfo item)))
      | otherwise -> do
          reps <- mapM (decideField known item) (dciFields (knownInfo item))
          Memo.modify' (Map.insert key reps)
          pure reps

storedField :: DataConFieldInfo -> FieldRep
storedField field = RepStored (dcfiType field) (dcfiStrict field)

-- | Whether the field has an UNPACK pragma and a bang. GHC ignores an
-- UNPACK pragma on a field without a bang.
unpackRequested :: DataConFieldInfo -> Bool
unpackRequested field = dcfiUnpack field == UnpackField && dcfiStrict field

decideField :: Map ConKey KnownCon -> KnownCon -> DataConFieldInfo -> MemoM FieldRep
decideField known item field
  | dcfiUnpack field /= UnpackField = pure (storedField field)
  | knownNewtype item = keep "a newtype has no heap field"
  | not (dcfiStrict field) = keep "the field is not strict"
  | otherwise = do
      acyclic <- lift (acyclicUnpack known Set.empty (dcfiType field))
      if not acyclic
        then keep "the field type is recursive"
        else do
          rep <- representationOf known (dcfiType field)
          maybe (keep "the field type is not one product") pure rep
  where
    keep reason = do
      let constructorName = T.unpack (dciName (knownInfo item))
      lift (emitWarning Nothing (OtherError ("The UNPACK pragma on " <> constructorName <> " has no effect because " <> reason <> ".")))
      pure (storedField field)

-- | Whether the unpack walk from one type meets no constructor twice.
--
-- The walk goes through each newtype and into each unpacked field of a
-- local product. A constructor from an earlier component cannot name a
-- local type, so the walk stops there. A walk that meets a constructor
-- again marks a recursive field, which stays one pointer as in GHC.
acyclicUnpack :: Map ConKey KnownCon -> Set ConKey -> TcType -> TcM Bool
acyclicUnpack known seen ty = do
  (_, classified) <- classifyType ty
  case classified of
    HeadOther -> pure True
    HeadNewtype info _ _ inner -> continue info [inner]
    HeadProduct info
      | maybe False knownLocal (Map.lookup (conKey info) known) ->
          continue info [dcfiType field | field <- dciFields info, unpackRequested field]
      | otherwise -> pure True
  where
    continue info types
      | conKey info `Set.member` seen = pure False
      | otherwise = and <$> mapM (acyclicUnpack known (Set.insert (conKey info) seen)) types

-- | The layout of one unpacked field type. 'Nothing' means the type is not
-- one product after newtype erasure. The walk must be acyclic.
representationOf :: Map ConKey KnownCon -> TcType -> MemoM (Maybe FieldRep)
representationOf known ty = do
  (prepared, classified) <- lift (classifyType ty)
  case classified of
    HeadOther -> pure Nothing
    HeadNewtype _ tyCon arguments inner ->
      fmap (RepCast tyCon arguments) <$> representationOf known inner
    HeadProduct info -> do
      reps <- decideCon known (conKey info)
      let substitution = fromMaybe Map.empty (matchTypes [dciResTy info] [prepared])
          leaves = concatMap (repLeaves . applySubstRep substitution) reps
      -- The outer case binds these arguments, so each leaf is stored.
      pure (Just (RepUnpack (conKey info) [RepStored leafType strict | (leafType, strict) <- leaves]))

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
          HeadNewtype info tyCon arguments (instantiate info (TcTyCon tyCon arguments) (dcfiType field))
    Just dataType
      | Just info <- unpackableConstructor dataType ->
          HeadProduct info
    _ -> HeadOther

-- | The constructor that a strict field with an UNPACK pragma can unpack.
-- Only a data type with one constructor has one. The field can be in
-- any module, also when the export list hides this constructor.
unpackableConstructor :: DataTypeInfo -> Maybe DataConInfo
unpackableConstructor dataType
  | dtiFlavor dataType == DataTyCon,
    [info] <- dtiConstructors dataType,
    unpackableProduct info =
      Just info
  | otherwise = Nothing

unpackableProduct :: DataConInfo -> Bool
unpackableProduct info =
  null (dciExTyVars info) && null (dciTheta info) && heapConstructor info

instantiate :: DataConInfo -> TcType -> TcType -> TcType
instantiate info useTy fieldTy =
  case matchTypes [dciResTy info] [useTy] of
    Just substitution -> applySubst substitution fieldTy
    Nothing -> fieldTy

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
