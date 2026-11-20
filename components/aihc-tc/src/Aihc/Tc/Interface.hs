-- | Semantic facts for module imports and exports.
module Aihc.Tc.Interface
  ( TcInterface (..),
    InstanceKey,
    tcInterfaceTerms,
    tcInterfaceTyCons,
    tcInterfaceDataTypes,
    tcInterfaceClasses,
    tcInterfaceInstances,
    tcInterfaceDataFamilyInstances,
    tcInterfaceTypeFamilyInstances,
    tcInterfacePatSyns,
    tcInterfaceForeignImports,
    tcInterfaceFromLists,
    emptyTcInterface,
    MergeCheck (..),
    mergeTcInterface,
    mergeTcInterfaces,

    -- * Facts by the module of their origin
    ModuleOrigin,
    FactEnv,
    emptyFactEnv,
    factEnvFromInterface,
    factEnvFromModules,
    factEnvUniqueBound,
    factEnvExtra,
    factEnvInterface,
    entityOrigin,
    globalNameOrigin,
    factEnvTerm,
    factEnvTyCon,
    factEnvDataType,
    factEnvClass,
    factEnvPatSyn,
    factEnvForeignImport,
    factEnvTyCons,
    factEnvDataTypes,
    factEnvClasses,
    factEnvPatSyns,
    factEnvModuleDataTypes,
  )
where

import Aihc.Resolve (PackageId)
import Aihc.Tc.Annotations (TcForeignImportInfo)
import Aihc.Tc.Env
import Aihc.Tc.Types
import Control.DeepSeq (NFData)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Semantic facts for type checks across module groups.
-- This interface contains no term implementations.
--
-- Each map uses global identities as keys.
-- Map operations can share unchanged subtrees with the checker state.
-- The list views below use key order.
data TcInterface = TcInterface
  { tcInterfaceTermMap :: !(Map.Map Entity TypeScheme),
    tcInterfaceTyConMap :: !(Map.Map GlobalName TyConInfo),
    tcInterfaceDataTypeMap :: !(Map.Map GlobalName DataTypeInfo),
    tcInterfaceClassMap :: !(Map.Map GlobalName ClassInfo),
    tcInterfaceInstanceMap :: !(Map.Map InstanceKey InstanceInfo),
    tcInterfaceDataFamilyInstanceMap :: !(Map.Map TcAxiomKey DataFamilyInstanceInfo),
    tcInterfaceTypeFamilyInstanceMap :: !(Map.Map TcAxiomKey TypeFamilyInstanceInfo),
    tcInterfacePatSynMap :: !(Map.Map Entity PatSynInfo),
    -- | The checked calling convention of each foreign import.
    tcInterfaceForeignImportMap :: !(Map.Map Entity TcForeignImportInfo),
    -- | A number above each unique in the facts. Each type checker run
    -- starts its unique supply at 0, so two runs can give one unique to two
    -- variables. A run that imports these facts starts its supply here, so
    -- a fresh variable never has the unique of an imported variable.
    tcInterfaceUniqueBound :: !Int
  }
  deriving (Eq, Show, Read, Generic)

instance NFData TcInterface

-- | The identity of an instance: its dictionary origin and name.
type InstanceKey = ((Text, Text), Text)

tcInterfaceTerms :: TcInterface -> [(Entity, TypeScheme)]
tcInterfaceTerms = Map.toList . tcInterfaceTermMap

tcInterfaceTyCons :: TcInterface -> [TyConInfo]
tcInterfaceTyCons = Map.elems . tcInterfaceTyConMap

tcInterfaceDataTypes :: TcInterface -> [DataTypeInfo]
tcInterfaceDataTypes = Map.elems . tcInterfaceDataTypeMap

tcInterfaceClasses :: TcInterface -> [ClassInfo]
tcInterfaceClasses = Map.elems . tcInterfaceClassMap

tcInterfaceInstances :: TcInterface -> [InstanceInfo]
tcInterfaceInstances = Map.elems . tcInterfaceInstanceMap

tcInterfaceDataFamilyInstances :: TcInterface -> [DataFamilyInstanceInfo]
tcInterfaceDataFamilyInstances = Map.elems . tcInterfaceDataFamilyInstanceMap

tcInterfaceTypeFamilyInstances :: TcInterface -> [TypeFamilyInstanceInfo]
tcInterfaceTypeFamilyInstances = Map.elems . tcInterfaceTypeFamilyInstanceMap

tcInterfacePatSyns :: TcInterface -> [PatSynInfo]
tcInterfacePatSyns = Map.elems . tcInterfacePatSynMap

tcInterfaceForeignImports :: TcInterface -> [(Entity, TcForeignImportInfo)]
tcInterfaceForeignImports = Map.toList . tcInterfaceForeignImportMap

-- | Build an interface from lists of facts. Two facts with one identity
-- must be equal. The unique bound is 0. Set it if the facts contain uniques.
tcInterfaceFromLists :: [(Entity, TypeScheme)] -> [TyConInfo] -> [DataTypeInfo] -> [ClassInfo] -> [InstanceInfo] -> [DataFamilyInstanceInfo] -> [TypeFamilyInstanceInfo] -> [PatSynInfo] -> [(Entity, TcForeignImportInfo)] -> TcInterface
tcInterfaceFromLists terms tyCons dataTypes classes instances dataFamilyInstances typeFamilyInstances patSyns foreignImports =
  TcInterface
    { tcInterfaceTermMap = fromListChecked "term interface" id terms,
      tcInterfaceTyConMap = fromListChecked "type constructor interface" (keyed (tyConKey . tciTyCon)) tyCons,
      tcInterfaceDataTypeMap = fromListChecked "data type interface" (keyed dataTypeKey) dataTypes,
      tcInterfaceClassMap = fromListChecked "class interface" (keyed classInfoKey) classes,
      tcInterfaceInstanceMap = fromListChecked "instance interface" (keyed instanceInfoKey) instances,
      tcInterfaceDataFamilyInstanceMap = fromListChecked "data family instance interface" (keyed dataFamilyAxiomKey) dataFamilyInstances,
      tcInterfaceTypeFamilyInstanceMap = fromListChecked "type family instance interface" (keyed typeFamilyAxiomKey) typeFamilyInstances,
      tcInterfacePatSynMap = fromListChecked "pattern synonym interface" (keyed patSynKey) patSyns,
      tcInterfaceForeignImportMap = fromListChecked "foreign import interface" id foreignImports,
      tcInterfaceUniqueBound = 0
    }
  where
    keyed key value = (key value, value)
    fromListChecked label key = Map.fromListWithKey (conflict label) . map key

emptyTcInterface :: TcInterface
emptyTcInterface =
  TcInterface
    { tcInterfaceTermMap = Map.empty,
      tcInterfaceTyConMap = Map.empty,
      tcInterfaceDataTypeMap = Map.empty,
      tcInterfaceClassMap = Map.empty,
      tcInterfaceInstanceMap = Map.empty,
      tcInterfaceDataFamilyInstanceMap = Map.empty,
      tcInterfaceTypeFamilyInstanceMap = Map.empty,
      tcInterfacePatSynMap = Map.empty,
      tcInterfaceForeignImportMap = Map.empty,
      tcInterfaceUniqueBound = 0
    }

-- | The policy for facts with the same identity.
--
-- 'CheckMergedFacts' compares facts with the same identity on both sides.
-- These comparisons can include all imported facts for each module group.
-- 'TrustMergedFacts' omits these comparisons and keeps the left value.
-- The caller must ensure that both sides agree.
data MergeCheck
  = CheckMergedFacts
  | TrustMergedFacts
  deriving (Eq, Show)

-- | Merge interfaces with the specified policy.
-- Facts with one identity must be equal.
-- The first interface supplies the value for each shared identity.
mergeTcInterfaces :: MergeCheck -> [TcInterface] -> TcInterface
mergeTcInterfaces _ [] = emptyTcInterface
mergeTcInterfaces check (first : rest) = List.foldl' (mergeTcInterface check) first rest

mergeTcInterface :: MergeCheck -> TcInterface -> TcInterface -> TcInterface
mergeTcInterface check left right =
  TcInterface
    { tcInterfaceTermMap = merge "term interface" tcInterfaceTermMap,
      tcInterfaceTyConMap = merge "type constructor interface" tcInterfaceTyConMap,
      tcInterfaceDataTypeMap = merge "data type interface" tcInterfaceDataTypeMap,
      tcInterfaceClassMap = merge "class interface" tcInterfaceClassMap,
      tcInterfaceInstanceMap = merge "instance interface" tcInterfaceInstanceMap,
      tcInterfaceDataFamilyInstanceMap = merge "data family instance interface" tcInterfaceDataFamilyInstanceMap,
      tcInterfaceTypeFamilyInstanceMap = merge "type family instance interface" tcInterfaceTypeFamilyInstanceMap,
      tcInterfacePatSynMap = merge "pattern synonym interface" tcInterfacePatSynMap,
      tcInterfaceForeignImportMap = merge "foreign import interface" tcInterfaceForeignImportMap,
      tcInterfaceUniqueBound = max (tcInterfaceUniqueBound left) (tcInterfaceUniqueBound right)
    }
  where
    merge :: (Ord key, Show key, Eq value) => String -> (TcInterface -> Map.Map key value) -> Map.Map key value
    merge label select = case check of
      CheckMergedFacts -> Map.unionWithKey (conflict label) (select left) (select right)
      TrustMergedFacts -> Map.union (select left) (select right)

conflict :: (Show key, Eq value) => String -> key -> value -> value -> value
conflict label key left right
  | left == right = left
  | otherwise = error ("conflicting " <> label <> " key: " <> show key)

-- | The module that declares a fact: its name and its package. The name
-- comes first, because two module names differ sooner than two package
-- identities.
type ModuleOrigin = (Text, PackageId)

-- | The facts that a module group can see, without a merge.
--
-- Each module has one interface with the facts that the module declares.
-- All module groups that can see the module use the same object. The key
-- of a fact gives the module of its origin, so a lookup goes to the
-- interface of that module.
--
-- The extra interface holds the facts that the origin does not find: the
-- instances, the family instances, and the facts that a module group
-- registered for a type constructor of a different module.
data FactEnv = FactEnv
  { factEnvModules :: !(Map.Map ModuleOrigin TcInterface),
    factEnvExtra :: !TcInterface,
    -- | A number above each unique in the facts.
    factEnvUniqueBound :: !Int
  }
  deriving (Show)

emptyFactEnv :: FactEnv
emptyFactEnv = FactEnv Map.empty emptyTcInterface 0

-- | The facts of one merged interface. Each lookup reads that interface.
factEnvFromInterface :: TcInterface -> FactEnv
factEnvFromInterface interface = FactEnv Map.empty interface (tcInterfaceUniqueBound interface)

-- | The facts of the given modules, and the extra facts.
factEnvFromModules :: Map.Map ModuleOrigin TcInterface -> TcInterface -> FactEnv
factEnvFromModules modules extra =
  FactEnv modules extra (Map.foldl' (\bound interface -> max bound (tcInterfaceUniqueBound interface)) (tcInterfaceUniqueBound extra) modules)

-- | All facts as one interface. This is a merge of each module, so use it
-- only where a consumer needs one interface.
factEnvInterface :: FactEnv -> TcInterface
factEnvInterface env =
  (mergeTcInterfaces TrustMergedFacts (factEnvExtra env : Map.elems (factEnvModules env)))
    { tcInterfaceUniqueBound = factEnvUniqueBound env
    }

entityOrigin :: Entity -> Maybe ModuleOrigin
entityOrigin entity =
  case entity of
    EntityGlobal global -> Just (globalNameOrigin global)
    _ -> Nothing

globalNameOrigin :: GlobalName -> ModuleOrigin
globalNameOrigin global = (globalNameModule global, globalNamePackage global)

lookupFact :: (Ord key) => (TcInterface -> Map.Map key value) -> Maybe ModuleOrigin -> key -> FactEnv -> Maybe value
lookupFact select origin key env =
  case origin >>= (`Map.lookup` factEnvModules env) of
    Just interface
      | Just value <- Map.lookup key (select interface) -> Just value
    _ -> Map.lookup key (select (factEnvExtra env))
{-# INLINE lookupFact #-}

factEnvTerm :: Entity -> FactEnv -> Maybe TypeScheme
factEnvTerm key = lookupFact tcInterfaceTermMap (entityOrigin key) key

factEnvTyCon :: GlobalName -> FactEnv -> Maybe TyConInfo
factEnvTyCon key = lookupFact tcInterfaceTyConMap (Just (globalNameOrigin key)) key

factEnvDataType :: GlobalName -> FactEnv -> Maybe DataTypeInfo
factEnvDataType key = lookupFact tcInterfaceDataTypeMap (Just (globalNameOrigin key)) key

factEnvClass :: GlobalName -> FactEnv -> Maybe ClassInfo
factEnvClass key = lookupFact tcInterfaceClassMap (Just (globalNameOrigin key)) key

factEnvPatSyn :: Entity -> FactEnv -> Maybe PatSynInfo
factEnvPatSyn key = lookupFact tcInterfacePatSynMap (entityOrigin key) key

factEnvForeignImport :: Entity -> FactEnv -> Maybe TcForeignImportInfo
factEnvForeignImport key = lookupFact tcInterfaceForeignImportMap (entityOrigin key) key

-- | Each fact of one table: the extra facts first, then each module.
-- A fact can be in the extra facts and in a module.
allFacts :: (TcInterface -> Map.Map key value) -> FactEnv -> [value]
allFacts select env =
  Map.elems (select (factEnvExtra env)) <> concatMap (Map.elems . select) (Map.elems (factEnvModules env))

factEnvTyCons :: FactEnv -> [TyConInfo]
factEnvTyCons = allFacts tcInterfaceTyConMap

factEnvDataTypes :: FactEnv -> [DataTypeInfo]
factEnvDataTypes = allFacts tcInterfaceDataTypeMap

factEnvClasses :: FactEnv -> [ClassInfo]
factEnvClasses = allFacts tcInterfaceClassMap

factEnvPatSyns :: FactEnv -> [PatSynInfo]
factEnvPatSyns = allFacts tcInterfacePatSynMap

-- | The data types that can have a constructor of the given module: those
-- of the module, and those of the extra facts.
factEnvModuleDataTypes :: ModuleOrigin -> FactEnv -> [DataTypeInfo]
factEnvModuleDataTypes origin env =
  maybe [] (Map.elems . tcInterfaceDataTypeMap) (Map.lookup origin (factEnvModules env))
    <> Map.elems (tcInterfaceDataTypeMap (factEnvExtra env))
