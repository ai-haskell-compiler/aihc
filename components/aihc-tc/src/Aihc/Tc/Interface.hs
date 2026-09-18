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
  )
where

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
  { tcInterfaceTermMap :: !(Map.Map TcTermKey TypeScheme),
    tcInterfaceTyConMap :: !(Map.Map TcTypeKey TyConInfo),
    tcInterfaceDataTypeMap :: !(Map.Map TcTypeKey DataTypeInfo),
    tcInterfaceClassMap :: !(Map.Map TcTypeKey ClassInfo),
    tcInterfaceInstanceMap :: !(Map.Map InstanceKey InstanceInfo),
    tcInterfaceDataFamilyInstanceMap :: !(Map.Map TcAxiomKey DataFamilyInstanceInfo),
    tcInterfaceTypeFamilyInstanceMap :: !(Map.Map TcAxiomKey TypeFamilyInstanceInfo),
    tcInterfacePatSynMap :: !(Map.Map TcTermKey PatSynInfo),
    -- | The checked calling convention of each foreign import.
    tcInterfaceForeignImportMap :: !(Map.Map TcTermKey TcForeignImportInfo)
  }
  deriving (Eq, Show, Read, Generic)

instance NFData TcInterface

-- | The identity of an instance: its dictionary origin and name.
type InstanceKey = ((Text, Text), Text)

tcInterfaceTerms :: TcInterface -> [(TcTermKey, TypeScheme)]
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

tcInterfaceForeignImports :: TcInterface -> [(TcTermKey, TcForeignImportInfo)]
tcInterfaceForeignImports = Map.toList . tcInterfaceForeignImportMap

-- | Build an interface from lists of facts. Two facts with one identity
-- must be equal.
tcInterfaceFromLists :: [(TcTermKey, TypeScheme)] -> [TyConInfo] -> [DataTypeInfo] -> [ClassInfo] -> [InstanceInfo] -> [DataFamilyInstanceInfo] -> [TypeFamilyInstanceInfo] -> [PatSynInfo] -> [(TcTermKey, TcForeignImportInfo)] -> TcInterface
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
      tcInterfaceForeignImportMap = fromListChecked "foreign import interface" id foreignImports
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
      tcInterfaceForeignImportMap = Map.empty
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
      tcInterfaceForeignImportMap = merge "foreign import interface" tcInterfaceForeignImportMap
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
