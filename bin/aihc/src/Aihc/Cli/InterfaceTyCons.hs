-- | Collect all type constructors that an interface stores or references.
--
-- Every collector adds to a set the caller passes in rather than building
-- one of its own for the caller to union. An interface holds tens of
-- thousands of types, and a set per type node -- merged again at each node
-- above it -- was most of what this cost.
module Aihc.Cli.InterfaceTyCons
  ( interfaceTyCons,
    interfaceTermTyCons,
    interfaceNonTermRootTyCons,
    tyConInfoTyCons,
    dataTypeInfoTyCons,
    classInfoTyCons,
    typeSchemeTyCons,
    typeTyCons,
  )
where

import Aihc.Tc
import Aihc.Tc.Annotations (TcForeignImportAnnotation (..), TcForeignImportInfo (..), TcForeignMarshal (..), TcForeignTarget (..))
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

-- | A collector: the type constructors of one part, added to a set.
type Collect a = a -> Set.Set TyCon -> Set.Set TyCon

collected :: Collect a -> a -> Set.Set TyCon
collected collect value = collect value Set.empty

each :: Collect a -> Collect [a]
each collect values acc = foldr collect acc values

interfaceTyCons :: TcInterface -> Set.Set TyCon
interfaceTyCons interface =
  interfaceTyConsWith tyConInfoTyConsInto dataTypeInfoTyConsInto classInfoTyConsInto interface (interfaceTermTyCons interface)

-- | Collect the type constructors of all terms into one set.
interfaceTermTyCons :: TcInterface -> Set.Set TyCon
interfaceTermTyCons interface = each typeSchemeTyConsInto (map snd (tcInterfaceTerms interface)) Set.empty

-- | The roots outside term schemes for a closure over a complete interface.
-- The closure reads type, data, and class declarations from its tables.
-- Start with their identities here to avoid a second walk of their types.
interfaceNonTermRootTyCons :: TcInterface -> Set.Set TyCon
interfaceNonTermRootTyCons = collected (interfaceTyConsWith (Set.insert . tciTyCon) (Set.insert . dtiTyCon) (Set.insert . ciTyCon))

interfaceTyConsWith :: Collect TyConInfo -> Collect DataTypeInfo -> Collect ClassInfo -> Collect TcInterface
interfaceTyConsWith onTyCon onDataType onClass interface =
  each onTyCon (tcInterfaceTyCons interface)
    . each onDataType (tcInterfaceDataTypes interface)
    . each onClass (tcInterfaceClasses interface)
    . each instanceInfoTyCons (tcInterfaceInstances interface)
    . each dataFamilyInstanceInfoTyCons (tcInterfaceDataFamilyInstances interface)
    . each typeFamilyInstanceInfoTyCons (tcInterfaceTypeFamilyInstances interface)
    . each patSynInfoTyCons (tcInterfacePatSyns interface)
    . each foreignImportInfoTyCons (map snd (tcInterfaceForeignImports interface))

tyConInfoTyCons :: TyConInfo -> Set.Set TyCon
tyConInfoTyCons = collected tyConInfoTyConsInto

tyConInfoTyConsInto :: Collect TyConInfo
tyConInfoTyConsInto info =
  Set.insert (tciTyCon info)
    . typeSchemeTyConsInto (tciKindScheme info)
    . maybe id typeSynonymInfoTyCons (tciTypeSynonym info)

typeSynonymInfoTyCons :: Collect TypeSynonymInfo
typeSynonymInfoTyCons info =
  each tyVarTyCons (tsiParams info)
    . maybe id typeTyConsInto (tsiBody info)

dataTypeInfoTyCons :: DataTypeInfo -> Set.Set TyCon
dataTypeInfoTyCons = collected dataTypeInfoTyConsInto

dataTypeInfoTyConsInto :: Collect DataTypeInfo
dataTypeInfoTyConsInto info =
  Set.insert (dtiTyCon info)
    . each tyVarTyCons (dtiTyVars info)
    . typeTyConsInto (dtiResultKind info)
    . each dataConInfoTyCons (dtiConstructors info)

dataConInfoTyCons :: Collect DataConInfo
dataConInfoTyCons info =
  each tyVarTyCons (dciUnivTyVars info <> dciExTyVars info)
    . each predTyCons (dciTheta info)
    . each (typeTyConsInto . dcfiType) (dciFields info)
    . typeTyConsInto (dciResTy info)

classInfoTyCons :: ClassInfo -> Set.Set TyCon
classInfoTyCons = collected classInfoTyConsInto

classInfoTyConsInto :: Collect ClassInfo
classInfoTyConsInto info =
  Set.insert (ciTyCon info)
    . each tyVarTyCons (ciKindTyVars info <> ciTyVars info)
    . each typeTyConsInto (ciSuperClassTypes info)
    . each (typeSchemeTyConsInto . snd) (ciMethods info <> ciDefaultSignatures info)
    . each (Set.insert . atiTyCon) (ciAssociatedTypes info)
    . each typeFamilyInstanceInfoTyCons (mapMaybe atiDefault (ciAssociatedTypes info))

instanceInfoTyCons :: Collect InstanceInfo
instanceInfoTyCons info =
  typeTyConsInto (iiDictType info)
    . each tyVarTyCons (iiTyVars info)
    . each predTyCons (iiContext info)
    . each typeTyConsInto (iiHead info)

dataFamilyInstanceInfoTyCons :: Collect DataFamilyInstanceInfo
dataFamilyInstanceInfoTyCons info =
  Set.insert (dfiiRepresentationTyCon info)
    . typeTyConsInto (dfiiFamilyType info)
    . each tyVarTyCons (dfiiTyVars info)

typeFamilyInstanceInfoTyCons :: Collect TypeFamilyInstanceInfo
typeFamilyInstanceInfoTyCons info =
  each tyVarTyCons (tfiiTyVars info)
    . typeTyConsInto (tfiiLeft info)
    . typeTyConsInto (tfiiRight info)

typeSchemeTyCons :: TypeScheme -> Set.Set TyCon
typeSchemeTyCons = collected typeSchemeTyConsInto

typeSchemeTyConsInto :: Collect TypeScheme
typeSchemeTyConsInto (ForAll variables predicates body) =
  each tyVarTyCons variables
    . each predTyCons predicates
    . typeTyConsInto body

tyVarTyCons :: Collect TyVarId
tyVarTyCons = typeTyConsInto . tvKind

predTyCons :: Collect Pred
predTyCons predicate = case predicate of
  ClassPred tyCon arguments -> Set.insert tyCon . each typeTyConsInto arguments
  EqPred left right -> typeTyConsInto left . typeTyConsInto right
  IParamPred _ payload -> typeTyConsInto payload
  IrredPred constraint -> typeTyConsInto constraint
  QuantifiedPred variables antecedents consequent ->
    each tyVarTyCons variables
      . each predTyCons antecedents
      . predTyCons consequent

typeTyCons :: TcType -> Set.Set TyCon
typeTyCons = collected typeTyConsInto

typeTyConsInto :: Collect TcType
typeTyConsInto ty = case ty of
  TcTyVar variable -> tyVarTyCons variable
  TcMetaTv {} -> id
  TcArrowTy -> id
  TcTyLit {} -> id
  TcTyCon tyCon arguments -> Set.insert tyCon . each typeTyConsInto arguments
  TcKindedTyCon tyCon kindArguments -> Set.insert tyCon . each typeTyConsInto kindArguments
  TcFunTy argument result -> typeTyConsInto argument . typeTyConsInto result
  TcForAllTy variable body -> tyVarTyCons variable . typeTyConsInto body
  TcQualTy predicates body -> each predTyCons predicates . typeTyConsInto body
  TcAppTy function argument -> typeTyConsInto function . typeTyConsInto argument

patSynInfoTyCons :: Collect PatSynInfo
patSynInfoTyCons info =
  typeSchemeTyConsInto (psiScheme info)
    . each predTyCons (psiReqTheta info <> psiProvTheta info)

foreignImportInfoTyCons :: Collect TcForeignImportInfo
foreignImportInfoTyCons info = case info of
  TcForeignPrimImport -> id
  TcForeignCCallImport _ plan ->
    each marshalTyCons (tcForeignArguments plan <> [tcForeignResult plan]) . case tcForeignTarget plan of
      TcForeignWrapper pointer -> marshalTyCons pointer
      _ -> id
  where
    marshalTyCons marshal =
      typeTyConsInto (tcForeignSourceType marshal) . typeTyConsInto (tcForeignPrimitiveType marshal)
