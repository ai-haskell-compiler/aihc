{-# LANGUAGE OverloadedStrings #-}

-- | Type constructor, data constructor, class, and instance information.
--
-- The type checker state stores this information during the type check.
module Aihc.Tc.Env
  ( -- * Type constructor info
    TyConFlavor (..),
    TyConInfo (..),
    TypeSynonymInfo (..),

    -- * Datatype and constructor info
    DataTypeInfo (..),
    dataTypeKey,
    DataConInfo (..),
    PatSynDirection (..),
    PatSynInfo (..),
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConSourceForm (..),
    dataConArgTypes,

    -- * Class info
    ClassInfo (..),
    classInfoKey,

    -- * Instance info
    InstanceInfo (..),
    instanceInfoKey,
    instanceClassTyCon,
    instanceIsForClass,

    -- * Data family instances
    DataFamilyInstanceInfo (..),
    dataFamilyAxiomKey,
    dataFamilyAxiomName,
    dataFamilyRepresentationName,

    -- * Type family equations
    TypeFamilyInstanceInfo (..),
    typeFamilyAxiomKey,
    typeFamilyAxiomName,
  )
where

import Aihc.Resolve (PackageId)
import Aihc.Tc.Types
import Data.Text (Text)
import Data.Text qualified as T

-- | Information about a type constructor.
data TyConFlavor
  = ClassTyCon
  | DataTyCon
  | DataFamilyTyCon
  | NewtypeTyCon
  | SynonymTyCon
  | TypeFamilyTyCon
  deriving (Eq, Show, Read)

data TyConInfo = TyConInfo
  { tciName :: !Text,
    tciArity :: !Int,
    tciTyCon :: !TyCon,
    tciKindScheme :: !TypeScheme,
    tciFlavor :: !TyConFlavor,
    tciTypeSynonym :: !(Maybe TypeSynonymInfo)
  }
  deriving (Eq, Show, Read)

data TypeSynonymInfo = TypeSynonymInfo
  { tsiParams :: ![TyVarId],
    tsiBody :: !(Maybe TcType)
  }
  deriving (Eq, Show, Read)

-- | Checked information about a data or newtype declaration. This is the
-- semantic constructor layout consumed by deriving and exported through
-- module interfaces; downstream phases must not reconstruct it from syntax.
data DataTypeInfo = DataTypeInfo
  { dtiName :: !Text,
    dtiTyCon :: !TyCon,
    dtiTyVars :: ![TyVarId],
    dtiResultKind :: !TcType,
    dtiFlavor :: !TyConFlavor,
    dtiConstructors :: ![DataConInfo]
  }
  deriving (Eq, Show, Read)

dataTypeKey :: DataTypeInfo -> TcTypeKey
dataTypeKey = tyConKey . dtiTyCon

-- | The source declaration form of a constructor. Stock deriving needs this
-- distinction for constructor rendering and record-specific operations.
data DataConSourceForm
  = PrefixDataCon
  | InfixDataCon
  | RecordDataCon
  deriving (Eq, Show, Read)

-- | Source unpacking intent for a constructor field. This is kept separate
-- from strictness because the source syntax permits both facts to be stated.
data DataConFieldUnpack
  = NoFieldUnpack
  | UnpackField
  | NoUnpackField
  deriving (Eq, Show, Read)

-- | Checked type and source layout of one constructor field.
data DataConFieldInfo = DataConFieldInfo
  { dcfiLabel :: !(Maybe Text),
    dcfiType :: !TcType,
    dcfiStrict :: !Bool,
    dcfiLazy :: !Bool,
    dcfiUnpack :: !DataConFieldUnpack
  }
  deriving (Eq, Show, Read)

-- | Information about a data constructor.
--
-- This is particularly important for GADT support: the universal/existential
-- split and constructor constraints are what drive implication generation
-- during case analysis.
data DataConInfo = DataConInfo
  { dciName :: !Text,
    -- | Package and module that define the constructor.
    dciOrigin :: !(PackageId, Text),
    -- | Universally quantified type variables.
    dciUnivTyVars :: ![TyVarId],
    -- | Existentially quantified type variables (GADTs).
    dciExTyVars :: ![TyVarId],
    -- | Constructor constraints (given on match).
    dciTheta :: ![Pred],
    -- | Checked fields in runtime argument order.
    dciFields :: ![DataConFieldInfo],
    -- | Result type (may mention universals).
    dciResTy :: !TcType,
    dciSourceForm :: !DataConSourceForm
  }
  deriving (Eq, Show, Read)

dataConArgTypes :: DataConInfo -> [TcType]
dataConArgTypes = map dcfiType . dciFields

-- | Whether a pattern synonym can build values.
data PatSynDirection
  = -- | @pattern P x <- pat@
    PatSynUnidirectionalInfo
  | -- | @pattern P x = pat@
    PatSynImplicitBidirectionalInfo
  | -- | @pattern P x <- pat where P x = expr@
    PatSynExplicitBidirectionalInfo
  deriving (Eq, Show, Read)

-- | Checked information about a pattern synonym. The scheme has the
-- shape of a constructor type: the argument types and then the scrutinee
-- type. The matcher @$mP@ and the builder @$bP@ are ordinary terms in the
-- same module.
data PatSynInfo = PatSynInfo
  { psiName :: !Text,
    -- | Package and module that define the pattern synonym.
    psiOrigin :: !(PackageId, Text),
    psiArity :: !Int,
    psiDirection :: !PatSynDirection,
    -- | The constructor-like type. Its predicates are the required
    -- predicates and then the provided predicates.
    psiScheme :: !TypeScheme,
    -- | Constraints that a match requires from its context.
    psiReqTheta :: ![Pred],
    -- | Constraints that a match provides to its branch.
    psiProvTheta :: ![Pred]
  }
  deriving (Eq, Show, Read)

-- | Information about a type class.
data ClassInfo = ClassInfo
  { ciName :: !Text,
    -- | Exact checked type constructor for the class constraint.
    ciTyCon :: !TyCon,
    -- | Package and module that define the class.
    ciOrigin :: !(Maybe (Text, Text)),
    -- | Invisible kind parameters of the class.
    ciKindTyVars :: ![TyVarId],
    -- | Type parameters of the class.
    ciTyVars :: ![TyVarId],
    -- | Superclass constraint types. Keeping the full type permits a class
    -- parameter to appear in predicate position, as in @class c a => D c a@.
    ciSuperClassTypes :: ![TcType],
    -- | Method names and their types.
    ciMethods :: ![(Text, TypeScheme)],
    -- | Methods with a source-level default implementation.
    ciDefaultMethods :: ![Text],
    -- | Checked source-level default signatures. Unlike ordinary method
    -- signatures, their constraints become instance obligations when
    -- DeriveAnyClass selects the default implementation.
    ciDefaultSignatures :: ![(Text, TypeScheme)]
  }
  deriving (Eq, Show, Read)

-- | The identity of a class: the key of its type constructor. Two modules
-- can each declare a class with the same source name.
classInfoKey :: ClassInfo -> TcTypeKey
classInfoKey = tyConKey . ciTyCon

-- | Information about a class instance.
data InstanceInfo = InstanceInfo
  { iiClassName :: !Text,
    -- | Dictionary binding generated for this instance.
    iiDictName :: !Text,
    -- | Package and module that define the dictionary binding.
    iiDictOrigin :: !(Text, Text),
    iiDictType :: !TcType,
    -- | Type variables quantified over.
    iiTyVars :: ![TyVarId],
    -- | Instance context (prerequisites).
    iiContext :: ![Pred],
    -- | Instance head types.
    iiHead :: ![TcType]
  }
  deriving (Eq, Show, Read)

instanceInfoKey :: InstanceInfo -> ((Text, Text), Text)
instanceInfoKey instanceInfo = (iiDictOrigin instanceInfo, iiDictName instanceInfo)

-- | The exact class type constructor of an instance, read from the head of
-- its dictionary type. 'iiClassName' alone cannot tell apart two classes with
-- the same source name from different modules.
instanceClassTyCon :: InstanceInfo -> Maybe TyCon
instanceClassTyCon = go . iiDictType
  where
    go ty =
      case ty of
        TcForAllTy _ body -> go body
        TcQualTy _ body -> go body
        TcTyCon tyCon _ -> Just tyCon
        _ -> Nothing

-- | Whether an instance belongs to the class with the given type constructor.
-- Falls back to the source name when the dictionary type has no constructor
-- head.
instanceIsForClass :: TyCon -> InstanceInfo -> Bool
instanceIsForClass classTyCon instanceInfo =
  case instanceClassTyCon instanceInfo of
    Just tyCon -> tyConKey tyCon == tyConKey classTyCon
    Nothing -> iiClassName instanceInfo == tyConName classTyCon

-- | A checked standalone data-family instance equation. The representation
-- type and nominal axiom are compiler-internal names derived from the first
-- (globally unique) constructor of the instance.
data DataFamilyInstanceInfo = DataFamilyInstanceInfo
  { dfiiFamilyName :: !Text,
    dfiiFamilyType :: !TcType,
    dfiiTyVars :: ![TyVarId],
    dfiiRepresentationTyCon :: !TyCon,
    dfiiAxiomName :: !Text,
    dfiiConstructorNames :: ![Text],
    dfiiIsNewtype :: !Bool
  }
  deriving (Eq, Show, Read)

dataFamilyAxiomKey :: DataFamilyInstanceInfo -> TcAxiomKey
dataFamilyAxiomKey info =
  TcAxiomKey
    (tyConPackageId (dfiiRepresentationTyCon info))
    (tyConModuleName (dfiiRepresentationTyCon info))
    (dfiiAxiomName info)

dataFamilyRepresentationName :: Text -> Text -> Text
dataFamilyRepresentationName familyName firstConstructor =
  "$R$" <> familyName <> "$" <> firstConstructor

dataFamilyAxiomName :: Text -> Text -> Text
dataFamilyAxiomName familyName firstConstructor =
  "$ax$" <> familyName <> "$" <> firstConstructor

-- | A checked type-family equation. Fc prints this as a named axiom.
-- Do not invent equations in a later phase.
data TypeFamilyInstanceInfo = TypeFamilyInstanceInfo
  { tfiiFamilyName :: !Text,
    tfiiAxiomName :: !Text,
    tfiiOrigin :: !(PackageId, Text),
    tfiiTyVars :: ![TyVarId],
    tfiiLeft :: !TcType,
    tfiiRight :: !TcType,
    tfiiClosed :: !Bool
  }
  deriving (Eq, Show, Read)

typeFamilyAxiomKey :: TypeFamilyInstanceInfo -> TcAxiomKey
typeFamilyAxiomKey info =
  let (package, moduleName') = tfiiOrigin info
   in TcAxiomKey package moduleName' (tfiiAxiomName info)

typeFamilyAxiomName :: Text -> Int -> Text
typeFamilyAxiomName familyName index =
  "$ax$" <> familyName <> "$" <> T.pack (show index)
