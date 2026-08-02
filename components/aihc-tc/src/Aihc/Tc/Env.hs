{-# LANGUAGE OverloadedStrings #-}

-- | Global and class environments.
--
-- These are built once per module from the parsed declarations and carry
-- type constructor info, data constructor info, class info, and instances.
module Aihc.Tc.Env
  ( -- * Global environment
    GlobalEnv (..),
    emptyGlobalEnv,

    -- * Type constructor info
    TyConFlavor (..),
    TyConInfo (..),
    TypeSynonymInfo (..),

    -- * Data constructor info
    DataConInfo (..),

    -- * Class info
    ClassInfo (..),

    -- * Instance info
    InstanceInfo (..),

    -- * Data family instances
    DataFamilyInstanceInfo (..),
    dataFamilyAxiomName,
    dataFamilyRepresentationName,
  )
where

import Aihc.Tc.Types
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Global environment, built from module declarations.
data GlobalEnv = GlobalEnv
  { geTyCons :: !(Map Text TyConInfo),
    geDataCons :: !(Map Text DataConInfo),
    geClasses :: !(Map Text ClassInfo),
    geInstances :: ![InstanceInfo]
  }
  deriving (Show)

-- | An empty global environment.
emptyGlobalEnv :: GlobalEnv
emptyGlobalEnv =
  GlobalEnv
    { geTyCons = Map.empty,
      geDataCons = Map.empty,
      geClasses = Map.empty,
      geInstances = []
    }

-- | Information about a type constructor.
data TyConFlavor
  = ClassTyCon
  | DataTyCon
  | DataFamilyTyCon
  | NewtypeTyCon
  | SynonymTyCon
  deriving (Eq, Show, Read)

data TyConInfo = TyConInfo
  { tciName :: !Text,
    tciArity :: !Int,
    tciTyCon :: !TyCon,
    tciKind :: !Kind,
    tciFlavor :: !TyConFlavor,
    tciTypeSynonym :: !(Maybe TypeSynonymInfo)
  }
  deriving (Show, Read)

data TypeSynonymInfo = TypeSynonymInfo
  { tsiParams :: ![TyVarId],
    tsiBody :: !(Maybe TcType)
  }
  deriving (Show, Read)

-- | Information about a data constructor.
--
-- This is particularly important for GADT support: the universal/existential
-- split and constructor constraints are what drive implication generation
-- during case analysis.
data DataConInfo = DataConInfo
  { dciName :: !Text,
    -- | Universally quantified type variables.
    dciUnivTyVars :: ![TyVarId],
    -- | Existentially quantified type variables (GADTs).
    dciExTyVars :: ![TyVarId],
    -- | Constructor constraints (given on match).
    dciTheta :: ![Pred],
    -- | Field types.
    dciArgTys :: ![TcType],
    -- | Result type (may mention universals).
    dciResTy :: !TcType
  }
  deriving (Show)

-- | Information about a type class.
data ClassInfo = ClassInfo
  { ciName :: !Text,
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
  deriving (Show, Read)

-- | Information about a class instance.
data InstanceInfo = InstanceInfo
  { iiClassName :: !Text,
    -- | Dictionary binding generated for this instance.
    iiDictName :: !Text,
    iiDictType :: !TcType,
    -- | Type variables quantified over.
    iiTyVars :: ![TyVarId],
    -- | Instance context (prerequisites).
    iiContext :: ![Pred],
    -- | Instance head types.
    iiHead :: ![TcType]
  }
  deriving (Show, Read)

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
  deriving (Show, Read)

dataFamilyRepresentationName :: Text -> Text -> Text
dataFamilyRepresentationName familyName firstConstructor =
  "$R$" <> familyName <> "$" <> firstConstructor

dataFamilyAxiomName :: Text -> Text -> Text
dataFamilyAxiomName familyName firstConstructor =
  "$ax$" <> familyName <> "$" <> firstConstructor
