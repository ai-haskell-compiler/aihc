-- | Global and class environments.
--
-- These are built once per module from the parsed declarations and carry
-- type constructor info, data constructor info, class info, and instances.
module Aihc.Tc.Env
  ( -- * Global environment
    GlobalEnv (..),
    emptyGlobalEnv,

    -- * Type constructor info
    TyConInfo (..),

    -- * Data constructor info
    DataConInfo (..),

    -- * Class info
    ClassInfo (..),

    -- * Instance info
    InstanceInfo (..),
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
data TyConInfo = TyConInfo
  { tciName :: !Text,
    tciArity :: !Int,
    tciTyCon :: !TyCon
  }
  deriving (Show)

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
    -- | Superclass predicates.
    ciSuperClasses :: ![Pred],
    -- | Method names and their types.
    ciMethods :: ![(Text, TypeScheme)]
  }
  deriving (Show)

-- | Information about a class instance.
data InstanceInfo = InstanceInfo
  { iiClassName :: !Text,
    -- | Type variables quantified over.
    iiTyVars :: ![TyVarId],
    -- | Instance context (prerequisites).
    iiContext :: ![Pred],
    -- | Instance head types.
    iiHead :: ![TcType]
  }
  deriving (Show)
