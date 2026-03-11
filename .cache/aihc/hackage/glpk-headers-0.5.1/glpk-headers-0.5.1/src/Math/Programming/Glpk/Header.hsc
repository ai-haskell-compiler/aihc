-- | Low-level bindings to the GLPK library.
--
-- Functions and enums wrapped directly from @glpk.h@ are
-- undocumented; refer to the official documentation distributed with
-- GLPK for details.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Math.Programming.Glpk.Header
  ( -- * Helper types

    -- ** Control parameters
    --
    -- $control-parameters
    BasisFactorizationControlParameters (..)
  , SimplexMethodControlParameters (..)
  , InteriorPointControlParameters (..)
  , MIPControlParameters (..)
  , MPSControlParameters (..)
  , CplexLPFormatControlParameters (..)
  , GlpkCutAttribute (..)
  , GlpkUserCutType (..)

    -- ** GLPK arrays
    --
    -- $arrays
  , GlpkArray (..)
  , mallocGlpkArray
  , allocaGlpkArray
  , initGlpkArray
  , FixedLength (..)
  , FixedLengthArray (..)

    -- ** Low-level and phantom types
  , GlpkInt (..)
  , Problem
  , GlpkColumn
  , GlpkRow
  , GlpkNodeIndex
  , GlpkTree
  , MathProgWorkspace
  , Row
  , Column
  , MathProgResult (..)

    -- ** Undocumented and unused structures
  , Unused (..)
  , BfcpFooBar
  , SmcpFooBar
  , IptcpFooBar
  , IocpFooBar
  , AttrFooBar
  , MpscpFooBar
  , CpxcpFooBar

    -- * GLPK API

    -- ** Enums
  , GlpkMajorVersion
  , glpkMajorVersion
  , GlpkMinorVersion
  , glpkMinorVersion
  , GlpkDirection
  , glpkMin
  , glpkMax
  , GlpkVariableType (..)
  , glpkContinuous
  , glpkInteger
  , glpkBinary
  , GlpkConstraintType
  , glpkFree
  , glpkGT
  , glpkLT
  , glpkBounded
  , glpkFixed
  , GlpkVariableStatus
  , glpkBasic
  , glpkNonBasicLower
  , glpkNonBasicUpper
  , glpkNonBasicFree
  , glpkNonBasicFixed
  , GlpkScaling
  , glpkGeometricMeanScaling
  , glpkEquilibrationScaling
  , glpkPowerOfTwoScaling
  , glpkSkipScaling
  , glpkAutoScaling
  , GlpkSolutionType
  , glpkBasicSolution
  , glpkInteriorPointSolution
  , glpkMIPSolution
  , GlpkSolutionStatus
  , glpkOptimal
  , glpkFeasible
  , glpkInfeasible
  , glpkNoFeasible
  , glpkUnbounded
  , glpkUndefined
  , GlpkMessageLevel
  , glpkMessageOff
  , glpkMessageError
  , glpkMessageOn
  , glpkMessageAll
  , glpkMessageDebug
  , GlpkSimplexMethod
  , glpkPrimalSimplex
  , glpkDualSimplex
  , glpkDualPSimplex
  , GlpkPricing
  , glpkTextbookPricing
  , glpkStandardPricing
  , glpkProjectedSteepestEdge
  , GlpkRatioTest
  , glpkStandardRatioTest
  , glpkHarrisTwoPassRatioTest
  , GlpkPreCholeskyOrdering
  , glpkNatural
  , glpkQuotientMinimumDegree
  , glpkApproximateMinimumDegree
  , glpkSymmetricApproximateMinimumDegree
  , GlpkBranchingTechnique
  , glpkFirstFractional
  , glpkLastFractional
  , glpkMostFractional
  , glpkDriebeckTomlin
  , glpkHybridPseudoCost
  , GlpkBacktrackingTechnique
  , glpkDepthFirstSearch
  , glpkBreadthFirstSearch
  , glpkBestLocalBound
  , glpkBestProjectionHeuristic
  , GlpkPreProcessingTechnique
  , glpkPreProcessNone
  , glpkPreProcessRoot
  , glpkPreProcessAll
  , GlpkFeasibilityPump
  , glpkFeasibilityPumpOn
  , glpkFeasibilityPumpOff
  , GlpkProximitySearch
  , glpkProximitySearchOn
  , glpkProximitySearchOff
  , GlpkGomoryCuts
  , glpkGomoryCutsOn
  , glpkGomoryCutsOff
  , GlpkMIRCuts
  , glpkMIRCutsOn
  , glpkMIRCutsOff
  , GlpkCoverCuts
  , glpkCoverCutsOn
  , glpkCoverCutsOff
  , GlpkCliqueCuts
  , glpkCliqueCutsOn
  , glpkCliqueCutsOff
  , GlpkPresolve
  , glpkPresolveOn
  , glpkPresolveOff
  , GlpkBinarization
  , glpkBinarizationOn
  , glpkBinarizationOff
  , GlpkSimpleRounding
  , glpkSimpleRoundingOn
  , glpkSimpleRoundingOff
  , GlpkConstraintOrigin
  , glpkRegularConstraint
  , glpkLazyConstraint
  , glpkCuttingPlaneConstraint
  , GlpkCutType
  , glpkGomoryCut
  , glpkMIRCut
  , glpkCoverCut
  , glpkCliqueCut
  , GlpkControl
  , glpkOn
  , glpkOff
  , GlpkCallbackReason
  , glpkSubproblemSelection
  , glpkPreprocessing
  , glpkRowGeneration
  , glpkHeuristicSolution
  , glpkCutGeneration
  , glpkBranching
  , glpkNewIncumbent
  , GlpkBranchOption
  , glpkBranchUp
  , glpkBranchDown
  , glpkBranchAuto
  , GlpkFactorizationResult
  , glpkFactorizationSuccess
  , glpkFactorizationBadBasis
  , glpkFactorizationSingular
  , glpkFactorizationIllConditioned
  , GlpkSimplexStatus
  , glpkSimplexSuccess
  , glpkSimplexBadBasis
  , glpkSimplexSingular
  , glpkSimplexIllConditioned
  , glpkSimplexBadBound
  , glpkSimplexFailure
  , glpkSimplexDualLowerLimitFailure
  , glpkSimplexDualUpperLimitFailure
  , glpkSimplexIterationLimit
  , glpkSimplexTimeLimit
  , glpkSimplexPrimalInfeasible
  , glpkSimplexDualInfeasible
  , GlpkMIPStatus
  , glpkMIPSuccess
  , glpkMIPBadBound
  , glpkMIPNoBasis
  , glpkMIPPrimalInfeasible
  , glpkMIPDualInfeasible
  , glpkMIPFailure
  , glpkMIPRelativeGap
  , glpkMIPTimeLimit
  , glpkMIPStopped
  , GlpkInteriorPointStatus
  , glpkInteriorPointSuccess
  , glpkInteriorPointFailure
  , glpkInteriorPointNoConvergence
  , glpkInteriorPointIterationLimit
  , glpkInteriorPointNumericalInstability
  , GlpkKKTCheck
  , glpkKKTPrimalEquality
  , glpkKKTPrimalBound
  , glpkKKTDualEquality
  , glpkKKTDualBound
  , GlpkMPSFormat
  , glpkMPSAncient
  , glpkMPSDeck
  , glpkMPSModern
  , GlpkFactorizationType
  , glpkLUForrestTomlin
  , glpkLUSchurCompBartelsGolub
  , glpkLUSchurGivensRotation
  , glpkBTSchurBartelsGolub
  , glpkBTSchurGivensRotation

  -- ** Functions
  , glp_create_prob
  , glp_delete_prob
  , glp_set_prob_name
  , glp_set_obj_name
  , glp_set_obj_dir
  , glp_add_rows
  , glp_add_cols
  , glp_set_row_name
  , glp_set_col_name
  , glp_set_row_bnds
  , glp_set_col_bnds
  , glp_set_obj_coef
  , glp_set_mat_row
  , glp_set_mat_col
  , glp_load_matrix
  , glp_check_dup
  , glp_sort_matrix
  , glp_del_rows
  , glp_del_cols
  , glp_copy_prob
  , glp_erase_prob
  , glp_get_prob_name
  , glp_get_obj_name
  , glp_get_obj_dir
  , glp_get_num_rows
  , glp_get_num_cols
  , glp_get_row_name
  , glp_get_col_name
  , glp_get_row_type
  , glp_get_row_lb
  , glp_get_row_ub
  , glp_get_col_type
  , glp_get_col_lb
  , glp_get_col_ub
  , glp_get_obj_coef
  , glp_get_num_nz
  , glp_get_mat_row
  , glp_get_mat_col
  , glp_create_index
  , glp_delete_index
  , glp_find_row
  , glp_find_col
  , glp_set_rii
  , glp_get_rii
  , glp_set_sjj
  , glp_get_sjj
  , glp_scale_prob
  , glp_unscale_prob
  , glp_set_row_stat
  , glp_set_col_stat
  , glp_std_basis
  , glp_adv_basis
  , glp_cpx_basis
  , glp_simplex
  , glp_exact
  , glp_init_smcp
  , glp_get_status
  , glp_get_prim_stat
  , glp_get_dual_stat
  , glp_get_obj_val
  , glp_get_row_stat
  , glp_get_col_stat
  , glp_get_row_prim
  , glp_get_row_dual
  , glp_get_col_prim
  , glp_get_col_dual
  , glp_get_unbnd_ray
  , glp_get_bfcp
  , glp_set_bfcp
  , glp_interior
  , glp_init_iptcp
  , glp_ipt_status
  , glp_intopt
  , glp_mip_status
  , glp_mip_obj_val
  , glp_mip_row_val
  , glp_mip_col_val
  , glp_check_kkt
  , glp_print_sol
  , glp_read_sol
  , glp_write_sol
  , glp_print_ranges
  , glp_print_ipt
  , glp_read_ipt
  , glp_write_ipt
  , glp_print_mip
  , glp_read_mip
  , glp_write_mip
  , glp_bf_exists
  , glp_factorize
  , glp_bf_updated
  , glp_get_bhead
  , glp_get_row_bind
  , glp_get_col_bind
  , glp_ftran
  , glp_btran
  , glp_warm_up
  , glp_eval_tab_row
  , glp_eval_tab_col
  , glp_transform_row
  , glp_transform_col
  , glp_prim_rtest
  , glp_dual_rtest
  , glp_analyze_bound
  , glp_analyze_coef
  , glp_init_iocp
  , glp_ipt_obj_val
  , glp_ipt_row_prim
  , glp_ipt_row_dual
  , glp_ipt_col_prim
  , glp_ipt_col_dual
  , glp_ios_reason
  , glp_ios_get_prob
  , glp_ios_tree_size
  , glp_ios_curr_node
  , glp_ios_next_node
  , glp_ios_prev_node
  , glp_ios_up_node
  , glp_ios_node_level
  , glp_ios_node_bound
  , glp_ios_best_node
  , glp_ios_mip_gap
  , glp_ios_node_data
  , glp_ios_row_attr
  , glp_ios_pool_size
  , glp_ios_add_row
  , glp_ios_del_row
  , glp_ios_clear_pool
  , glp_ios_can_branch
  , glp_ios_branch_upon
  , glp_ios_select_node
  , glp_ios_heur_sol
  , glp_ios_terminate
  , glp_set_col_kind
  , glp_get_col_kind
  , glp_get_num_int
  , glp_get_num_bin
  , glp_init_mpscp
  , glp_read_mps
  , glp_write_mps
  , glp_init_cpxcp
  , glp_read_lp
  , glp_write_lp
  , glp_read_prob
  , glp_write_prob
  , glp_mpl_alloc_wksp
  , glp_mpl_free_wksp
  , glp_mpl_init_rand
  , glp_mpl_read_model
  , glp_mpl_read_data
  , glp_mpl_generate
  , glp_mpl_build_prob
  , glp_mpl_postsolve
  , glp_read_cnfstat
  , glp_write_cnfstat
  , glp_minisat1
  , glp_intfeas1
  , glp_init_env
  , glp_free_env
  , glp_version
  , glp_config
  , glp_term_out
  , glp_term_hook
  , glp_error_hook
  -- ** Helper functions
  , mkHaskellErrorHook
  , mkHaskellTermHook
  , mkHaskellMIPCallback
  ) where

import Data.Typeable
import GHC.Generics (Generic)
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic

#include <glpk.h>

-- Low-level and phantom types

-- | A phantom type representing a problem in GLPK.
data Problem

-- | Phantom type used to denote data as being a column.
data GlpkColumn

-- | Phantom type used to denote data as being a row.
data GlpkRow

-- | Phantom type used to denote data as being a node index.
data GlpkNodeIndex

-- | Phantom type indicating the data stored in MIP callbacks.
data GlpkTree a

-- | Phantom type used to denote pointers to workspaces.
data MathProgWorkspace

-- | Wrapper around 'CInt' values, tagged with a phantom type to help
-- track what it refers to.
newtype GlpkInt a
  = GlpkInt { fromGlpkInt :: CInt }
  deriving
    ( Enum
    , Eq
    , Integral
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | Convenient alias for rows.
type Row = GlpkInt GlpkRow

-- | Convenient alias for columns.
type Column = GlpkInt GlpkColumn

-- GLPK Arrays

-- $arrays
--
-- GLPK uses a 1-based indexing for arrays. This is accomplished by
-- ignoring the 0th entry.

-- | An array whose data begins at index 1
newtype GlpkArray a
  = GlpkArray { fromGlpkArray :: Ptr a }
  deriving
    ( Eq
    , Ord
    , Show
    , Storable
    )

-- | A type used to represent an unused or undocumented struct member.
newtype Unused a
  = Unused { fromUnused :: a }
  deriving
    ( Enum
    , Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

-- | The class of arrays of fixed length.
class FixedLength a where
  fixedLength :: a -> Int

-- | A type representing fixed-length array members of structs.
newtype FixedLengthArray a b
  = FixedLengthArray { fromFixedLengthArray :: [b] }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

instance (FixedLength a, Storable b) => GStorable (FixedLengthArray a b) where
  gsizeOf _ = (fixedLength (undefined :: a)) * (sizeOf (undefined :: b))

  galignment _ = alignment (undefined :: b)

  gpeekByteOff ptr offset
    = FixedLengthArray <$> peekArray arrayLength (plusPtr ptr offset)
    where
      arrayLength = fixedLength (undefined :: a)

  gpokeByteOff ptr offset (FixedLengthArray array)
    = pokeArray (plusPtr ptr offset) array

-- | Create a new 'GlpkArray'.
mallocGlpkArray :: (Storable a) => [a] -> IO (GlpkArray a)
mallocGlpkArray xs = do
  array <- mallocArray (1 + length xs)
  initGlpkArray xs array

-- | Run a computation with a temporary 'GlpkArray'.
allocaGlpkArray :: (Storable a) => [a] -> (GlpkArray a -> IO b) -> IO b
allocaGlpkArray xs f
  = allocaArray (1 + length xs) $ \array -> initGlpkArray xs array >>= f

-- | Set the contents of a 'GlpkArray' from a list.
initGlpkArray :: (Storable a) => [a] -> Ptr a -> IO (GlpkArray a)
initGlpkArray xs array =
  let
    elemSize :: Int
    elemSize = sizeOf (head xs)
  in do
    pokeArray (plusPtr array elemSize) xs
    return (GlpkArray array)

-- Control parameters

-- $control-parameters
--
-- These structures wrap the low-level control structures used to
-- change the behavior of various solver functions. You will likely
-- want to utilize these.

data BasisFactorizationControlParameters
  = BasisFactorizationControlParameters
    { bfcpMessageLevel :: Unused GlpkMessageLevel
    , bfcpType :: GlpkFactorizationType
    , bfcpLUSize :: Unused CInt
    , bfcpPivotTolerance :: CDouble
    , bfcpPivotLimit :: CInt
    , bfcpSuhl :: GlpkControl
    , bfcpEpsilonTolerance :: CDouble
    , bfcpMaxGro :: Unused CDouble
    , bfcpNfsMax :: CInt
    , bfcpUpdateTolerance :: Unused CDouble
    , bfcpNrsMax :: CInt
    , bfcpRsSize :: Unused CInt
    , bfcpFooBar :: Unused (FixedLengthArray BfcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable BasisFactorizationControlParameters

data SimplexMethodControlParameters
  = SimplexMethodControlParameters
    { smcpMessageLevel :: GlpkMessageLevel
    , smcpMethod :: GlpkSimplexMethod
    , smcpPricing :: GlpkPricing
    , smcpRatioTest :: GlpkRatioTest
    , smcpPrimalFeasibilityTolerance :: Double
    , smcpDualFeasibilityTolerance :: Double
    , smcpPivotTolerance :: Double
    , smcpLowerObjectiveLimit :: Double
    , smcpUpperObjectiveLimit :: Double
    , smcpIterationLimit :: CInt
    , smcpTimeLimitMillis :: CInt
    , smcpOutputFrequencyMillis :: CInt
    , smcpOutputDelayMillis :: CInt
    , smcpPresolve :: GlpkPresolve
    , smcpExcl :: Unused CInt
    , smcpShift :: Unused CInt
    , smcpAOrN :: Unused CInt
    , smcpFooBar :: Unused (FixedLengthArray SmcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable SimplexMethodControlParameters

data InteriorPointControlParameters
  = InteriorPointControlParameters
    { iptcpMessageLevel :: GlpkMessageLevel
    , iptcpOrderingAlgorithm :: GlpkPreCholeskyOrdering
    , iptcpFooBar :: Unused (FixedLengthArray IptcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable InteriorPointControlParameters

data MIPControlParameters a
  = MIPControlParameters
    { iocpMessageLevel :: GlpkMessageLevel
    , iocpBranchingTechnique :: GlpkBranchingTechnique
    , iocpBacktrackingTechnique :: GlpkBacktrackingTechnique
    , iocpAbsoluteFeasibilityTolerance :: CDouble
    , iocpRelativeObjectiveTolerance :: CDouble
    , iocpTimeLimitMillis :: CInt
    , iocpOutputFrequencyMillis :: CInt
    , iocpOutputDelayMillis :: CInt
    , iocpCallback :: FunPtr (Ptr (GlpkTree a) -> Ptr a -> IO ())
    , iocpNodeData :: Ptr a
    , iocpNodeDataSize :: CInt
    , iocpPreprocessingTechnique :: GlpkPreProcessingTechnique
    , iocpRelativeMIPGap :: CDouble
    , iocpMIRCuts :: GlpkMIRCuts
    , iocpGormoryCuts :: GlpkGomoryCuts
    , iocpCoverCuts :: GlpkCoverCuts
    , iocpCliqueCuts :: GlpkCliqueCuts
    , iocpPresolve :: GlpkPresolve
    , iocpBinarization :: GlpkBinarization
    , iocpFeasibilityPump :: GlpkFeasibilityPump
    , iocpProximitySearch :: GlpkProximitySearch
    , iocpProximityTimeLimitMillis :: CInt
    , iocpSimpleRounding :: GlpkSimpleRounding
    , iocpUseExistingSolution :: Unused CInt
    , iocpNewSolutionFileName :: Unused (Ptr CChar)
    , iocpUseAlienSolver :: Unused CInt
    , iocpUseLongStepDual :: Unused CInt
    , iocpFooBar :: Unused (FixedLengthArray IocpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable (MIPControlParameters a)

data GlpkCutAttribute
  = GlpkCutAttribute
    { attrLevel :: CInt
    , attrContraintOrigin :: GlpkConstraintOrigin
    , attrCutType :: GlpkCutType
    , attrFooBar :: Unused (FixedLengthArray AttrFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable GlpkCutAttribute

newtype GlpkUserCutType
  = GlpkUserCutType { fromGlpkUserCutType :: CInt }
  deriving
    ( Enum
    , Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

data MPSControlParameters
  = MPSControlParameters
    { mpscpBlank :: CInt
    , mpscpObjectiveName :: CString
    , mpscpZeroTolerance :: CDouble
    , mpscpFooBar :: Unused (FixedLengthArray MpscpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable MPSControlParameters

data CplexLPFormatControlParameters
  = CplexLPFormatControlParameters
    { cpxcpFooBar :: Unused (FixedLengthArray CpxcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable CplexLPFormatControlParameters

newtype MathProgResult
  = MathProgResult { fromMathProgResult :: CInt }
  deriving
    ( Enum
    , Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )


data BfcpFooBar

instance FixedLength BfcpFooBar where
  fixedLength _ = 38

data SmcpFooBar

instance FixedLength SmcpFooBar where
  fixedLength _ = 33

data IptcpFooBar

instance FixedLength IptcpFooBar where
  fixedLength _ = 48

data IocpFooBar

instance FixedLength IocpFooBar where
  fixedLength _ = 23

data AttrFooBar

instance FixedLength AttrFooBar where
  fixedLength _ = 7

data MpscpFooBar

instance FixedLength MpscpFooBar where
  fixedLength _ = 17

data CpxcpFooBar

instance FixedLength CpxcpFooBar where
  fixedLength _ = 20

-- Enums

newtype GlpkMajorVersion
  = GlpkMajorVersion { fromGlpkMajorVersion :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkMajorVersion
 , GlpkMajorVersion
 , glpkMajorVersion = GLP_MAJOR_VERSION
 }

newtype GlpkMinorVersion
  = GlpkMinorVersion { fromGlpkMinorVersion :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkMinorVersion
 , GlpkMinorVersion
 , glpkMinorVersion = GLP_MINOR_VERSION
 }

newtype GlpkDirection
  = GlpkDirection { fromGlpkDirection :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkDirection
 , GlpkDirection
 , glpkMin = GLP_MIN
 , glpkMax = GLP_MAX
 }

newtype GlpkVariableType
  = GlpkVariableType { fromGlpkVariableType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkVariableType
 , GlpkVariableType
 , glpkContinuous = GLP_CV
 , glpkInteger = GLP_IV
 , glpkBinary = GLP_BV
 }

newtype GlpkConstraintType
  = GlpkConstraintType { fromGlpkConstraintType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkConstraintType
 , GlpkConstraintType
 , glpkFree = GLP_FR
 , glpkGT = GLP_LO
 , glpkLT = GLP_UP
 , glpkBounded = GLP_DB
 , glpkFixed = GLP_FX
 }

newtype GlpkVariableStatus
  = GlpkVariableStatus { fromGlpkVariableStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkVariableStatus
 , GlpkVariableStatus
 , glpkBasic = GLP_BS
 , glpkNonBasicLower = GLP_NL
 , glpkNonBasicUpper = GLP_NU
 , glpkNonBasicFree = GLP_NF
 , glpkNonBasicFixed = GLP_NS
 }

newtype GlpkScaling
  = GlpkScaling { fromGlpkScaling :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkScaling
 , GlpkScaling
 , glpkGeometricMeanScaling = GLP_SF_GM
 , glpkEquilibrationScaling = GLP_SF_EQ
 , glpkPowerOfTwoScaling = GLP_SF_2N
 , glpkSkipScaling = GLP_SF_SKIP
 , glpkAutoScaling = GLP_SF_AUTO
 }

newtype GlpkSolutionType
  = GlpkSolutionType { fromGlpkSolutionType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkSolutionType
 , GlpkSolutionType
 , glpkBasicSolution = GLP_SOL
 , glpkInteriorPointSolution = GLP_IPT
 , glpkMIPSolution = GLP_MIP
 }

newtype GlpkSolutionStatus
  = GlpkSolutionStatus { fromGlpkSolutionStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkSolutionStatus
 , GlpkSolutionStatus
 , glpkOptimal = GLP_OPT
 , glpkFeasible = GLP_FEAS
 , glpkInfeasible = GLP_INFEAS
 , glpkNoFeasible = GLP_NOFEAS
 , glpkUnbounded = GLP_UNBND
 , glpkUndefined = GLP_UNDEF
 }

newtype GlpkMessageLevel
  = GlpkMessageLevel { fromGlpkMessageLevel :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkMessageLevel
 , GlpkMessageLevel
 , glpkMessageOff = GLP_MSG_OFF
 , glpkMessageError = GLP_MSG_ERR
 , glpkMessageOn = GLP_MSG_ON
 , glpkMessageAll = GLP_MSG_ALL
 , glpkMessageDebug = GLP_MSG_DBG
 }

newtype GlpkSimplexMethod
  = GlpkSimplexMethod { fromGlpkSimplexMethod :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkSimplexMethod
 , GlpkSimplexMethod
 , glpkPrimalSimplex = GLP_PRIMAL
 , glpkDualSimplex = GLP_DUAL
 , glpkDualPSimplex = GLP_DUALP
 }

newtype GlpkPricing
  = GlpkPricing { fromGlpkPricing :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkPricing
 , GlpkPricing
 , glpkTextbookPricing = GLP_PT_STD
 , glpkStandardPricing = GLP_PT_STD
 , glpkProjectedSteepestEdge = GLP_PT_PSE
 }

newtype GlpkRatioTest
  = GlpkRatioTest { fromGlpkRatioTest :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkRatioTest
 , GlpkRatioTest
 , glpkStandardRatioTest = GLP_RT_STD
 , glpkHarrisTwoPassRatioTest = GLP_RT_HAR
 }

newtype GlpkPreCholeskyOrdering
  = GlpkPreCholeskyOrdering { fromGlpkPreCholeskyOrdering :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkPreCholeskyOrdering
 , GlpkPreCholeskyOrdering
 , glpkNatural = GLP_ORD_NONE
 , glpkQuotientMinimumDegree = GLP_ORD_QMD
 , glpkApproximateMinimumDegree = GLP_ORD_AMD
 , glpkSymmetricApproximateMinimumDegree = GLP_ORD_SYMAMD
 }

newtype GlpkBranchingTechnique
  = GlpkBranchingTechnique { fromGlpkBranchingTechnique :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkBranchingTechnique
 , GlpkBranchingTechnique
 , glpkFirstFractional = GLP_BR_FFV
 , glpkLastFractional = GLP_BR_LFV
 , glpkMostFractional = GLP_BR_MFV
 , glpkDriebeckTomlin = GLP_BR_DTH
 , glpkHybridPseudoCost = GLP_BR_PCH
 }

newtype GlpkBacktrackingTechnique
  = GlpkBacktrackingTechnique { fromGlpkBacktrackingTechnique :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkBacktrackingTechnique
 , GlpkBacktrackingTechnique
 , glpkDepthFirstSearch = GLP_BT_DFS
 , glpkBreadthFirstSearch = GLP_BT_BFS
 , glpkBestLocalBound = GLP_BT_BLB
 , glpkBestProjectionHeuristic = GLP_BT_BPH
 }

newtype GlpkPreProcessingTechnique
  = GlpkPreProcessingTechnique { fromGlpkPreProcessingTechnique :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkPreProcessingTechnique
 , GlpkPreProcessingTechnique
 , glpkPreProcessNone = GLP_PP_NONE
 , glpkPreProcessRoot = GLP_PP_ROOT
 , glpkPreProcessAll = GLP_PP_ALL
 }

newtype GlpkFeasibilityPump
  = GlpkFeasibilityPump { fromGlpkFeasibilityPump :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkFeasibilityPump
 , GlpkFeasibilityPump
 , glpkFeasibilityPumpOn = GLP_ON
 , glpkFeasibilityPumpOff = GLP_OFF
 }

newtype GlpkProximitySearch
  = GlpkProximitySearch { fromGlpkProximitySearch :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkProximitySearch
 , GlpkProximitySearch
 , glpkProximitySearchOn = GLP_ON
 , glpkProximitySearchOff = GLP_OFF
 }

newtype GlpkGomoryCuts
  = GlpkGomoryCuts { fromGlpkGomoryCuts :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkGomoryCuts
 , GlpkGomoryCuts
 , glpkGomoryCutsOn = GLP_ON
 , glpkGomoryCutsOff = GLP_OFF
 }

newtype GlpkMIRCuts
  = GlpkMIRCuts { fromGlpkMIRCuts :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkMIRCuts
 , GlpkMIRCuts
 , glpkMIRCutsOn = GLP_ON
 , glpkMIRCutsOff = GLP_OFF
 }

newtype GlpkCoverCuts
  = GlpkCoverCuts { fromGlpkCoverCuts :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkCoverCuts
 , GlpkCoverCuts
 , glpkCoverCutsOn = GLP_ON
 , glpkCoverCutsOff = GLP_OFF
 }

newtype GlpkCliqueCuts
  = GlpkCliqueCuts { fromGlpkCliqueCuts :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkCliqueCuts
 , GlpkCliqueCuts
 , glpkCliqueCutsOn = GLP_ON
 , glpkCliqueCutsOff = GLP_OFF
 }

newtype GlpkPresolve
  = GlpkPresolve { fromGlpkPresolve :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkPresolve
 , GlpkPresolve
 , glpkPresolveOn = GLP_ON
 , glpkPresolveOff = GLP_OFF
 }

newtype GlpkBinarization
  = GlpkBinarization { fromGlpkBinarization :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkBinarization
 , GlpkBinarization
 , glpkBinarizationOn = GLP_ON
 , glpkBinarizationOff = GLP_OFF
 }

newtype GlpkSimpleRounding
  = GlpkSimpleRounding { fromGlpkSimpleRounding :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkSimpleRounding
 , GlpkSimpleRounding
 , glpkSimpleRoundingOn = GLP_ON
 , glpkSimpleRoundingOff = GLP_OFF
 }

newtype GlpkConstraintOrigin
  = GlpkConstraintOrigin { fromGlpkConstraintOrigin :: CInt }
  deriving
    ( Eq
    , Generic
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkConstraintOrigin
 , GlpkConstraintOrigin
 , glpkRegularConstraint = GLP_RF_REG
 , glpkLazyConstraint = GLP_RF_LAZY
 , glpkCuttingPlaneConstraint = GLP_RF_CUT
 }

newtype GlpkCutType
  = GlpkCutType { fromGlpkCutType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkCutType
 , GlpkCutType
 , glpkGomoryCut = GLP_RF_GMI
 , glpkMIRCut = GLP_RF_MIR
 , glpkCoverCut = GLP_RF_COV
 , glpkCliqueCut = GLP_RF_CLQ
 }

newtype GlpkControl
  = GlpkControl { fromGlpkControl :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkControl
 , GlpkControl
 , glpkOn = GLP_ON
 , glpkOff = GLP_OFF
 }

newtype GlpkCallbackReason
  = GlpkCallbackReason { fromGlpkCallbackReason :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkCallbackReason
 , GlpkCallbackReason
 , glpkSubproblemSelection = GLP_ISELECT
 , glpkPreprocessing = GLP_IPREPRO
 , glpkRowGeneration = GLP_IROWGEN
 , glpkHeuristicSolution = GLP_IHEUR
 , glpkCutGeneration = GLP_ICUTGEN
 , glpkBranching = GLP_IBRANCH
 , glpkNewIncumbent = GLP_IBINGO
 }

newtype GlpkBranchOption
  = GlpkBranchOption { fromGlpkBranchOption :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkBranchOption
 , GlpkBranchOption
 , glpkBranchUp = GLP_UP_BRNCH
 , glpkBranchDown = GLP_DN_BRNCH
 , glpkBranchAuto = GLP_NO_BRNCH
 }

newtype GlpkFactorizationResult
  = GlpkFactorizationResult { fromGlpkFactorizationResult :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkFactorizationResult
 , GlpkFactorizationResult
 , glpkFactorizationSuccess = 0
 , glpkFactorizationBadBasis = GLP_EBADB
 , glpkFactorizationSingular = GLP_ESING
 , glpkFactorizationIllConditioned = GLP_ECOND
 }

newtype GlpkSimplexStatus
  = GlpkSimplexStatus { fromGlpkSimplexStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkSimplexStatus
 , GlpkSimplexStatus
 , glpkSimplexSuccess = 0
 , glpkSimplexBadBasis = GLP_EBADB
 , glpkSimplexSingular = GLP_ESING
 , glpkSimplexIllConditioned = GLP_ECOND
 , glpkSimplexBadBound = GLP_EBOUND
 , glpkSimplexFailure = GLP_EFAIL
 , glpkSimplexDualLowerLimitFailure = GLP_EOBJLL
 , glpkSimplexDualUpperLimitFailure = GLP_EOBJUL
 , glpkSimplexIterationLimit = GLP_EITLIM
 , glpkSimplexTimeLimit = GLP_ETMLIM
 , glpkSimplexPrimalInfeasible = GLP_ENOPFS
 , glpkSimplexDualInfeasible = GLP_ENODFS
 }

newtype GlpkMIPStatus
  = GlpkMIPStatus { fromGlpkMIPStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkMIPStatus
 , GlpkMIPStatus
 , glpkMIPSuccess = 0
 , glpkMIPBadBound = GLP_EBOUND
 , glpkMIPNoBasis = GLP_EROOT
 , glpkMIPPrimalInfeasible = GLP_ENOPFS
 , glpkMIPDualInfeasible =  GLP_ENODFS
 , glpkMIPFailure = GLP_EFAIL
 , glpkMIPRelativeGap = GLP_EMIPGAP
 , glpkMIPTimeLimit = GLP_ETMLIM
 , glpkMIPStopped = GLP_ESTOP
 }

newtype GlpkInteriorPointStatus
  = GlpkInteriorPointStatus { fromGlpkInteriorPointStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkInteriorPointStatus
 , GlpkInteriorPointStatus
 , glpkInteriorPointSuccess = 0
 , glpkInteriorPointFailure = GLP_EFAIL
 , glpkInteriorPointNoConvergence = GLP_ENOCVG
 , glpkInteriorPointIterationLimit = GLP_EITLIM
 , glpkInteriorPointNumericalInstability = GLP_EINSTAB
 }

newtype GlpkKKTCheck
  = GlpkKKTCheck { fromGlpkKKTCheck :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkKKTCheck
 , GlpkKKTCheck
 , glpkKKTPrimalEquality = GLP_KKT_PE
 , glpkKKTPrimalBound = GLP_KKT_PB
 , glpkKKTDualEquality = GLP_KKT_DE
 , glpkKKTDualBound = GLP_KKT_DB
 }

newtype GlpkMPSFormat
  = GlpkMPSFormat { fromGlpkMPSFormat :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkMPSFormat
 , GlpkMPSFormat
 , glpkMPSAncient = GLP_MPS_DECK
 , glpkMPSDeck = GLP_MPS_DECK
 , glpkMPSModern = GLP_MPS_FILE
 }

newtype GlpkFactorizationType
  = GlpkFactorizationType { fromGlpkFactorizationType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    , Typeable
    )

#{enum
   GlpkFactorizationType
 , GlpkFactorizationType
 , glpkLUForrestTomlin = GLP_BF_LUF + GLP_BF_FT
 , glpkLUSchurCompBartelsGolub = GLP_BF_LUF + GLP_BF_BG
 , glpkLUSchurGivensRotation = GLP_BF_LUF + GLP_BF_GR
 , glpkBTSchurBartelsGolub = GLP_BF_BTF + GLP_BF_BG
 , glpkBTSchurGivensRotation = GLP_BF_BTF + GLP_BF_GR
 }

-- Functions

foreign import ccall "glp_create_prob" glp_create_prob
  :: IO (Ptr Problem)
  -- ^ The allocated problem instance

foreign import ccall "glp_delete_prob" glp_delete_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_set_prob_name" glp_set_prob_name
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The problem name
  -> IO ()

foreign import ccall "glp_set_obj_name" glp_set_obj_name
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The objective name
  -> IO ()

foreign import ccall "glp_set_obj_dir" glp_set_obj_dir
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkDirection
  -- ^ Whether the problem is a minimization or maximization problem
  -> IO ()

foreign import ccall "glp_add_rows" glp_add_rows
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of constraints to add
  -> IO Row
  -- ^ The index of the first new constraint added

foreign import ccall "glp_add_cols" glp_add_cols
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of variables to add
  -> IO Column
  -- ^ The index of the first new variable added

foreign import ccall "glp_set_row_name" glp_set_row_name
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint being named
  -> CString
  -- ^ The name of the constraint
  -> IO ()

foreign import ccall "glp_set_col_name" glp_set_col_name
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable being named
  -> CString
  -- ^ The name of the variable
  -> IO ()

foreign import ccall "glp_set_row_bnds" glp_set_row_bnds
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint being bounded
  -> GlpkConstraintType
  -- ^ The type of constraint
  -> CDouble
  -- ^ The lower bound
  -> CDouble
  -- ^ The upper bound
  -> IO ()

foreign import ccall "glp_set_col_bnds" glp_set_col_bnds
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable being bounded
  -> GlpkConstraintType
  -- ^ The type of constraint
  -> CDouble
  -- ^ The lower bound
  -> CDouble
  -- ^ The upper bound
  -> IO ()

foreign import ccall "glp_set_obj_coef" glp_set_obj_coef
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable
  -> CDouble
  -- ^ The objective coefficient
  -> IO ()

foreign import ccall "glp_set_mat_row" glp_set_mat_row
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint being modified
  -> CInt
  -- ^ The number of variables being set
  -> GlpkArray Column
  -- ^ The variables being set
  -> GlpkArray CDouble
  -- ^ The variable coefficients
  -> IO ()

foreign import ccall "glp_set_mat_col" glp_set_mat_col
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable being modified
  -> CInt
  -- ^ The number of coefficients being set
  -> Ptr Row
  -- ^ The constraints being modified
  -> GlpkArray CDouble
  -- ^ The variable coefficients
  -> IO ()

foreign import ccall "glp_load_matrix" glp_load_matrix
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of nonzero elements to be loaded
  -> GlpkArray Row
  -- ^ The constraint indices
  -> GlpkArray Column
  -- ^ The variable indices
  -> GlpkArray CDouble
  -- ^ The coefficients
  -> IO ()

foreign import ccall "glp_check_dup" glp_check_dup
  :: CInt
  -- ^ The number of rows in the matrix
  -> CInt
  -- ^ The number of columns in the matrix
  -> CInt
  -- ^ The number of nonzeros in the matrix
  -> GlpkArray CInt
  -- ^ The rows being checked
  -> GlpkArray CInt
  -- ^ The columns being checked
  -> CInt

foreign import ccall "glp_sort_matrix" glp_sort_matrix
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_del_rows" glp_del_rows
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of constraints to delete
  -> GlpkArray Row
  -- ^ The indices of the constraints to delete
  -> IO ()

foreign import ccall "glp_del_cols" glp_del_cols
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of variables to delete
  -> GlpkArray Column
  -- ^ The indices of the variables to delete
  -> IO ()

foreign import ccall "glp_copy_prob" glp_copy_prob
  :: Ptr Problem
  -- ^ The destination problem instance
  -> Ptr Problem
  -- ^ The problem instance to be copied
  -> GlpkControl
  -- ^ Whether to copy symbolic names
  -> IO ()

foreign import ccall "glp_erase_prob" glp_erase_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_get_prob_name" glp_get_prob_name
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CString
  -- ^ The name of the problem

foreign import ccall "glp_get_obj_name" glp_get_obj_name
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CString
  -- ^ The name of the objective

foreign import ccall "glp_get_obj_dir" glp_get_obj_dir
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkDirection
  -- ^ The direction of the objective

foreign import ccall "glp_get_num_rows" glp_get_num_rows
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of constraints

foreign import ccall "glp_get_num_cols" glp_get_num_cols
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of variables

foreign import ccall "glp_get_row_name" glp_get_row_name
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The index of the constraint
  -> IO CString
  -- ^ The constraint name

foreign import ccall "glp_get_col_name" glp_get_col_name
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The index of the variable
  -> IO CString
  -- ^ The variable name

foreign import ccall "glp_get_row_type" glp_get_row_type
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The index of the constraint
  -> IO GlpkConstraintType
  -- ^ The constraint type

foreign import ccall "glp_get_row_lb" glp_get_row_lb
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The index of the constraint
  -> IO CDouble
  -- ^ The constraint lower bound

foreign import ccall "glp_get_row_ub" glp_get_row_ub
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The index of the constraint
  -> IO CDouble
  -- ^ The constraint upper bound

foreign import ccall "glp_get_col_type" glp_get_col_type
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The index of the variable
  -> IO GlpkVariableType
  -- ^ The constraint type

foreign import ccall "glp_get_col_lb" glp_get_col_lb
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The index of the variable
  -> IO CDouble
  -- ^ The variable lower bound

foreign import ccall "glp_get_col_ub" glp_get_col_ub
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The index of the variable
  -> IO CDouble
  -- ^ The variable upper bound

foreign import ccall "glp_get_obj_coef" glp_get_obj_coef
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The index of the variable
  -> IO CDouble
  -- ^ The objective coefficient

foreign import ccall "glp_get_num_nz" glp_get_num_nz
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of nonzero constraint coefficients

foreign import ccall "glp_get_mat_row" glp_get_mat_row
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to retrieve
  -> GlpkArray Column
  -- ^ The variable indices in the constraint
  -> GlpkArray CDouble
  -- ^ The variable coefficients in the constraint
  -> IO CInt
  -- ^ The length of the arrays

foreign import ccall "glp_get_mat_col" glp_get_mat_col
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The constraint to retrieve
  -> GlpkArray Row
  -- ^ The constraint indices the variable is in
  -> GlpkArray CDouble
  -- ^ The constraint coefficients for the variable
  -> IO CInt
  -- ^ The length of the arrays

foreign import ccall "glp_create_index" glp_create_index
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_delete_index" glp_delete_index
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_find_row" glp_find_row
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The name of the constraint
  -> IO Row
  -- ^ The index of the constraint

foreign import ccall "glp_find_col" glp_find_col
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The name of the variable
  -> IO Column
  -- ^ The index of the variable

foreign import ccall "glp_set_rii" glp_set_rii
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to scale
  -> CDouble
  -- ^ The scaling factor
  -> IO ()

foreign import ccall "glp_get_rii" glp_get_rii
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint index
  -> IO CDouble

foreign import ccall "glp_set_sjj" glp_set_sjj
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to scale
  -> CDouble
  -- ^ The scaling factor
  -> IO ()

foreign import ccall "glp_get_sjj" glp_get_sjj
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable index
  -> IO CDouble

foreign import ccall "glp_scale_prob" glp_scale_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkScaling
  -- ^ The type of scaling to apply
  -> IO ()

foreign import ccall "glp_unscale_prob" glp_unscale_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_set_row_stat" glp_set_row_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to modify
  -> GlpkVariableStatus
  -- ^ The status to apply
  -> IO ()

foreign import ccall "glp_set_col_stat" glp_set_col_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to modify
  -> GlpkVariableStatus
  -- ^ The status to apply
  -> IO ()

foreign import ccall "glp_std_basis" glp_std_basis
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_adv_basis" glp_adv_basis
  :: Ptr Problem
  -- ^ The problem instance
  -> Unused CInt
  -- ^ Reserved for future use, must be zero
  -> IO ()

foreign import ccall "glp_cpx_basis" glp_cpx_basis
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_simplex" glp_simplex
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr SimplexMethodControlParameters
  -- ^ Simplex control parameters
  -> IO GlpkSimplexStatus
  -- ^ The exit status

foreign import ccall "glp_exact" glp_exact
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr SimplexMethodControlParameters
  -- ^ Simplex control parameters
  -> IO GlpkSimplexStatus
  -- ^ The exit status

foreign import ccall "glp_init_smcp" glp_init_smcp
  :: Ptr SimplexMethodControlParameters
  -- ^ The Simplex control parameters to initialize
  -> IO ()

foreign import ccall "glp_get_status" glp_get_status
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus

foreign import ccall "glp_get_prim_stat" glp_get_prim_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus

foreign import ccall "glp_get_dual_stat" glp_get_dual_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus

foreign import ccall "glp_get_obj_val" glp_get_obj_val
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CDouble

foreign import ccall "glp_get_row_stat" glp_get_row_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to query
  -> IO GlpkVariableStatus
  -- ^ The status of the associated with the auxiliary variable

foreign import ccall "glp_get_col_stat" glp_get_col_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to query
  -> IO GlpkVariableStatus
  -- ^ The status of the variable

foreign import ccall "glp_get_row_prim" glp_get_row_prim
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to query
  -> IO CDouble
  -- ^ The primal auxiliary variable value

foreign import ccall "glp_get_row_dual" glp_get_row_dual
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to query
  -> IO CDouble
  -- ^ The dual auxiliary variable value

foreign import ccall "glp_get_col_prim" glp_get_col_prim
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to query
  -> IO CDouble
  -- ^ The primal variable value

foreign import ccall "glp_get_col_dual" glp_get_col_dual
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to query
  -> IO CDouble
  -- ^ The dual variable value

foreign import ccall "glp_get_unbnd_ray" glp_get_unbnd_ray
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The index, k, of the variable producing unboundedness. If 1 <=
  -- k <= m, then k is the kth auxiliary variable. If m + 1 <= k <= m
  -- + n, it is the (k - m)th structural variable.

foreign import ccall "glp_get_bfcp" glp_get_bfcp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr BasisFactorizationControlParameters
  -- ^ A pointer that will hold the basis factorization control
  -- parameters
  -> IO ()

foreign import ccall "glp_set_bfcp" glp_set_bfcp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr BasisFactorizationControlParameters
  -- ^ The basis factorization control parameters
  -> IO ()

foreign import ccall "glp_interior" glp_interior
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr InteriorPointControlParameters
  -- ^ The interior point control parameters
  -> IO GlpkInteriorPointStatus
  -- ^ The status of the solve

foreign import ccall "glp_init_iptcp" glp_init_iptcp
  :: Ptr InteriorPointControlParameters
  -- ^ The control parameters to initialize
  -> IO ()

foreign import ccall "glp_ipt_status" glp_ipt_status
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus
  -- ^ The status of the interior point solve

foreign import ccall "glp_intopt" glp_intopt
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr (MIPControlParameters a)
  -- ^ The MIP control parameters
  -> IO GlpkMIPStatus
  -- ^ The status of the solve

foreign import ccall "glp_mip_status" glp_mip_status
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus
  -- The status of the solution

foreign import ccall "glp_mip_obj_val" glp_mip_obj_val
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CDouble
  -- ^ The MIP object

foreign import ccall "glp_mip_row_val" glp_mip_row_val
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to query
  -> IO CDouble
  -- ^ The value of the auxiliary variable

foreign import ccall "glp_mip_col_val" glp_mip_col_val
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to query
  -> IO CDouble
  -- ^ The value of the variable

foreign import ccall "glp_check_kkt" glp_check_kkt
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkSolutionType
  -- ^ The solution type to check
  -> GlpkKKTCheck
  -- ^ The condition to be checked
  -> Ptr CDouble
  -- ^ The largest absolute error
  -> Ptr CInt
  -- ^ The row, column, or variable with the largest absolute error
  -> Ptr CDouble
  -- ^ The largest relative error
  -> Ptr CInt
  -- ^ The row, column, or variable with the largest relative error
  -> IO ()

foreign import ccall "glp_print_sol" glp_print_sol
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file name to write to
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_read_sol" glp_read_sol
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file name to read from
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_write_sol" glp_write_sol
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file name to write to
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_print_ranges" glp_print_ranges
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of rows and columns
  -> Ptr CInt
  -- ^ The rows and clumns to analyze
  -> Unused CInt
  -- ^ Reserved for future use, must be zero
  -> CString
  -- ^ The file name to write to
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_print_ipt" glp_print_ipt
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write to
  -> IO CInt
  -- Zero on success

foreign import ccall "glp_read_ipt" glp_read_ipt
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to read from
  -> IO CInt
  -- Zero on success

foreign import ccall "glp_write_ipt" glp_write_ipt
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write to
  -> IO CInt
  -- Zero on success

foreign import ccall "glp_print_mip" glp_print_mip
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write to
  -> IO CInt
  -- Zero on success

foreign import ccall "glp_read_mip" glp_read_mip
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to read from
  -> IO CInt
  -- Zero on success

foreign import ccall "glp_write_mip" glp_write_mip
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write to
  -> IO CInt
  -- Zero on success

foreign import ccall "glp_bf_exists" glp_bf_exists
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ Whether an LP basis factorization exists

foreign import ccall "glp_factorize" glp_factorize
  :: Ptr Problem
  -- ^ Compute an LP basis factorization
  -> IO ()

foreign import ccall "glp_bf_updated" glp_bf_updated
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ Whether the LP basis factorization is updated

foreign import ccall "glp_get_bhead" glp_get_bhead
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> IO CInt

foreign import ccall "glp_get_row_bind" glp_get_row_bind
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> IO CInt

foreign import ccall "glp_get_col_bind" glp_get_col_bind
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> IO CInt

foreign import ccall "glp_ftran" glp_ftran
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr CDouble
  -> IO ()

foreign import ccall "glp_btran" glp_btran
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr CDouble
  -> IO ()

foreign import ccall "glp_warm_up" glp_warm_up
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkFactorizationResult

foreign import ccall "glp_eval_tab_row" glp_eval_tab_row
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CInt
  -> Ptr CDouble
  -> IO CInt

foreign import ccall "glp_eval_tab_col" glp_eval_tab_col
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CInt
  -> Ptr CDouble
  -> IO CInt

foreign import ccall "glp_transform_row" glp_transform_row
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CInt
  -> Ptr CDouble
  -> IO CInt

foreign import ccall "glp_transform_col" glp_transform_col
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CInt
  -> Ptr CDouble
  -> IO CInt

foreign import ccall "glp_prim_rtest" glp_prim_rtest
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CInt
  -> Ptr CDouble
  -> CInt
  -> CDouble
  -> IO CInt

foreign import ccall "glp_dual_rtest" glp_dual_rtest
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CInt
  -> Ptr CDouble
  -> CInt
  -> CDouble
  -> IO CInt

foreign import ccall "glp_analyze_bound" glp_analyze_bound
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CDouble
  -> Ptr Column
  -> Ptr CDouble
  -> Ptr Column
  -> IO ()

foreign import ccall "glp_analyze_coef" glp_analyze_coef
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> Ptr CDouble
  -> Ptr Column
  -> Ptr CDouble
  -> Ptr CDouble
  -> Ptr Column
  -> Ptr CDouble
  -> IO ()

foreign import ccall "glp_init_iocp" glp_init_iocp
  :: Ptr (MIPControlParameters a)
  -- ^ The MIP control parameters to initialize
  -> IO ()

foreign import ccall "glp_ipt_obj_val" glp_ipt_obj_val
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CDouble
  -- ^ The objective value

foreign import ccall "glp_ipt_row_prim" glp_ipt_row_prim
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to query
  -> IO Double
  -- ^ The primal auxiliary variable value

foreign import ccall "glp_ipt_row_dual" glp_ipt_row_dual
  :: Ptr Problem
  -- ^ The problem instance
  -> Row
  -- ^ The constraint to query
  -> IO Double
  -- ^ The dual auxiliary variable value

foreign import ccall "glp_ipt_col_prim" glp_ipt_col_prim
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to query
  -> IO CDouble
  -- ^ The primal variable value

foreign import ccall "glp_ipt_col_dual" glp_ipt_col_dual
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable to query
  -> IO Double
  -- ^ The dual variable value

foreign import ccall "glp_ios_reason" glp_ios_reason
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO GlpkCallbackReason
  -- ^ The reason the callback is being called

foreign import ccall "glp_ios_get_prob" glp_ios_get_prob
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO (Ptr Problem)
  -- ^ The active problem

foreign import ccall "glp_ios_tree_size" glp_ios_tree_size
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> Ptr CInt
  -- ^ The number of active nodes
  -> Ptr CInt
  -- ^ The total number of active and inactive nodes
  -> Ptr CInt
  -- ^ The total number of nodes that have been added to the tree
  -> IO ()

foreign import ccall "glp_ios_curr_node" glp_ios_curr_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO (GlpkInt GlpkNodeIndex)
  -- ^ The current node in the search tree

foreign import ccall "glp_ios_next_node" glp_ios_next_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkInt GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO (GlpkInt GlpkNodeIndex)
  -- ^ The next node in the search tree after the target node

foreign import ccall "glp_ios_prev_node" glp_ios_prev_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkInt GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO (GlpkInt GlpkNodeIndex)
  -- ^ The parent node in the search tree after the target node

foreign import ccall "glp_ios_up_node" glp_ios_up_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkInt GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO (GlpkInt GlpkNodeIndex)
  -- ^ The parent of the target node

foreign import ccall "glp_ios_node_level" glp_ios_node_level
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkInt GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO CInt
  -- ^ The level of the target in the search tree; the root problem
  -- has level 0.

foreign import ccall "glp_ios_node_bound" glp_ios_node_bound
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkInt GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO CDouble
  -- ^ The objective bound on the target

foreign import ccall "glp_ios_best_node" glp_ios_best_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO (GlpkInt GlpkNodeIndex)
  -- ^ The node in the search tree with the best objective bound

foreign import ccall "glp_ios_mip_gap" glp_ios_mip_gap
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO CDouble
  -- ^ The current MIP gap

foreign import ccall "glp_ios_node_data" glp_ios_node_data
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkInt GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO (Ptr a)
  -- ^ The data associated with the target

foreign import ccall "glp_ios_row_attr" glp_ios_row_attr
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> CInt
  -- ^ The index of the target cut
  -> Ptr GlpkCutAttribute
  -- ^ The information about the target cut
  -> IO ()

foreign import ccall "glp_ios_pool_size" glp_ios_pool_size
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO CInt
  -- ^ The number of cutting planes added to the problem

foreign import ccall "glp_ios_add_row" glp_ios_add_row
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> CString
  -- ^ The name of the cutting plane to add
  -> GlpkUserCutType
  -- ^ The type of cut being added
  -> Unused CInt
  -- ^ Unused; must be zero
  -> CInt
  -- ^ The number of variable indices specified
  -> GlpkArray CInt
  -- ^ The variable indices
  -> GlpkArray CDouble
  -- ^ The variable coefficients
  -> GlpkConstraintType
  -- ^ The type of the constraint
  -> CDouble
  -- ^ The right-hand side of the constraint
  -> IO ()

foreign import ccall "glp_ios_del_row" glp_ios_del_row
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> CInt
  -- ^ The index of the cut to delete
  -> IO ()

foreign import ccall "glp_ios_clear_pool" glp_ios_clear_pool
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO ()

foreign import ccall "glp_ios_can_branch" glp_ios_can_branch
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> Column

foreign import ccall "glp_ios_branch_upon" glp_ios_branch_upon
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> Column
  -- ^ The index of the variable to branch on
  -> GlpkBranchOption
  -- ^ The branching decision
  -> IO ()

foreign import ccall "glp_ios_select_node" glp_ios_select_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkInt GlpkNodeIndex
  -- ^ The subproblem to explore
  -> IO ()

foreign import ccall "glp_ios_heur_sol" glp_ios_heur_sol
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkArray CDouble
  -- ^ The variable values of an integer heuristic
  -> IO ()

foreign import ccall "glp_ios_terminate" glp_ios_terminate
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO ()

foreign import ccall "glp_set_col_kind" glp_set_col_kind
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable index
  -> GlpkVariableType
  -- ^ The type of the variable
  -> IO ()

foreign import ccall "glp_get_col_kind" glp_get_col_kind
  :: Ptr Problem
  -- ^ The problem instance
  -> Column
  -- ^ The variable index
  -> IO GlpkVariableType
  -- ^ The type of the variable

foreign import ccall "glp_get_num_int" glp_get_num_int
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of integer variables

foreign import ccall "glp_get_num_bin" glp_get_num_bin
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of binary variables

foreign import ccall "glp_init_mpscp" glp_init_mpscp
  :: Ptr MPSControlParameters
  -- ^ The MPS control parameters to initialize
  -> IO ()

foreign import ccall "glp_read_mps" glp_read_mps
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkMPSFormat
  -- ^ The MPS format to read
  -> Ptr MPSControlParameters
  -- ^ The MPS control parameters
  -> CString
  -- ^ The name of the file to read
  -> IO ()

foreign import ccall "glp_write_mps" glp_write_mps
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkMPSFormat
  -- ^ The MPS format to read
  -> Ptr MPSControlParameters
  -- ^ The MPS control parameters
  -> CString
  -- ^ The name of the file to write to
  -> IO ()

foreign import ccall "glp_init_cpxcp" glp_init_cpxcp
  :: Ptr CplexLPFormatControlParameters
  -- ^ The CPLEX LP control parameters to initialize
  -> IO ()

foreign import ccall "glp_read_lp" glp_read_lp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr CplexLPFormatControlParameters
  -- ^ The CPLEX LP control parameters
  -> CString
  -- ^ The name of the file to read
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_write_lp" glp_write_lp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr CplexLPFormatControlParameters
  -- ^ The CPLEX LP control parameters
  -> CString
  -- ^ The name of the file to write to
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_read_prob" glp_read_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> Unused CInt
  -- ^ Reserved for future use, must be zero
  -> CString
  -- ^ The file to read from
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_write_prob" glp_write_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> Unused CInt
  -- ^ Reserved for future use, must be zero
  -> CString
  -- ^ The file to write to
  -> IO CInt
  -- ^ Zero on success

foreign import ccall "glp_mpl_alloc_wksp" glp_mpl_alloc_wksp
  :: IO (Ptr MathProgWorkspace)
  -- ^ The allocated MathProg workspace

foreign import ccall "glp_mpl_free_wksp" glp_mpl_free_wksp
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace to deallocate
  -> IO ()

foreign import ccall "glp_mpl_init_rand" glp_mpl_init_rand
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CInt
  -- ^ The random number generator seed
  -> IO MathProgResult

foreign import ccall "glp_mpl_read_model" glp_mpl_read_model
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CString
  -- ^ The name of the file to read
  -> CInt
  -- ^ If nonzero, skip the data section
  -> IO MathProgResult

foreign import ccall "glp_mpl_read_data" glp_mpl_read_data
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CString
  -- ^ The name of the file to read
  -> IO MathProgResult

foreign import ccall "glp_mpl_generate" glp_mpl_generate
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CString
  -- ^ The output file. If NULL, output is written to standard output
  -> IO MathProgResult

foreign import ccall "glp_mpl_build_prob" glp_mpl_build_prob
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> Ptr Problem
  -- ^ The problem instance to write to
  -> IO MathProgResult

foreign import ccall "glp_mpl_postsolve" glp_mpl_postsolve
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> Ptr Problem
  -- ^ The solved problem instance
  -> GlpkSolutionType
  -- ^ The type of solution to be copied
  -> IO MathProgResult

foreign import ccall "glp_read_cnfsat" glp_read_cnfstat
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to read from
  -> CInt
  -- ^ Zero on success

foreign import ccall "glp_write_cnfsat" glp_write_cnfstat
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The file to write to
  -> CInt
  -- ^ Zero on success

foreign import ccall "glp_minisat1" glp_minisat1
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt

foreign import ccall "glp_intfeas1" glp_intfeas1
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -> CInt
  -> IO CInt

foreign import ccall "glp_init_env" glp_init_env
  :: IO CInt

foreign import ccall "glp_free_env" glp_free_env
  :: IO CInt

foreign import ccall "glp_version" glp_version
  :: IO CString

foreign import ccall "glp_config" glp_config
  :: CString
  -> IO CString

foreign import ccall "glp_term_out" glp_term_out
  :: GlpkControl
  -> IO GlpkControl

foreign import ccall "glp_term_hook" glp_term_hook
  :: FunPtr (Ptr a -> CString -> IO CInt)
  -> Ptr a
  -> IO ()

foreign import ccall "glp_error_hook" glp_error_hook
  :: FunPtr (Ptr a -> IO CInt)
  -> Ptr a
  -> IO ()

foreign import ccall "wrapper"
  mkHaskellErrorHook :: (Ptr a -> IO CInt) -> IO (FunPtr (Ptr a -> IO CInt))

foreign import ccall "wrapper"
  mkHaskellTermHook :: (Ptr a -> IO CInt) -> IO (FunPtr (Ptr a -> IO CInt))

foreign import ccall "wrapper"
  mkHaskellMIPCallback :: (Ptr (GlpkTree a) -> Ptr a -> IO ()) -> IO (FunPtr (Ptr (GlpkTree a) -> Ptr a -> IO ()))
