module GHC.Platform.Constants where

import Prelude
import Data.Char

data PlatformConstants = PlatformConstants {
      pc_CONTROL_GROUP_CONST_291 :: {-# UNPACK #-} !Int,
      pc_STD_HDR_SIZE :: {-# UNPACK #-} !Int,
      pc_PROF_HDR_SIZE :: {-# UNPACK #-} !Int,
      pc_BLOCK_SIZE :: {-# UNPACK #-} !Int,
      pc_BLOCKS_PER_MBLOCK :: {-# UNPACK #-} !Int,
      pc_TICKY_BIN_COUNT :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR2 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR3 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR4 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR5 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR6 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR7 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR8 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR9 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rR10 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rF1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rF2 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rF3 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rF4 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rF5 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rF6 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rD1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rD2 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rD3 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rD4 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rD5 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rD6 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rXMM1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rXMM2 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rXMM3 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rXMM4 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rXMM5 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rXMM6 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rYMM1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rYMM2 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rYMM3 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rYMM4 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rYMM5 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rYMM6 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rZMM1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rZMM2 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rZMM3 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rZMM4 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rZMM5 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rZMM6 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rL1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rSp :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rSpLim :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rHp :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rHpLim :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rCCCS :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rCurrentTSO :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rCurrentNursery :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgRegTable_rHpAlloc :: {-# UNPACK #-} !Int,
      pc_OFFSET_stgEagerBlackholeInfo :: {-# UNPACK #-} !Int,
      pc_OFFSET_stgGCEnter1 :: {-# UNPACK #-} !Int,
      pc_OFFSET_stgGCFun :: {-# UNPACK #-} !Int,
      pc_OFFSET_Capability_r :: {-# UNPACK #-} !Int,
      pc_OFFSET_bdescr_start :: {-# UNPACK #-} !Int,
      pc_OFFSET_bdescr_free :: {-# UNPACK #-} !Int,
      pc_OFFSET_bdescr_blocks :: {-# UNPACK #-} !Int,
      pc_OFFSET_bdescr_flags :: {-# UNPACK #-} !Int,
      pc_SIZEOF_CostCentreStack :: {-# UNPACK #-} !Int,
      pc_OFFSET_CostCentreStack_mem_alloc :: {-# UNPACK #-} !Int,
      pc_REP_CostCentreStack_mem_alloc :: {-# UNPACK #-} !Int,
      pc_OFFSET_CostCentreStack_scc_count :: {-# UNPACK #-} !Int,
      pc_REP_CostCentreStack_scc_count :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgHeader_ccs :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgHeader_ldvw :: {-# UNPACK #-} !Int,
      pc_SIZEOF_StgSMPThunkHeader :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgEntCounter_allocs :: {-# UNPACK #-} !Int,
      pc_REP_StgEntCounter_allocs :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgEntCounter_allocd :: {-# UNPACK #-} !Int,
      pc_REP_StgEntCounter_allocd :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgEntCounter_registeredp :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgEntCounter_link :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgEntCounter_entry_count :: {-# UNPACK #-} !Int,
      pc_SIZEOF_StgUpdateFrame_NoHdr :: {-# UNPACK #-} !Int,
      pc_SIZEOF_StgOrigThunkInfoFrame_NoHdr :: {-# UNPACK #-} !Int,
      pc_SIZEOF_StgMutArrPtrs_NoHdr :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgMutArrPtrs_ptrs :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgMutArrPtrs_size :: {-# UNPACK #-} !Int,
      pc_SIZEOF_StgSmallMutArrPtrs_NoHdr :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgSmallMutArrPtrs_ptrs :: {-# UNPACK #-} !Int,
      pc_SIZEOF_StgArrBytes_NoHdr :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgArrBytes_bytes :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgTSO_alloc_limit :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgTSO_cccs :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgTSO_stackobj :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgStack_sp :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgStack_stack :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgUpdateFrame_updatee :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgOrigThunkInfoFrame_info_ptr :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgFunInfoExtraFwd_arity :: {-# UNPACK #-} !Int,
      pc_REP_StgFunInfoExtraFwd_arity :: {-# UNPACK #-} !Int,
      pc_SIZEOF_StgFunInfoExtraRev :: {-# UNPACK #-} !Int,
      pc_OFFSET_StgFunInfoExtraRev_arity :: {-# UNPACK #-} !Int,
      pc_REP_StgFunInfoExtraRev_arity :: {-# UNPACK #-} !Int,
      pc_MAX_SPEC_SELECTEE_SIZE :: {-# UNPACK #-} !Int,
      pc_MAX_SPEC_AP_SIZE :: {-# UNPACK #-} !Int,
      pc_MIN_PAYLOAD_SIZE :: {-# UNPACK #-} !Int,
      pc_MIN_INTLIKE :: {-# UNPACK #-} !Int,
      pc_MAX_INTLIKE :: {-# UNPACK #-} !Int,
      pc_MIN_CHARLIKE :: {-# UNPACK #-} !Int,
      pc_MAX_CHARLIKE :: {-# UNPACK #-} !Int,
      pc_MUT_ARR_PTRS_CARD_BITS :: {-# UNPACK #-} !Int,
      pc_MAX_Vanilla_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Float_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Double_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Long_REG :: {-# UNPACK #-} !Int,
      pc_MAX_XMM_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Real_Vanilla_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Real_Float_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Real_Double_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Real_XMM_REG :: {-# UNPACK #-} !Int,
      pc_MAX_Real_Long_REG :: {-# UNPACK #-} !Int,
      pc_RESERVED_C_STACK_BYTES :: {-# UNPACK #-} !Int,
      pc_RESERVED_STACK_WORDS :: {-# UNPACK #-} !Int,
      pc_AP_STACK_SPLIM :: {-# UNPACK #-} !Int,
      pc_WORD_SIZE :: {-# UNPACK #-} !Int,
      pc_CINT_SIZE :: {-# UNPACK #-} !Int,
      pc_CLONG_SIZE :: {-# UNPACK #-} !Int,
      pc_CLONG_LONG_SIZE :: {-# UNPACK #-} !Int,
      pc_BITMAP_BITS_SHIFT :: {-# UNPACK #-} !Int,
      pc_TAG_BITS :: {-# UNPACK #-} !Int,
      pc_LDV_SHIFT :: {-# UNPACK #-} !Int,
      pc_ILDV_CREATE_MASK :: !Integer,
      pc_ILDV_STATE_CREATE :: !Integer,
      pc_ILDV_STATE_USE :: !Integer,
      pc_USE_INLINE_SRT_FIELD :: !Bool
  } deriving (Show, Read, Eq, Ord)


parseConstantsHeader :: FilePath -> IO PlatformConstants
parseConstantsHeader fp = do
  s <- readFile fp
  let def = "#define HS_CONSTANTS \""
      find [] xs = xs
      find _  [] = error $ "GHC couldn't find the RTS constants ("++def++") in " ++ fp ++ ": the RTS package you are trying to use is perhaps for another GHC version" ++
                               "(e.g. you are using the wrong package database) or the package database is broken.\n"
      find (d:ds) (x:xs)
        | d == x    = find ds xs
        | otherwise = find def xs

      readVal' :: Bool -> Integer -> String -> [Integer]
      readVal' n     c (x:xs) = case x of
        '"' -> [if n then negate c else c]
        '-' -> readVal' True c xs
        ',' -> (if n then negate c else c) : readVal' False 0 xs
        _   -> readVal' n (c*10 + fromIntegral (ord x - ord '0')) xs
      readVal' n     c []     = [if n then negate c else c]

      readVal = readVal' False 0

  return $! case readVal (find def s) of
    [v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15
     ,v16,v17,v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30,v31
     ,v32,v33,v34,v35,v36,v37,v38,v39,v40,v41,v42,v43,v44,v45,v46,v47
     ,v48,v49,v50,v51,v52,v53,v54,v55,v56,v57,v58,v59,v60,v61,v62,v63
     ,v64,v65,v66,v67,v68,v69,v70,v71,v72,v73,v74,v75,v76,v77,v78,v79
     ,v80,v81,v82,v83,v84,v85,v86,v87,v88,v89,v90,v91,v92,v93,v94,v95
     ,v96,v97,v98,v99,v100,v101,v102,v103,v104,v105,v106,v107,v108,v109,v110,v111
     ,v112,v113,v114,v115,v116,v117,v118,v119,v120,v121,v122,v123,v124,v125,v126,v127
     ,v128,v129,v130
     ] -> PlatformConstants
            { pc_CONTROL_GROUP_CONST_291 = fromIntegral v0
            , pc_STD_HDR_SIZE = fromIntegral v1
            , pc_PROF_HDR_SIZE = fromIntegral v2
            , pc_BLOCK_SIZE = fromIntegral v3
            , pc_BLOCKS_PER_MBLOCK = fromIntegral v4
            , pc_TICKY_BIN_COUNT = fromIntegral v5
            , pc_OFFSET_StgRegTable_rR1 = fromIntegral v6
            , pc_OFFSET_StgRegTable_rR2 = fromIntegral v7
            , pc_OFFSET_StgRegTable_rR3 = fromIntegral v8
            , pc_OFFSET_StgRegTable_rR4 = fromIntegral v9
            , pc_OFFSET_StgRegTable_rR5 = fromIntegral v10
            , pc_OFFSET_StgRegTable_rR6 = fromIntegral v11
            , pc_OFFSET_StgRegTable_rR7 = fromIntegral v12
            , pc_OFFSET_StgRegTable_rR8 = fromIntegral v13
            , pc_OFFSET_StgRegTable_rR9 = fromIntegral v14
            , pc_OFFSET_StgRegTable_rR10 = fromIntegral v15
            , pc_OFFSET_StgRegTable_rF1 = fromIntegral v16
            , pc_OFFSET_StgRegTable_rF2 = fromIntegral v17
            , pc_OFFSET_StgRegTable_rF3 = fromIntegral v18
            , pc_OFFSET_StgRegTable_rF4 = fromIntegral v19
            , pc_OFFSET_StgRegTable_rF5 = fromIntegral v20
            , pc_OFFSET_StgRegTable_rF6 = fromIntegral v21
            , pc_OFFSET_StgRegTable_rD1 = fromIntegral v22
            , pc_OFFSET_StgRegTable_rD2 = fromIntegral v23
            , pc_OFFSET_StgRegTable_rD3 = fromIntegral v24
            , pc_OFFSET_StgRegTable_rD4 = fromIntegral v25
            , pc_OFFSET_StgRegTable_rD5 = fromIntegral v26
            , pc_OFFSET_StgRegTable_rD6 = fromIntegral v27
            , pc_OFFSET_StgRegTable_rXMM1 = fromIntegral v28
            , pc_OFFSET_StgRegTable_rXMM2 = fromIntegral v29
            , pc_OFFSET_StgRegTable_rXMM3 = fromIntegral v30
            , pc_OFFSET_StgRegTable_rXMM4 = fromIntegral v31
            , pc_OFFSET_StgRegTable_rXMM5 = fromIntegral v32
            , pc_OFFSET_StgRegTable_rXMM6 = fromIntegral v33
            , pc_OFFSET_StgRegTable_rYMM1 = fromIntegral v34
            , pc_OFFSET_StgRegTable_rYMM2 = fromIntegral v35
            , pc_OFFSET_StgRegTable_rYMM3 = fromIntegral v36
            , pc_OFFSET_StgRegTable_rYMM4 = fromIntegral v37
            , pc_OFFSET_StgRegTable_rYMM5 = fromIntegral v38
            , pc_OFFSET_StgRegTable_rYMM6 = fromIntegral v39
            , pc_OFFSET_StgRegTable_rZMM1 = fromIntegral v40
            , pc_OFFSET_StgRegTable_rZMM2 = fromIntegral v41
            , pc_OFFSET_StgRegTable_rZMM3 = fromIntegral v42
            , pc_OFFSET_StgRegTable_rZMM4 = fromIntegral v43
            , pc_OFFSET_StgRegTable_rZMM5 = fromIntegral v44
            , pc_OFFSET_StgRegTable_rZMM6 = fromIntegral v45
            , pc_OFFSET_StgRegTable_rL1 = fromIntegral v46
            , pc_OFFSET_StgRegTable_rSp = fromIntegral v47
            , pc_OFFSET_StgRegTable_rSpLim = fromIntegral v48
            , pc_OFFSET_StgRegTable_rHp = fromIntegral v49
            , pc_OFFSET_StgRegTable_rHpLim = fromIntegral v50
            , pc_OFFSET_StgRegTable_rCCCS = fromIntegral v51
            , pc_OFFSET_StgRegTable_rCurrentTSO = fromIntegral v52
            , pc_OFFSET_StgRegTable_rCurrentNursery = fromIntegral v53
            , pc_OFFSET_StgRegTable_rHpAlloc = fromIntegral v54
            , pc_OFFSET_stgEagerBlackholeInfo = fromIntegral v55
            , pc_OFFSET_stgGCEnter1 = fromIntegral v56
            , pc_OFFSET_stgGCFun = fromIntegral v57
            , pc_OFFSET_Capability_r = fromIntegral v58
            , pc_OFFSET_bdescr_start = fromIntegral v59
            , pc_OFFSET_bdescr_free = fromIntegral v60
            , pc_OFFSET_bdescr_blocks = fromIntegral v61
            , pc_OFFSET_bdescr_flags = fromIntegral v62
            , pc_SIZEOF_CostCentreStack = fromIntegral v63
            , pc_OFFSET_CostCentreStack_mem_alloc = fromIntegral v64
            , pc_REP_CostCentreStack_mem_alloc = fromIntegral v65
            , pc_OFFSET_CostCentreStack_scc_count = fromIntegral v66
            , pc_REP_CostCentreStack_scc_count = fromIntegral v67
            , pc_OFFSET_StgHeader_ccs = fromIntegral v68
            , pc_OFFSET_StgHeader_ldvw = fromIntegral v69
            , pc_SIZEOF_StgSMPThunkHeader = fromIntegral v70
            , pc_OFFSET_StgEntCounter_allocs = fromIntegral v71
            , pc_REP_StgEntCounter_allocs = fromIntegral v72
            , pc_OFFSET_StgEntCounter_allocd = fromIntegral v73
            , pc_REP_StgEntCounter_allocd = fromIntegral v74
            , pc_OFFSET_StgEntCounter_registeredp = fromIntegral v75
            , pc_OFFSET_StgEntCounter_link = fromIntegral v76
            , pc_OFFSET_StgEntCounter_entry_count = fromIntegral v77
            , pc_SIZEOF_StgUpdateFrame_NoHdr = fromIntegral v78
            , pc_SIZEOF_StgOrigThunkInfoFrame_NoHdr = fromIntegral v79
            , pc_SIZEOF_StgMutArrPtrs_NoHdr = fromIntegral v80
            , pc_OFFSET_StgMutArrPtrs_ptrs = fromIntegral v81
            , pc_OFFSET_StgMutArrPtrs_size = fromIntegral v82
            , pc_SIZEOF_StgSmallMutArrPtrs_NoHdr = fromIntegral v83
            , pc_OFFSET_StgSmallMutArrPtrs_ptrs = fromIntegral v84
            , pc_SIZEOF_StgArrBytes_NoHdr = fromIntegral v85
            , pc_OFFSET_StgArrBytes_bytes = fromIntegral v86
            , pc_OFFSET_StgTSO_alloc_limit = fromIntegral v87
            , pc_OFFSET_StgTSO_cccs = fromIntegral v88
            , pc_OFFSET_StgTSO_stackobj = fromIntegral v89
            , pc_OFFSET_StgStack_sp = fromIntegral v90
            , pc_OFFSET_StgStack_stack = fromIntegral v91
            , pc_OFFSET_StgUpdateFrame_updatee = fromIntegral v92
            , pc_OFFSET_StgOrigThunkInfoFrame_info_ptr = fromIntegral v93
            , pc_OFFSET_StgFunInfoExtraFwd_arity = fromIntegral v94
            , pc_REP_StgFunInfoExtraFwd_arity = fromIntegral v95
            , pc_SIZEOF_StgFunInfoExtraRev = fromIntegral v96
            , pc_OFFSET_StgFunInfoExtraRev_arity = fromIntegral v97
            , pc_REP_StgFunInfoExtraRev_arity = fromIntegral v98
            , pc_MAX_SPEC_SELECTEE_SIZE = fromIntegral v99
            , pc_MAX_SPEC_AP_SIZE = fromIntegral v100
            , pc_MIN_PAYLOAD_SIZE = fromIntegral v101
            , pc_MIN_INTLIKE = fromIntegral v102
            , pc_MAX_INTLIKE = fromIntegral v103
            , pc_MIN_CHARLIKE = fromIntegral v104
            , pc_MAX_CHARLIKE = fromIntegral v105
            , pc_MUT_ARR_PTRS_CARD_BITS = fromIntegral v106
            , pc_MAX_Vanilla_REG = fromIntegral v107
            , pc_MAX_Float_REG = fromIntegral v108
            , pc_MAX_Double_REG = fromIntegral v109
            , pc_MAX_Long_REG = fromIntegral v110
            , pc_MAX_XMM_REG = fromIntegral v111
            , pc_MAX_Real_Vanilla_REG = fromIntegral v112
            , pc_MAX_Real_Float_REG = fromIntegral v113
            , pc_MAX_Real_Double_REG = fromIntegral v114
            , pc_MAX_Real_XMM_REG = fromIntegral v115
            , pc_MAX_Real_Long_REG = fromIntegral v116
            , pc_RESERVED_C_STACK_BYTES = fromIntegral v117
            , pc_RESERVED_STACK_WORDS = fromIntegral v118
            , pc_AP_STACK_SPLIM = fromIntegral v119
            , pc_WORD_SIZE = fromIntegral v120
            , pc_CINT_SIZE = fromIntegral v121
            , pc_CLONG_SIZE = fromIntegral v122
            , pc_CLONG_LONG_SIZE = fromIntegral v123
            , pc_BITMAP_BITS_SHIFT = fromIntegral v124
            , pc_TAG_BITS = fromIntegral v125
            , pc_LDV_SHIFT = fromIntegral v126
            , pc_ILDV_CREATE_MASK = v127
            , pc_ILDV_STATE_CREATE = v128
            , pc_ILDV_STATE_USE = v129
            , pc_USE_INLINE_SRT_FIELD = 0 < v130
            }
    _ -> error "Invalid platform constants"

