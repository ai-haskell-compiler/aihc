{-# LANGUAGE DeriveGeneric #-}

-- | The GHC RTS flag API.
--
-- The aihc runtime does not use the GHC RTS flags. Thus, each function in
-- this module gives the values that the non-threaded GHC 9.12 runtime gives
-- when the program has no RTS options. The types are the same as in GHC, so
-- that code that inspects them compiles.
module GHC.RTS.Flags
  ( RtsTime,
    RTSFlags (..),
    GiveGCStats (..),
    GCFlags (..),
    ConcFlags (..),
    MiscFlags (..),
    IoManagerFlag (..),
    DebugFlags (..),
    DoCostCentres (..),
    CCFlags (..),
    DoHeapProfile (..),
    ProfFlags (..),
    DoTrace (..),
    TraceFlags (..),
    TickyFlags (..),
    ParFlags (..),
    HpcFlags (..),
    IoSubSystem (..),
    getRTSFlags,
    getGCFlags,
    getConcFlags,
    getMiscFlags,
    getDebugFlags,
    getCCFlags,
    getProfFlags,
    getTraceFlags,
    getTickyFlags,
    getParFlags,
    getHpcFlags,
  )
where

import Data.Word (Word32, Word64)
import GHC.Err (errorWithoutStackTrace)
import GHC.Generics (Generic)
import GHC.IO.SubSystem (IoSubSystem (..))
import Prelude

-- | Time in nanoseconds.
type RtsTime = Word64

-- | Whether the runtime collects and reports garbage collection statistics.
data GiveGCStats
  = NoGCStats
  | CollectGCStats
  | OneLineGCStats
  | SummaryGCStats
  | VerboseGCStats
  deriving (Show, Generic)

-- | The numbers are the C constants of the GHC runtime.
instance Enum GiveGCStats where
  fromEnum NoGCStats = 0
  fromEnum CollectGCStats = 1
  fromEnum OneLineGCStats = 2
  fromEnum SummaryGCStats = 3
  fromEnum VerboseGCStats = 4

  toEnum 0 = NoGCStats
  toEnum 1 = CollectGCStats
  toEnum 2 = OneLineGCStats
  toEnum 3 = SummaryGCStats
  toEnum 4 = VerboseGCStats
  toEnum e = errorWithoutStackTrace ("invalid enum for GiveGCStats: " ++ show e)

-- | Flags that control the garbage collector.
data GCFlags = GCFlags
  { statsFile :: Maybe FilePath,
    giveStats :: GiveGCStats,
    maxStkSize :: Word32,
    initialStkSize :: Word32,
    stkChunkSize :: Word32,
    stkChunkBufferSize :: Word32,
    maxHeapSize :: Word32,
    minAllocAreaSize :: Word32,
    largeAllocLim :: Word32,
    nurseryChunkSize :: Word32,
    minOldGenSize :: Word32,
    heapSizeSuggestion :: Word32,
    heapSizeSuggestionAuto :: Bool,
    oldGenFactor :: Double,
    returnDecayFactor :: Double,
    pcFreeHeap :: Double,
    generations :: Word32,
    squeezeUpdFrames :: Bool,
    compact :: Bool,
    compactThreshold :: Double,
    sweep :: Bool,
    ringBell :: Bool,
    idleGCDelayTime :: RtsTime,
    doIdleGC :: Bool,
    heapBase :: Word,
    allocLimitGrace :: Word,
    numa :: Bool,
    numaMask :: Word
  }
  deriving (Show, Generic)

-- | Flags that control the scheduler.
data ConcFlags = ConcFlags
  { ctxtSwitchTime :: RtsTime,
    ctxtSwitchTicks :: Int
  }
  deriving (Show, Generic)

-- | Miscellaneous runtime flags.
data MiscFlags = MiscFlags
  { tickInterval :: RtsTime,
    installSignalHandlers :: Bool,
    installSEHHandlers :: Bool,
    generateCrashDumpFile :: Bool,
    generateStackTrace :: Bool,
    machineReadable :: Bool,
    disableDelayedOsMemoryReturn :: Bool,
    internalCounters :: Bool,
    linkerAlwaysPic :: Bool,
    linkerMemBase :: Word,
    ioManager :: IoManagerFlag,
    numIoWorkerThreads :: Word32
  }
  deriving (Show, Generic)

-- | The I/O manager that the runtime uses.
data IoManagerFlag
  = IoManagerFlagAuto
  | IoManagerFlagSelect
  | IoManagerFlagMIO
  | IoManagerFlagWinIO
  | IoManagerFlagWin32Legacy
  deriving (Eq, Enum, Show)

-- | Flags that enable debug output of the runtime.
data DebugFlags = DebugFlags
  { scheduler :: Bool,
    interpreter :: Bool,
    weak :: Bool,
    gccafs :: Bool,
    gc :: Bool,
    nonmoving_gc :: Bool,
    block_alloc :: Bool,
    sanity :: Bool,
    stable :: Bool,
    prof :: Bool,
    linker :: Bool,
    apply :: Bool,
    stm :: Bool,
    squeeze :: Bool,
    hpc :: Bool,
    sparks :: Bool
  }
  deriving (Show, Generic)

-- | The cost-centre report that the profiler writes.
data DoCostCentres
  = CostCentresNone
  | CostCentresSummary
  | CostCentresVerbose
  | CostCentresAll
  | CostCentresJSON
  deriving (Show, Generic)

-- | The numbers are the C constants of the GHC runtime.
instance Enum DoCostCentres where
  fromEnum CostCentresNone = 0
  fromEnum CostCentresSummary = 1
  fromEnum CostCentresVerbose = 2
  fromEnum CostCentresAll = 3
  fromEnum CostCentresJSON = 4

  toEnum 0 = CostCentresNone
  toEnum 1 = CostCentresSummary
  toEnum 2 = CostCentresVerbose
  toEnum 3 = CostCentresAll
  toEnum 4 = CostCentresJSON
  toEnum e = errorWithoutStackTrace ("invalid enum for DoCostCentres: " ++ show e)

-- | Flags that control cost-centre profiling.
data CCFlags = CCFlags
  { doCostCentres :: DoCostCentres,
    profilerTicks :: Int,
    msecsPerTick :: Int
  }
  deriving (Show, Generic)

-- | The heap profile that the runtime writes.
data DoHeapProfile
  = NoHeapProfiling
  | HeapByCCS
  | HeapByMod
  | HeapByDescr
  | HeapByType
  | HeapByRetainer
  | HeapByLDV
  | HeapByClosureType
  | HeapByInfoTable
  | HeapByEra
  deriving (Show, Generic)

-- | The numbers are the C constants of the GHC runtime. The runtime does not
-- use the number 3.
instance Enum DoHeapProfile where
  fromEnum NoHeapProfiling = 0
  fromEnum HeapByCCS = 1
  fromEnum HeapByMod = 2
  fromEnum HeapByDescr = 4
  fromEnum HeapByType = 5
  fromEnum HeapByRetainer = 6
  fromEnum HeapByLDV = 7
  fromEnum HeapByClosureType = 8
  fromEnum HeapByInfoTable = 9
  fromEnum HeapByEra = 10

  toEnum 0 = NoHeapProfiling
  toEnum 1 = HeapByCCS
  toEnum 2 = HeapByMod
  toEnum 4 = HeapByDescr
  toEnum 5 = HeapByType
  toEnum 6 = HeapByRetainer
  toEnum 7 = HeapByLDV
  toEnum 8 = HeapByClosureType
  toEnum 9 = HeapByInfoTable
  toEnum 10 = HeapByEra
  toEnum e = errorWithoutStackTrace ("invalid enum for DoHeapProfile: " ++ show e)

-- | Flags that control heap profiling.
data ProfFlags = ProfFlags
  { doHeapProfile :: DoHeapProfile,
    heapProfileInterval :: RtsTime,
    heapProfileIntervalTicks :: Word,
    startHeapProfileAtStartup :: Bool,
    startTimeProfileAtStartup :: Bool,
    showCCSOnException :: Bool,
    automaticEraIncrement :: Bool,
    maxRetainerSetSize :: Word,
    ccsLength :: Word,
    modSelector :: Maybe String,
    descrSelector :: Maybe String,
    typeSelector :: Maybe String,
    ccSelector :: Maybe String,
    ccsSelector :: Maybe String,
    retainerSelector :: Maybe String,
    bioSelector :: Maybe String,
    eraSelector :: Word
  }
  deriving (Show, Generic)

-- | The destination of trace events.
data DoTrace
  = TraceNone
  | TraceEventLog
  | TraceStderr
  deriving (Show, Generic)

-- | The numbers are the C constants of the GHC runtime.
instance Enum DoTrace where
  fromEnum TraceNone = 0
  fromEnum TraceEventLog = 1
  fromEnum TraceStderr = 2

  toEnum 0 = TraceNone
  toEnum 1 = TraceEventLog
  toEnum 2 = TraceStderr
  toEnum e = errorWithoutStackTrace ("invalid enum for DoTrace: " ++ show e)

-- | Flags that control tracing.
data TraceFlags = TraceFlags
  { tracing :: DoTrace,
    timestamp :: Bool,
    traceScheduler :: Bool,
    traceGc :: Bool,
    traceNonmovingGc :: Bool,
    sparksSampled :: Bool,
    sparksFull :: Bool,
    user :: Bool
  }
  deriving (Show, Generic)

-- | Flags that control ticky-ticky profiling.
data TickyFlags = TickyFlags
  { showTickyStats :: Bool,
    tickyFile :: Maybe FilePath
  }
  deriving (Show, Generic)

-- | Flags that control parallel execution.
data ParFlags = ParFlags
  { nCapabilities :: Word32,
    migrate :: Bool,
    maxLocalSparks :: Word32,
    parGcEnabled :: Bool,
    parGcGen :: Word32,
    parGcLoadBalancingEnabled :: Bool,
    parGcLoadBalancingGen :: Word32,
    parGcNoSyncWithIdle :: Word32,
    parGcThreads :: Word32,
    setAffinity :: Bool
  }
  deriving (Show, Generic)

-- | Flags that control program coverage.
data HpcFlags = HpcFlags
  { readTixFile :: Bool,
    writeTixFile :: Bool
  }
  deriving (Show, Generic)

-- | All the runtime flags.
data RTSFlags = RTSFlags
  { gcFlags :: GCFlags,
    concurrentFlags :: ConcFlags,
    miscFlags :: MiscFlags,
    debugFlags :: DebugFlags,
    costCentreFlags :: CCFlags,
    profilingFlags :: ProfFlags,
    traceFlags :: TraceFlags,
    tickyFlags :: TickyFlags,
    parFlags :: ParFlags,
    hpcFlags :: HpcFlags
  }
  deriving (Show, Generic)

-- | Give all the runtime flags.
getRTSFlags :: IO RTSFlags
getRTSFlags =
  return
    RTSFlags
      { gcFlags = defaultGCFlags,
        concurrentFlags = defaultConcFlags,
        miscFlags = defaultMiscFlags,
        debugFlags = defaultDebugFlags,
        costCentreFlags = defaultCCFlags,
        profilingFlags = defaultProfFlags,
        traceFlags = defaultTraceFlags,
        tickyFlags = defaultTickyFlags,
        parFlags = defaultParFlags,
        hpcFlags = defaultHpcFlags
      }

-- | Give the garbage collector flags.
getGCFlags :: IO GCFlags
getGCFlags = return defaultGCFlags

-- | Give the scheduler flags (a 20 ms context switch time).
getConcFlags :: IO ConcFlags
getConcFlags = return defaultConcFlags

-- | Give the miscellaneous flags.
getMiscFlags :: IO MiscFlags
getMiscFlags = return defaultMiscFlags

-- | Give the debug flags.
getDebugFlags :: IO DebugFlags
getDebugFlags = return defaultDebugFlags

-- | Give the cost-centre profiling flags.
getCCFlags :: IO CCFlags
getCCFlags = return defaultCCFlags

-- | Give the heap profiling flags.
getProfFlags :: IO ProfFlags
getProfFlags = return defaultProfFlags

-- | Give the trace flags.
getTraceFlags :: IO TraceFlags
getTraceFlags = return defaultTraceFlags

-- | Give the ticky-ticky profiling flags.
getTickyFlags :: IO TickyFlags
getTickyFlags = return defaultTickyFlags

-- | Give the parallel execution flags.
getParFlags :: IO ParFlags
getParFlags = return defaultParFlags

-- | Give the program coverage flags.
getHpcFlags :: IO HpcFlags
getHpcFlags = return defaultHpcFlags

defaultGCFlags :: GCFlags
defaultGCFlags =
  GCFlags
    { statsFile = Nothing,
      giveStats = NoGCStats,
      maxStkSize = 4294967295,
      initialStkSize = 128,
      stkChunkSize = 4096,
      stkChunkBufferSize = 128,
      maxHeapSize = 0,
      minAllocAreaSize = 1024,
      largeAllocLim = 0,
      nurseryChunkSize = 0,
      minOldGenSize = 256,
      heapSizeSuggestion = 0,
      heapSizeSuggestionAuto = False,
      oldGenFactor = 2,
      returnDecayFactor = 4,
      pcFreeHeap = 3,
      generations = 2,
      squeezeUpdFrames = True,
      compact = False,
      compactThreshold = 30,
      sweep = False,
      ringBell = False,
      idleGCDelayTime = 300000000,
      doIdleGC = False,
      heapBase = 0,
      allocLimitGrace = 25,
      numa = False,
      numaMask = 1
    }

defaultConcFlags :: ConcFlags
defaultConcFlags =
  ConcFlags
    { ctxtSwitchTime = 20000000,
      ctxtSwitchTicks = 2
    }

defaultMiscFlags :: MiscFlags
defaultMiscFlags =
  MiscFlags
    { tickInterval = 10000000,
      installSignalHandlers = True,
      installSEHHandlers = True,
      generateCrashDumpFile = False,
      generateStackTrace = True,
      machineReadable = False,
      disableDelayedOsMemoryReturn = False,
      internalCounters = False,
      linkerAlwaysPic = True,
      linkerMemBase = 0,
      ioManager = IoManagerFlagAuto,
      numIoWorkerThreads = 1
    }

defaultDebugFlags :: DebugFlags
defaultDebugFlags =
  DebugFlags
    { scheduler = False,
      interpreter = False,
      weak = False,
      gccafs = False,
      gc = False,
      nonmoving_gc = False,
      block_alloc = False,
      sanity = False,
      stable = False,
      prof = False,
      linker = False,
      apply = False,
      stm = False,
      squeeze = False,
      hpc = False,
      sparks = False
    }

-- | GHC reads uninitialized memory for 'msecsPerTick' when the runtime has
-- no profiler. Thus this value is zero.
defaultCCFlags :: CCFlags
defaultCCFlags =
  CCFlags
    { doCostCentres = CostCentresNone,
      profilerTicks = 0,
      msecsPerTick = 0
    }

-- | GHC reads uninitialized memory for 'heapProfileIntervalTicks' when the
-- runtime has no profiler. Thus this value is zero.
defaultProfFlags :: ProfFlags
defaultProfFlags =
  ProfFlags
    { doHeapProfile = NoHeapProfiling,
      heapProfileInterval = 100000000,
      heapProfileIntervalTicks = 0,
      startHeapProfileAtStartup = True,
      startTimeProfileAtStartup = True,
      showCCSOnException = False,
      automaticEraIncrement = False,
      maxRetainerSetSize = 0,
      ccsLength = 0,
      modSelector = Nothing,
      descrSelector = Nothing,
      typeSelector = Nothing,
      ccSelector = Nothing,
      ccsSelector = Nothing,
      retainerSelector = Nothing,
      bioSelector = Nothing,
      eraSelector = 0
    }

defaultTraceFlags :: TraceFlags
defaultTraceFlags =
  TraceFlags
    { tracing = TraceNone,
      timestamp = False,
      traceScheduler = False,
      traceGc = False,
      traceNonmovingGc = False,
      sparksSampled = False,
      sparksFull = False,
      user = False
    }

defaultTickyFlags :: TickyFlags
defaultTickyFlags =
  TickyFlags
    { showTickyStats = False,
      tickyFile = Nothing
    }

defaultParFlags :: ParFlags
defaultParFlags =
  ParFlags
    { nCapabilities = 0,
      migrate = False,
      maxLocalSparks = 0,
      parGcEnabled = False,
      parGcGen = 0,
      parGcLoadBalancingEnabled = False,
      parGcLoadBalancingGen = 0,
      parGcNoSyncWithIdle = 0,
      parGcThreads = 0,
      setAffinity = False
    }

defaultHpcFlags :: HpcFlags
defaultHpcFlags =
  HpcFlags
    { readTixFile = True,
      writeTixFile = True
    }
