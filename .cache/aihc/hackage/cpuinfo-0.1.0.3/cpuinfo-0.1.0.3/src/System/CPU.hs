{-|
Module      : System.CPU
Description : Haskell Library for Checking CPU Information
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Linux >=2.6

This module provides information about the processors available on a system.
Modern hardware provides not only multiple physical processors and physical
cores, but logical cores which may not have dedicated execution resources.
Intel's Hyper-Threading is an example of such a technology, capable of providing
two logical cores for every physical core present on a supported physical
processor.

These additional logical cores increase the performance of some, but not all
workloads. Indeed, some parallel workloads may suffer a performance decrease if
all logical cores presented by the operating system do not have dedicated
physical resources. This is because technologies providing supernumerary logical
cores typically work by scheduling multiple threads in a shared pool of
execution resources, e.g. ALUs and FPUs. If threads sharing a pool of execution
resources are doing the same sort of work there will be scheduling contention
for a single type of execution resource on the physical core.

It is common for threaded Haskell programs to be run with @+RTS -N@, causing the
RTS to simply multiplex Haskell threads or sparks over the number of logical
cores available. However, if each logical core does not have dedicated physical
resources and the thread/spark workloads are similar, then this might be slower
than multiplexing over fewer cores.

This package allows a program to use information about the physical and logical
features of the available processors as a heuristic for selecting the number of
worker OS threads to use (e.g. via 'setNumCapabilities'). Some workloads may
benefit from, for example, using half the number of logical cores available if
there are in fact two logical cores for each physical core. This is typically
true of numerical workloads, but as always benchmarking should be employed to
evaluate the impact of different heuristics.

In its current state this module can only collect information from Linux systems
with a kernel from the 2.6 branch or later by reading @\/proc\/cpuinfo@. If this
module is unable to provide information on your system please file a bug
including your @\/proc\/cpuinfo@. Help providing Windows support would be
greatly appreciated!
-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           , OverloadedStrings
           #-}

module System.CPU (
    CPU(..)
    -- * Retrieving CPU Information
  , getCPUs
  , tryGetCPUs
    -- * Physical Features
  , physicalProcessors
  , physicalCores
  , logicalCores
  , hyperthreadingFactor
  , hyperthreadingInUse
  ) where

import Control.Applicative

import Control.Arrow

import Control.DeepSeq

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC

import Data.Data

import Data.Foldable

import Data.List

import Data.Maybe

import Data.Word

import GHC.Generics

-- | Representation of a logical processor and its features.
data CPU = CPU {
    -- | Logical Processor Index
    processorID     :: !Word32
    -- | CPU Vendor
  , vendor          :: !(Maybe B.ByteString)
    -- | CPU Model Number
  , model           :: !(Maybe Word32)
    -- | CPU Model Name
  , modelName       :: !(Maybe B.ByteString)
    -- | CPU Model Revision
  , revision        :: !(Maybe Word32)
    -- | CPU Microcode Revision
  , microcode       :: !(Maybe Word32)
    -- | Processor Frequency
  , freq            :: !Double
    -- | CPU Cache Size. (TODO figure out how to get the cache topology)
  , cache           :: !(Maybe Word32)
    -- | Physical Processor Index
  , physicalID      :: !Word32
    -- | Number of Physical Cores on this Physical Processor.
  , siblings        :: !Word32
    -- | Physical Core Index
  , coreID          :: !Word32
    -- | CPU APIC Index
  , apicID          :: !(Maybe Word32)
    -- | Whether or not the Physical Core provides a floating point unit.
  , fpu             :: !(Maybe Bool)
    -- | Whether or not the Physical Core provides a floating point exception
    --   unit.
  , fpuExcept       :: !(Maybe Bool)
    -- | Vendor-specific CPU flags.
  , flags           :: !(Maybe [B.ByteString])
    -- | MIPS approximation computed by the Linux kernel on boot.
  , bogoMIPS        :: !Double
    -- | Cache line size in bytes.
  , cacheAlignment  :: !(Maybe Word32)
    -- | Physical address width.
  , physicalAddress :: !(Maybe Word32)
    -- | Virtual address width.
  , virtualAddress  :: !(Maybe Word32)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Data
             , Typeable
             , Generic
             , NFData
             )

parsePair :: B.ByteString -> A.Parser a -> A.Parser a
parsePair k vp = A.string k
              *> A.skipSpace
              *> A.char ':'
              *> A.skipSpace
              *> vp

parseProcessor :: A.Parser Word32
parseProcessor = parsePair "processor" A.decimal

parseVendor :: A.Parser B.ByteString
parseVendor = parsePair "vendor_id" A.takeByteString

parseModel :: A.Parser Word32
parseModel = parsePair "model" A.decimal

parseModelName :: A.Parser B.ByteString
parseModelName = parsePair "model name" A.takeByteString

parseRevision :: A.Parser Word32
parseRevision = parsePair "stepping" A.decimal

parseMicrocode :: A.Parser Word32
parseMicrocode = parsePair "microcode" (A.string "0x" *> A.hexadecimal)

parseFreq :: A.Parser Double
parseFreq = parsePair "cpu MHz" A.double

parseCache :: A.Parser Word32
parseCache = parsePair "cache size"
                       (A.decimal <* (A.skipSpace *> A.string "KB"))

parsePhysicalID :: A.Parser Word32
parsePhysicalID = parsePair "physical id" A.decimal

parseSiblings :: A.Parser Word32
parseSiblings = parsePair "siblings" A.decimal

parseCoreID :: A.Parser Word32
parseCoreID = parsePair "core id" A.decimal

parseApicID :: A.Parser Word32
parseApicID = parsePair "apicid" A.decimal

parseFpu :: A.Parser Bool
parseFpu = parsePair "fpu" parseBool

parseFpuExcept :: A.Parser Bool
parseFpuExcept = parsePair "fpu_exception" parseBool

parseFlags :: A.Parser [B.ByteString]
parseFlags = parsePair "flags" parseWords

parseBogoMIPS :: A.Parser Double
parseBogoMIPS = parsePair "bogomips" A.double

parseCacheAlignment :: A.Parser Word32
parseCacheAlignment = parsePair "cache_alignment" A.decimal

parseAddresses :: A.Parser (Word32, Word32)
parseAddresses = parsePair "address sizes"
                           ((,) <$> parsePhysicalAddress
                                <*> parseVirtualAddress)

parsePhysicalAddress :: A.Parser Word32
parsePhysicalAddress = A.decimal <* A.string " bits physical, "

parseVirtualAddress :: A.Parser Word32
parseVirtualAddress = A.decimal <* A.string " bits virtual"

parseBool :: A.Parser Bool
parseBool = (A.string "yes" *> pure True)
        <|> (A.string "no" *> pure False)

parseWords :: A.Parser [B.ByteString]
parseWords = A.sepBy (A.takeWhile1 (/= ' ')) (A.char ' ')

parseMaybe :: A.Parser a -> B.ByteString -> Maybe a
parseMaybe = (check .) . A.parseOnly
    where check (Left _)  = Nothing
          check (Right x) = Just x

keepTrying :: [B.ByteString] -> A.Parser a -> Maybe a
keepTrying bs p = asum (map (parseMaybe p) bs)

splitCPULines :: B.ByteString -> [[B.ByteString]]
splitCPULines = splitCPUs . BC.lines
    where splitCPUs [] = []
          splitCPUs ls = let (cs, lss) = break (== "") ls
                         in cs : case lss of []      -> []
                                             (_:ls') -> splitCPUs ls'

tryCPU :: [B.ByteString] -> Maybe CPU
tryCPU bs = do
    proc     <- keepTrying bs parseProcessor
    vend     <- pure (keepTrying bs parseVendor)
    modl     <- pure (keepTrying bs parseModel)
    modn     <- pure (keepTrying bs parseModelName)
    rev      <- pure (keepTrying bs parseRevision)
    mcode    <- pure (keepTrying bs parseMicrocode)
    frq      <- keepTrying bs parseFreq
    cch      <- pure (keepTrying bs parseCache)
    pid      <- keepTrying bs parsePhysicalID
    sib      <- keepTrying bs parseSiblings
    cid      <- keepTrying bs parseCoreID
    aid      <- pure (keepTrying bs parseApicID)
    flpu     <- pure (keepTrying bs parseFpu)
    flpex    <- pure (keepTrying bs parseFpuExcept)
    flg      <- pure (keepTrying bs parseFlags)
    bgm      <- keepTrying bs parseBogoMIPS
    ca       <- pure (keepTrying bs parseCacheAlignment)
    (pa, va) <- pure $ case keepTrying bs parseAddresses
                of Nothing     -> (Nothing, Nothing)
                   Just (p, v) -> (Just p, Just v)
    pure $ CPU proc
               vend
               modl
               modn
               rev
               mcode
               frq
               cch
               pid
               sib
               cid
               aid
               flpu
               flpex
               flg
               bgm
               ca
               pa
               va

-- | Read @\/proc\/cpuinfo@ and try to parse the output. If this function
--   returns 'Nothing' on your system, please file a bug report with your
--   @\/proc\/cpuinfo@ contents and CPU specifications.
tryGetCPUs :: IO (Maybe [CPU])
tryGetCPUs = (mapM tryCPU . splitCPULines)
          <$> B.readFile "/proc/cpuinfo"

-- | Read @\/proc\/cpuinfo@ and try to parse the output. If this function throws
--   an error on your system, please file a bug report with your
--   @\/proc\/cpuinfo@ contents and CPU specifications.
getCPUs :: IO [CPU]
getCPUs = fromMaybe (error e) <$> tryGetCPUs
    where e = unlines [ "Couldn't parse your /proc/cpuinfo contents."
                      , "Please file a bug including your /proc/cpuinfo here:"
                      , "https://github.com/traviswhitaker/cpuinfo/issues"
                      ]

-- | Counts the number of physical processors in the system. A physical
--   processor corresponds to a single CPU unit in a single socket, i.e. unless
--   you have a multi-socket motherboard, this number will be one.
physicalProcessors :: [CPU] -> Int
physicalProcessors = length . nub . map physicalID

-- | Counts the number of physical cores in the system. A physical core is an
--   independent processing unit that reads and executes instructions on its
--   own, but potentially shares its die (and other resources) with other cores.
physicalCores :: [CPU] -> Int
physicalCores = length . nub . map (physicalID &&& coreID)

-- | Counts the number of logical cores in the system. A logical core is a
--   virtual processing unit exposed to the operating system, that may or may
--   not directly correspond with an independent physical processing unit, e.g.
--   a hyperthread appears as an independent processing unit to the operating
--   system, but has no physically dedicated execution resources.
logicalCores :: [CPU] -> Int
logicalCores = length

-- | The hyperthreading factor is the number of logical cores divided by the
--   number of physical cores. This quantity indicates the degree to which
--   physical execution resources are shared among logical processors, and may
--   be used to tune parallel applications.
hyperthreadingFactor :: [CPU] -> Rational
hyperthreadingFactor cpus = fromIntegral (logicalCores cpus)
                          / fromIntegral (physicalCores cpus)

-- | If hyperthreading is in use, the 'hyperthreadingFactor' will be greater
--   than 1.
hyperthreadingInUse :: [CPU] -> Bool
hyperthreadingInUse = (/= 1) . hyperthreadingFactor
