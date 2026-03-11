{-# LANGUAGE BangPatterns #-}

{-|
Module      : Data.Snowflake
Description : Unique id generator. Port of Twitter Snowflake.
License     : Apache 2.0
Maintainer  : edofic@gmail.com
Stability   : experimental

This generates unique(guaranteed) identifiers build from time stamp,
counter(inside same millisecond) and node id - if you wish to generate 
ids across several nodes. Identifiers are convertible to `Integer` 
values which are monotonically increasing with respect to time.
-}
module Data.Snowflake 
( SnowflakeConfig(..)
, Snowflake
, SnowflakeGen
, newSnowflakeGen
, nextSnowflake
, defaultConfig
, snowflakeToInteger
) where

import Data.Bits  ((.|.), (.&.), shift, Bits)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (when)
import Control.Concurrent (threadDelay)

{-|
Configuration that specifies how much bits are used for each part of the id.
There are no limits to total bit sum.
-}
data SnowflakeConfig = SnowflakeConfig { confTimeBits  :: {-# UNPACK #-} !Int
                                       , confCountBits :: {-# UNPACK #-} !Int
                                       , confNodeBits  :: {-# UNPACK #-} !Int
                                       } deriving (Eq, Show)

-- |Default configuration using 40 bits for time, 16 for count and 8 for node id.
defaultConfig :: SnowflakeConfig
defaultConfig = SnowflakeConfig 40 16 8

-- |Generator which contains needed state. You should use `newSnowflakeGen` to create instances.
newtype SnowflakeGen = SnowflakeGen { genLastSnowflake :: MVar Snowflake } 


-- |Generated identifier. Can be converted to `Integer`.
data Snowflake = Snowflake { snowflakeTime  :: !Integer
                           , snowflakeCount :: !Integer
                           , snowflakeNode  :: !Integer
                           , snowflakeConf  :: !SnowflakeConfig
                           } deriving (Eq)

-- |Converts an identifier to an integer with respect to configuration used to generate it.
snowflakeToInteger :: Snowflake -> Integer
snowflakeToInteger (Snowflake time count node config) = let
  SnowflakeConfig _ countBits nodeBits = config
  in 
    (time `shift` (countBits + nodeBits))
    .|.
    (count `shift` nodeBits)
    .|.
    node

instance Show Snowflake where
  show = show . snowflakeToInteger

cutBits :: (Num a, Bits a) => a -> Int -> a
cutBits n bits = n .&. ((1 `shift` bits) - 1)

currentTimestamp :: IO Integer
currentTimestamp = (round . (*1000)) `fmap` getPOSIXTime 

currentTimestampFixed :: Int -> IO Integer
currentTimestampFixed n = fmap (`cutBits` n) currentTimestamp

-- |Create a new generator. Takes a configuration and node id.
newSnowflakeGen ::  SnowflakeConfig -> Integer -> IO SnowflakeGen
newSnowflakeGen conf@(SnowflakeConfig timeBits _ nodeBits) nodeIdRaw = do
  timestamp <- currentTimestampFixed timeBits
  let nodeId    = nodeIdRaw `cutBits` nodeBits 
      initial    = Snowflake timestamp 0 nodeId conf
  mvar <- newMVar initial
  return $ SnowflakeGen mvar

-- |Generates next id. The bread and butter. See module description for details.
nextSnowflake :: SnowflakeGen -> IO Snowflake
nextSnowflake (SnowflakeGen lastRef) = do
  Snowflake lastTime lastCount node conf <- takeMVar lastRef
  let SnowflakeConfig timeBits countBits _ = conf
      getNextTime = do
        time <- currentTimestampFixed timeBits
        if (lastTime > time) then do
          threadDelay $ fromInteger $ (lastTime - time) * 1000
          getNextTime
        else 
          return time
      loop = do
        timestamp <- getNextTime
        let count = if timestamp == lastTime then lastCount + 1 else 0
        if ((count `shift` (-1 * countBits)) /= 0) then 
          loop
        else 
          return $ Snowflake timestamp count node conf
  new <- loop          
  putMVar lastRef new
  return new
  
