{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Codec.Compression.LZ4.CTypes
Description : C type definitions for the lz4 compression codec.
Copyright   : (c) Niklas Hambüchen, 2020
License     : MIT
Maintainer  : mail@nh2.me
Stability   : stable

-}


module Codec.Compression.LZ4.CTypes
  ( Lz4FrameException(..)
  , BlockSizeID(..)
  , BlockMode(..)
  , ContentChecksum(..)
  , BlockChecksum(..)
  , FrameType(..)
  , FrameInfo(..)
  , Preferences(..)
  , LZ4F_cctx
  , LZ4F_dctx
  , lz4FrameTypesTable
  ) where

import           Control.Exception (Exception, throwIO)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Typeable (Typeable)
import           Data.Word (Word32, Word64)
import           Foreign.Marshal.Utils (fillBytes)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable (Storable(..), poke)
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

#include "lz4frame.h"


data Lz4FrameException = Lz4FormatException String
  deriving (Eq, Ord, Show, Typeable)

instance Exception Lz4FrameException


data BlockSizeID
  = LZ4F_default
  | LZ4F_max64KB
  | LZ4F_max256KB
  | LZ4F_max1MB
  | LZ4F_max4MB
  deriving (Eq, Ord, Show)

instance Storable BlockSizeID where
  sizeOf _ = #{size LZ4F_blockSizeID_t}
  alignment _ = #{alignment LZ4F_blockSizeID_t}
  peek p = do
    n <- peek (castPtr p :: Ptr #{type LZ4F_blockSizeID_t})
    case n of
      #{const LZ4F_default} -> return LZ4F_default
      #{const LZ4F_max64KB} -> return LZ4F_max64KB
      #{const LZ4F_max256KB} -> return LZ4F_max256KB
      #{const LZ4F_max1MB} -> return LZ4F_max1MB
      #{const LZ4F_max4MB} -> return LZ4F_max4MB
      _ -> throwIO $ Lz4FormatException $ "lz4 instance Storable BlockSizeID: encountered unknown LZ4F_blockSizeID_t: " ++ show n
  poke p i = poke (castPtr p :: Ptr #{type LZ4F_blockSizeID_t}) $ case i of
    LZ4F_default -> #{const LZ4F_default}
    LZ4F_max64KB -> #{const LZ4F_max64KB}
    LZ4F_max256KB -> #{const LZ4F_max256KB}
    LZ4F_max1MB -> #{const LZ4F_max1MB}
    LZ4F_max4MB -> #{const LZ4F_max4MB}


data BlockMode
  = LZ4F_blockLinked
  | LZ4F_blockIndependent
  deriving (Eq, Ord, Show)

instance Storable BlockMode where
  sizeOf _ = #{size LZ4F_blockMode_t}
  alignment _ = #{alignment LZ4F_blockMode_t}
  peek p = do
    n <- peek (castPtr p :: Ptr #{type LZ4F_blockMode_t})
    case n of
      #{const LZ4F_blockLinked } -> return LZ4F_blockLinked
      #{const LZ4F_blockIndependent } -> return LZ4F_blockIndependent
      _ -> throwIO $ Lz4FormatException $ "lz4 instance Storable BlockMode: encountered unknown LZ4F_blockMode_t: " ++ show n
  poke p mode = poke (castPtr p :: Ptr #{type LZ4F_blockMode_t}) $ case mode of
    LZ4F_blockLinked -> #{const LZ4F_blockLinked}
    LZ4F_blockIndependent -> #{const LZ4F_blockIndependent}


data ContentChecksum
  = LZ4F_noContentChecksum
  | LZ4F_contentChecksumEnabled
  deriving (Eq, Ord, Show)

instance Storable ContentChecksum where
  sizeOf _ = #{size LZ4F_contentChecksum_t}
  alignment _ = #{alignment LZ4F_contentChecksum_t}
  peek p = do
    n <- peek (castPtr p :: Ptr #{type LZ4F_contentChecksum_t})
    case n of
      #{const LZ4F_noContentChecksum } -> return LZ4F_noContentChecksum
      #{const LZ4F_contentChecksumEnabled } -> return LZ4F_contentChecksumEnabled
      _ -> throwIO $ Lz4FormatException $ "lz4 instance Storable ContentChecksum: encountered unknown LZ4F_contentChecksum_t: " ++ show n
  poke p mode = poke (castPtr p :: Ptr #{type LZ4F_contentChecksum_t}) $ case mode of
    LZ4F_noContentChecksum -> #{const LZ4F_noContentChecksum}
    LZ4F_contentChecksumEnabled -> #{const LZ4F_contentChecksumEnabled}


data BlockChecksum
  = LZ4F_noBlockChecksum
  | LZ4F_blockChecksumEnabled
  deriving (Eq, Ord, Show)

instance Storable BlockChecksum where
  sizeOf _ = #{size LZ4F_blockChecksum_t}
  alignment _ = #{alignment LZ4F_blockChecksum_t}
  peek p = do
    n <- peek (castPtr p :: Ptr #{type LZ4F_blockChecksum_t})
    case n of
      #{const LZ4F_noBlockChecksum } -> return LZ4F_noBlockChecksum
      #{const LZ4F_blockChecksumEnabled } -> return LZ4F_blockChecksumEnabled
      _ -> throwIO $ Lz4FormatException $ "lz4 instance Storable BlockChecksum: encountered unknown LZ4F_blockChecksum_t: " ++ show n
  poke p mode = poke (castPtr p :: Ptr #{type LZ4F_blockChecksum_t}) $ case mode of
    LZ4F_noBlockChecksum  -> #{const LZ4F_noBlockChecksum}
    LZ4F_blockChecksumEnabled -> #{const LZ4F_blockChecksumEnabled}


data FrameType
  = LZ4F_frame
  | LZ4F_skippableFrame
  deriving (Eq, Ord, Show)

instance Storable FrameType where
  sizeOf _ = #{size LZ4F_frameType_t}
  alignment _ = #{alignment LZ4F_frameType_t}
  peek p = do
    n <- peek (castPtr p :: Ptr #{type LZ4F_frameType_t})
    case n of
      #{const LZ4F_frame } -> return LZ4F_frame
      #{const LZ4F_skippableFrame } -> return LZ4F_skippableFrame
      _ -> throwIO $ Lz4FormatException $ "lz4 instance Storable FrameType: encountered unknown LZ4F_frameType_t: " ++ show n
  poke p mode = poke (castPtr p :: Ptr #{type LZ4F_frameType_t}) $ case mode of
    LZ4F_frame  -> #{const LZ4F_frame}
    LZ4F_skippableFrame -> #{const LZ4F_skippableFrame}


data FrameInfo = FrameInfo
  { blockSizeID         :: BlockSizeID
  , blockMode           :: BlockMode
  , contentChecksumFlag :: ContentChecksum
  , frameType           :: FrameType
  , contentSize         :: Word64
  , dictID              :: Word32 -- ^ @unsigned int@ in @lz4frame.h@, which can be 16 or 32 bits; AFAIK GHC does not run on archs where it is 16-bit, so there's a compile-time check for it.
  , blockChecksumFlag   :: BlockChecksum
  }

-- See comment on `dictID`.
$(if #{size unsigned} /= (4 :: Int)
    then error "sizeof(unsigned) is not 4 (32-bits), the code is not written for this"
    else pure []
 )

instance Storable FrameInfo where
  sizeOf _ = #{size LZ4F_frameInfo_t}
  alignment _ = #{alignment LZ4F_frameInfo_t}
  peek p = do
    blockSizeID <- #{peek LZ4F_frameInfo_t, blockSizeID} p
    blockMode <- #{peek LZ4F_frameInfo_t, blockMode} p
    contentChecksumFlag <- #{peek LZ4F_frameInfo_t, contentChecksumFlag} p
    frameType <- #{peek LZ4F_frameInfo_t, frameType} p
    contentSize <- #{peek LZ4F_frameInfo_t, contentSize} p
    dictID <- #{peek LZ4F_frameInfo_t, dictID} p
    blockChecksumFlag <- #{peek LZ4F_frameInfo_t, blockChecksumFlag} p
    return $ FrameInfo
      { blockSizeID
      , blockMode
      , contentChecksumFlag
      , frameType
      , contentSize
      , dictID
      , blockChecksumFlag
      }
  poke p frameInfo = do
    #{poke LZ4F_frameInfo_t, blockSizeID} p $ blockSizeID frameInfo
    #{poke LZ4F_frameInfo_t, blockMode} p $ blockMode frameInfo
    #{poke LZ4F_frameInfo_t, contentChecksumFlag} p $ contentChecksumFlag frameInfo
    #{poke LZ4F_frameInfo_t, frameType} p $ frameType frameInfo
    #{poke LZ4F_frameInfo_t, contentSize} p $ contentSize frameInfo
    -- These were reserved fields once; versions of `lz4frame.h` older
    -- than v1.8.0 will not have them.
    #{poke LZ4F_frameInfo_t, dictID} p $ dictID frameInfo
    #{poke LZ4F_frameInfo_t, blockChecksumFlag} p $ blockChecksumFlag frameInfo


data Preferences = Preferences
  { frameInfo        :: FrameInfo
  , compressionLevel :: Int
  , autoFlush        :: Bool
  , favorDecSpeed    :: Bool
  }

instance Storable Preferences where
  sizeOf _ = #{size LZ4F_preferences_t}
  alignment _ = #{alignment LZ4F_preferences_t}
  peek p = do
    frameInfo <- #{peek LZ4F_preferences_t, frameInfo} p
    compressionLevel <- #{peek LZ4F_preferences_t, compressionLevel} p
    autoFlush <- #{peek LZ4F_preferences_t, autoFlush} p
    favorDecSpeed <- #{peek LZ4F_preferences_t, favorDecSpeed} p
    return $ Preferences
      { frameInfo
      , compressionLevel
      , autoFlush
      , favorDecSpeed
      }
  poke p preferences = do
    fillBytes p 0 #{size LZ4F_preferences_t} -- set reserved fields to 0 as lz4frame.h requires
    #{poke LZ4F_preferences_t, frameInfo} p $ frameInfo preferences
    #{poke LZ4F_preferences_t, compressionLevel} p $ compressionLevel preferences
    #{poke LZ4F_preferences_t, autoFlush} p $ autoFlush preferences
    #{poke LZ4F_preferences_t, favorDecSpeed} p $ favorDecSpeed preferences -- since lz4 v1.8.2
    -- reserved uint field here, see lz4frame.h
    -- reserved uint field here, see lz4frame.h
    -- reserved uint field here, see lz4frame.h


data LZ4F_cctx
data LZ4F_dctx


lz4FrameTypesTable :: Map C.TypeSpecifier TH.TypeQ
lz4FrameTypesTable = Map.fromList
  [ (C.TypeName "LZ4F_cctx", [t| LZ4F_cctx |])
  , (C.TypeName "LZ4F_dctx", [t| LZ4F_dctx |])
  , (C.TypeName "LZ4F_blockSizeID_t", [t| BlockSizeID |])
  , (C.TypeName "LZ4F_blockMode_t", [t| BlockMode |])
  , (C.TypeName "LZ4F_contentChecksum_t", [t| ContentChecksum |])
  , (C.TypeName "LZ4F_blockChecksum_t", [t| BlockChecksum |])
  , (C.TypeName "LZ4F_frameInfo_t", [t| FrameInfo |])
  , (C.TypeName "LZ4F_frameType_t", [t| FrameType |])
  , (C.TypeName "LZ4F_preferences_t", [t| Preferences |])
  ]
