{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Exts.Stack.Constants where

import GHC.Exts.Heap.Unsupported
import Prelude

newtype ByteOffset = ByteOffset {offsetInBytes :: Int}
  deriving newtype (Eq, Show, Integral, Real, Num, Enum, Ord)

newtype WordOffset = WordOffset {offsetInWords :: Int}
  deriving newtype (Eq, Show, Integral, Real, Num, Enum, Ord)

-- The offsets and the sizes below describe the layout of the stack frames of
-- GHC. AIHC stack frames have another layout, so each value raises an
-- exception when a program uses it.

offsetStgCatchFrameHandler :: WordOffset
offsetStgCatchFrameHandler = unsupported "offsetStgCatchFrameHandler"

sizeStgCatchFrame :: Int
sizeStgCatchFrame = unsupported "sizeStgCatchFrame"

offsetStgCatchSTMFrameCode :: WordOffset
offsetStgCatchSTMFrameCode = unsupported "offsetStgCatchSTMFrameCode"

offsetStgCatchSTMFrameHandler :: WordOffset
offsetStgCatchSTMFrameHandler = unsupported "offsetStgCatchSTMFrameHandler"

sizeStgCatchSTMFrame :: Int
sizeStgCatchSTMFrame = unsupported "sizeStgCatchSTMFrame"

offsetStgUpdateFrameUpdatee :: WordOffset
offsetStgUpdateFrameUpdatee = unsupported "offsetStgUpdateFrameUpdatee"

sizeStgUpdateFrame :: Int
sizeStgUpdateFrame = unsupported "sizeStgUpdateFrame"

offsetStgAtomicallyFrameCode :: WordOffset
offsetStgAtomicallyFrameCode = unsupported "offsetStgAtomicallyFrameCode"

offsetStgAtomicallyFrameResult :: WordOffset
offsetStgAtomicallyFrameResult = unsupported "offsetStgAtomicallyFrameResult"

sizeStgAtomicallyFrame :: Int
sizeStgAtomicallyFrame = unsupported "sizeStgAtomicallyFrame"

offsetStgCatchRetryFrameRunningAltCode :: WordOffset
offsetStgCatchRetryFrameRunningAltCode = unsupported "offsetStgCatchRetryFrameRunningAltCode"

offsetStgCatchRetryFrameRunningFirstCode :: WordOffset
offsetStgCatchRetryFrameRunningFirstCode = unsupported "offsetStgCatchRetryFrameRunningFirstCode"

offsetStgCatchRetryFrameAltCode :: WordOffset
offsetStgCatchRetryFrameAltCode = unsupported "offsetStgCatchRetryFrameAltCode"

sizeStgCatchRetryFrame :: Int
sizeStgCatchRetryFrame = unsupported "sizeStgCatchRetryFrame"

offsetStgRetFunFrameSize :: WordOffset
offsetStgRetFunFrameSize = unsupported "offsetStgRetFunFrameSize"

offsetStgRetFunFrameFun :: WordOffset
offsetStgRetFunFrameFun = unsupported "offsetStgRetFunFrameFun"

offsetStgRetFunFramePayload :: WordOffset
offsetStgRetFunFramePayload = unsupported "offsetStgRetFunFramePayload"

sizeStgRetFunFrame :: Int
sizeStgRetFunFrame = unsupported "sizeStgRetFunFrame"

offsetStgBCOFrameInstrs :: ByteOffset
offsetStgBCOFrameInstrs = unsupported "offsetStgBCOFrameInstrs"

offsetStgBCOFrameLiterals :: ByteOffset
offsetStgBCOFrameLiterals = unsupported "offsetStgBCOFrameLiterals"

offsetStgBCOFramePtrs :: ByteOffset
offsetStgBCOFramePtrs = unsupported "offsetStgBCOFramePtrs"

offsetStgBCOFrameArity :: ByteOffset
offsetStgBCOFrameArity = unsupported "offsetStgBCOFrameArity"

offsetStgBCOFrameSize :: ByteOffset
offsetStgBCOFrameSize = unsupported "offsetStgBCOFrameSize"

offsetStgClosurePayload :: WordOffset
offsetStgClosurePayload = unsupported "offsetStgClosurePayload"

sizeStgClosure :: Int
sizeStgClosure = unsupported "sizeStgClosure"

byteOffsetToWordOffset :: ByteOffset -> WordOffset
byteOffsetToWordOffset = WordOffset . bytesToWords . fromInteger . toInteger

bytesToWords :: Int -> Int
bytesToWords b =
  if b `mod` bytesInWord == 0
    then fromIntegral $ b `div` bytesInWord
    else error "Unexpected struct alignment!"

bytesInWord :: Int
bytesInWord = 8
