{-# LANGUAGE ExistentialQuantification #-}

-- | The text encoding types. aihc's own handle layer sends all text
-- through UTF-8 and only carries the name of an encoding, but libraries
-- build and pattern match on the full GHC codec record, so the types
-- follow GHC.
module GHC.IO.Encoding.Types
  ( BufferCodec (..),
    TextEncoding (..),
    TextEncoder,
    TextDecoder,
    CodeBuffer,
    EncodeBuffer,
    DecodeBuffer,
    CodingProgress (..),
  )
where

import GHC.Base (String)
import GHC.IO (IO)
import GHC.IO.Buffer (Buffer, CharBufElem)
import GHC.Internal.Classes (Eq (..))
import GHC.Show (Show (..))
import GHC.Word (Word8)

-- | A codec translating a buffer of @from@ elements into a buffer of
-- @to@ elements, keeping @state@ between calls.
data BufferCodec from to state = BufferCodec
  { -- | Translate as much of the input buffer as fits in the output one.
    encode :: CodeBuffer from to,
    -- | Step over an input element the codec cannot translate.
    recover :: Buffer from -> Buffer to -> IO (Buffer from, Buffer to),
    -- | Release the resources the codec holds.
    close :: IO (),
    -- | Snapshot the codec state.
    getState :: IO state,
    -- | Restore a state taken by 'getState'.
    setState :: state -> IO ()
  }

type CodeBuffer from to = Buffer from -> Buffer to -> IO (CodingProgress, Buffer from, Buffer to)

type DecodeBuffer = CodeBuffer Word8 CharBufElem

type EncodeBuffer = CodeBuffer CharBufElem Word8

type TextDecoder state = BufferCodec Word8 CharBufElem state

type TextEncoder state = BufferCodec CharBufElem Word8 state

-- | An encoding is a name plus the two codecs that realise it.
data TextEncoding
  = forall dstate estate.
  TextEncoding
  { -- | The name the runtime knows the encoding by.
    textEncodingName :: String,
    -- | Build a decoder for this encoding.
    mkTextDecoder :: IO (TextDecoder dstate),
    -- | Build an encoder for this encoding.
    mkTextEncoder :: IO (TextEncoder estate)
  }

instance Show TextEncoding where
  show = textEncodingName

-- | Why a codec stopped.
data CodingProgress
  = -- | The input buffer ran out.
    InputUnderflow
  | -- | The output buffer ran out.
    OutputUnderflow
  | -- | The input holds a sequence the codec cannot translate.
    InvalidSequence
  deriving (Eq, Show)
