{-# LANGUAGE StrictData #-}

-- | This module exports the input classification type to avoid import
-- cycles between other modules that need this.
module Graphics.Vty.Platform.Windows.Input.Classify.Types
  ( KClass(..)
  , ClassifierState(..)
  )
where

import Data.ByteString.Char8 (ByteString)
import Graphics.Vty.Input.Events ( Event )

-- | Whether the classifier is currently processing a chunked format.
-- Currently, only bracketed pastes use this.
data ClassifierState
    = ClassifierStart
    -- ^ Not processing a chunked format.
    | ClassifierInChunk ByteString [ByteString]
    -- ^ Currently processing a chunked format. The initial chunk is in the
    -- first argument and a reversed remainder of the chunks is collected in
    -- the second argument. At the end of the processing, the chunks are
    -- reversed and concatenated with the final chunk.

-- | Description of parsed input sequences, including state for valid key,
-- mouse, and window events plus invalid and partial events.
data KClass
    = Valid Event ByteString
    -- ^ A valid event was parsed. Any unused characters from the input
    -- stream are also provided.
    | Invalid
    -- ^ The input characters did not represent a valid event.
    | Prefix
    -- ^ The input characters form the prefix of a valid event character
    -- sequence.
    | Chunk
    -- ^ The input characters are either start of a bracketed paste chunk
    -- or in the middle of a bracketed paste chunk.
    deriving(Show, Eq)
