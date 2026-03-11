--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Marshal.Sequencer
-- Copyright : (c) Henning Thielemann, 2010
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- PRIVATE MODULE.
--
-- Here we have the various types used by the library,
-- and how they are imported\/exported to C.
--------------------------------------------------------------------------------

module Sound.ALSA.Sequencer.Marshal.Sequencer where

#include <alsa/asoundlib.h>
import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )


-- | Read\/Write permissions for the sequencer device.
-- This way we prevent the ALSA exception 22 "Invalid argument"
-- when calling @event_output@ on an input-only sequencer.
class OpenMode mode where expOpenMode :: mode -> C.CInt

class OpenMode mode => AllowInput  mode where
class OpenMode mode => AllowOutput mode where

data OutputMode = OutputMode deriving (Show)
data InputMode  = InputMode  deriving (Show)
data DuplexMode = DuplexMode deriving (Show)

instance OpenMode OutputMode where expOpenMode _ = #{const SND_SEQ_OPEN_OUTPUT}
instance OpenMode InputMode  where expOpenMode _ = #{const SND_SEQ_OPEN_INPUT}
instance OpenMode DuplexMode where expOpenMode _ = #{const SND_SEQ_OPEN_DUPLEX}

instance AllowOutput OutputMode where
instance AllowOutput DuplexMode where
instance AllowInput  InputMode  where
instance AllowInput  DuplexMode where


-- | Blocking behavior of the sequencer device.
data BlockMode      = Block     -- ^ Operations may block.
                    | Nonblock  -- ^ Throw exceptions instead of blocking.
                      deriving (Show,Eq)

expBlockMode      :: BlockMode -> C.CInt
expBlockMode x     = case x of
  Block     -> 0
  Nonblock  -> #{const SND_SEQ_NONBLOCK}


-- | The type of sequencer handles.
newtype T mode = Cons (Ptr Core) deriving Eq
data Core
