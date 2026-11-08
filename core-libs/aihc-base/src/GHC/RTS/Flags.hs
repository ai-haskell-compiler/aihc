-- | A subset of the GHC RTS flag API.
--
-- The aihc runtime does not use the GHC RTS flags.
-- Thus, the functions in this module return the GHC default values.
module GHC.RTS.Flags
  ( RtsTime,
    ConcFlags (..),
    getConcFlags,
  )
where

import Data.Word (Word64)
import Prelude (IO, Int, Show, return)

-- | Time in nanoseconds.
type RtsTime = Word64

-- | Flags that control the scheduler.
data ConcFlags = ConcFlags
  { ctxtSwitchTime :: RtsTime,
    ctxtSwitchTicks :: Int
  }
  deriving (Show)

-- | Return the GHC default scheduler flags (a 20 ms context switch time).
getConcFlags :: IO ConcFlags
getConcFlags =
  return
    ConcFlags
      { ctxtSwitchTime = 20000000,
        ctxtSwitchTicks = 2
      }
