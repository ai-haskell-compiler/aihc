-- | Requests to the garbage collector.
--
-- A request has no observable result in GHC either. The aihc runtime
-- collects only when the heap needs space, so each request does nothing.
module System.Mem
  ( performGC,
    performMajorGC,
    performMinorGC,
    performBlockingMajorGC,
  )
where

import Prelude

-- | Request a major collection. The runtime ignores the request.
performGC :: IO ()
performGC = return ()

-- | Request a major collection. The runtime ignores the request.
performMajorGC :: IO ()
performMajorGC = return ()

-- | Request a minor collection. The runtime ignores the request.
performMinorGC :: IO ()
performMinorGC = return ()

-- | Request a blocking major collection. The runtime ignores the request.
performBlockingMajorGC :: IO ()
performBlockingMajorGC = return ()
