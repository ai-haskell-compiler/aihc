module GHC.Conc
  ( module GHC.Conc.Sync,
    module GHC.Conc.IO,
    Signal,
    HandlerFun,
    setHandler,
    runHandlers,
  )
where

import GHC.Conc.IO
import GHC.Conc.Signal (HandlerFun, Signal, runHandlers, setHandler)
import GHC.Conc.Sync
