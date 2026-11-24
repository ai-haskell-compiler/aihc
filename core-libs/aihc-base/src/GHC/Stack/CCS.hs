-- | Access to the cost-centre stacks of the profiler.
--
-- The aihc runtime has no cost-centre profiling. Thus, these functions act
-- as in GHC without profiling: the current cost-centre stack is the null
-- pointer, and each list of cost-centre names is empty.
module GHC.Stack.CCS
  ( currentCallStack,
    whoCreated,
    CostCentreStack,
    CostCentre,
    getCurrentCCS,
    getCCSOf,
    clearCCS,
    ccsCC,
    ccsParent,
    ccLabel,
    ccModule,
    ccSrcSpan,
    ccsToStrings,
    renderStack,
  )
where

import Foreign.C.String (CString)
import GHC.Ptr (Ptr, nullPtr)
import Prelude (IO, String, concatMap, ioError, return, reverse, userError, (++))

-- | A cost-centre stack of the profiler.
data CostCentreStack

-- | A cost centre of the profiler.
data CostCentre

-- | Get the cost-centre stack of the current thread.
--
-- The result is always empty.
currentCallStack :: IO [String]
currentCallStack = return []

-- | Get the cost-centre stack that was current when the value was made.
--
-- The result is always empty.
whoCreated :: a -> IO [String]
whoCreated _ = return []

-- | Get the current cost-centre stack.
--
-- The result is always the null pointer.
getCurrentCCS :: dummy -> IO (Ptr CostCentreStack)
getCurrentCCS _ = return nullPtr

-- | Get the cost-centre stack that was current when the value was made.
--
-- The result is always the null pointer.
getCCSOf :: a -> IO (Ptr CostCentreStack)
getCCSOf _ = return nullPtr

-- | Run an action with an empty cost-centre stack.
clearCCS :: IO a -> IO a
clearCCS action = action

-- | Get the top cost centre of a cost-centre stack.
ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC _ = noProfiling "ccsCC"

-- | Get the parent of a cost-centre stack.
ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent _ = noProfiling "ccsParent"

-- | Get the label of a cost centre.
ccLabel :: Ptr CostCentre -> IO CString
ccLabel _ = noProfiling "ccLabel"

-- | Get the module of a cost centre.
ccModule :: Ptr CostCentre -> IO CString
ccModule _ = noProfiling "ccModule"

-- | Get the source span of a cost centre.
ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan _ = noProfiling "ccSrcSpan"

-- | Get the names of the cost centres in a cost-centre stack.
--
-- The result is always empty, because each stack is the null pointer.
ccsToStrings :: Ptr CostCentreStack -> IO [String]
ccsToStrings _ = return []

-- | Format a list of cost-centre names as a stack trace.
renderStack :: [String] -> String
renderStack names = "CallStack (from -prof):" ++ concatMap ("\n  " ++) (reverse names)

-- | There is no valid cost-centre stack or cost centre to read, because
-- the runtime has no profiling.
noProfiling :: String -> IO a
noProfiling name = ioError (userError (name ++ ": the aihc runtime has no cost-centre profiling"))
