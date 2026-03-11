-- | 
-- Copyright: 2015 Joey Hess <id@joeyh.name>
-- License: BSD-2-clause
-- 
-- The functions exported by this module are intended to be drop-in
-- replacements for those from System.Process, when converting a whole
-- program to use System.Console.Concurrent.

module System.Process.Concurrent where

import System.Console.Concurrent
import System.Process hiding (createProcess, waitForProcess)
import System.IO
import System.Exit

-- | Calls `createProcessConcurrent`
createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess = createProcessConcurrent

-- | Calls `waitForProcessConcurrent`
waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess = waitForProcessConcurrent
