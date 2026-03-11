-- | 
-- Copyright: 2015 Joey Hess <id@joeyh.name>
-- License: BSD-2-clause
-- 
-- Concurrent output handling.
--
-- > import Control.Concurrent.Async
-- > import System.Console.Concurrent
-- >
-- > main = withConcurrentOutput $ do
-- > 	outputConcurrent "washed the car\n"
-- > 		`concurrently`
-- >	outputConcurrent "walked the dog\n"
-- >		`concurrently`
-- > 	createProcessConcurrent (proc "ls" [])

module System.Console.Concurrent (
	-- * Concurrent output
	withConcurrentOutput,
	Outputable(..),
	outputConcurrent,
	errorConcurrent,
	createProcessConcurrent,
	createProcessForeground,
	flushConcurrentOutput,
	lockOutput,
	ConcurrentProcessHandle,
	waitForProcessConcurrent,
	-- * Low level access to the output buffer
	OutputBuffer,
	StdHandle(..),
	bufferOutputSTM,
	outputBufferWaiterSTM,
	waitAnyBuffer,
	waitCompleteLines,
	emitOutputBuffer,
) where

import System.Console.Concurrent.Internal

