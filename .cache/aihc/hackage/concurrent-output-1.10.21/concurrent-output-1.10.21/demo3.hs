import Control.Concurrent.Async
import System.Console.Concurrent
import System.Process

main = withConcurrentOutput $
	outputConcurrent "hello world\n"
		`concurrently`
	createProcessConcurrent (proc "ls" [])
		`concurrently`
	createProcessConcurrent (proc "who" [])
		`concurrently`
	createProcessForeground (proc "vim" [])
		`concurrently`
	outputConcurrent "hello world again\n"
