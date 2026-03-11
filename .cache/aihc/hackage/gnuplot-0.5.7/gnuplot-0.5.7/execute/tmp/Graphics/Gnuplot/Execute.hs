module Graphics.Gnuplot.Execute where

import qualified System.IO as IO
import System.IO.Temp (withSystemTempFile, )
import System.Exit (ExitCode, )
import System.Process (readProcessWithExitCode, )


tmpScript :: FilePath
tmpScript = "curve.gp"

simple ::
      [String] {-^ The lines of the gnuplot script to be piped into gnuplot -}
   -> [String] {-^ Options for gnuplot -}
   -> IO ExitCode
simple program options =
   withSystemTempFile tmpScript $ \path handle -> do
      IO.hPutStr handle (unlines program)
      IO.hClose handle
      -- putStrLn $ showCommandForUser "gnuplot" (options ++ [path])
      (exitCode, _out, _err) <-
         readProcessWithExitCode "gnuplot" (options ++ [path]) []
      -- putStr out
      -- putStr err
      return exitCode
