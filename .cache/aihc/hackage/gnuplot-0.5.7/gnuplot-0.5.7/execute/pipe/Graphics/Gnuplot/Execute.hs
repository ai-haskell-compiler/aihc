module Graphics.Gnuplot.Execute where

import System.Exit (ExitCode, )
import System.IO (hPutStr, )
import qualified System.Process as Proc


simple ::
      [String] {-^ The lines of the gnuplot script to be piped into gnuplot -}
   -> [String] {-^ Options for gnuplot -}
   -> IO ExitCode
simple program options =
   do -- putStrLn cmd
      (inp,_out,_err,pid) <-
         Proc.runInteractiveProcess "gnuplot" options Nothing Nothing
      hPutStr inp (unlines program)
      Proc.waitForProcess pid
