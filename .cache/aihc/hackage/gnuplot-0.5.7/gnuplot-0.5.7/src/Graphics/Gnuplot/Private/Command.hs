module Graphics.Gnuplot.Private.Command where

import qualified Graphics.Gnuplot.Private.File as File
import qualified Graphics.Gnuplot.Execute as Exec
import System.Exit (ExitCode(ExitSuccess), )
import Control.Concurrent (forkIO, )

import System.IO.Temp (withSystemTempDirectory, )

import Control.DeepSeq (deepseq, )
import Control.Functor.HT (void, )


run ::
   (File.C file) =>
   (FilePath -> ([String], [file])) -> IO ExitCode
run render =
   withSystemTempDirectory "gnuplot" $ \dir ->
   case render dir of
      (cmds, files) -> deepseq cmds $ do
         mapM_ File.write files
         Exec.simple (cmds ++ ["pause mouse close"]) []

asyncIfInteractive :: Bool -> IO ExitCode -> IO ExitCode
asyncIfInteractive interactive act =
   if interactive
     then fmap (const ExitSuccess) $ forkIO $ void act
     else act
