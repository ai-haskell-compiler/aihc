module Graphics.Gnuplot.Private.File where

class C file where
   write :: file -> IO ()
