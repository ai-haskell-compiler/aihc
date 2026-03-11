module Graphics.Gnuplot.File (T(Cons, name, content), write) where

import qualified Graphics.Gnuplot.Private.File as File

data T =
   Cons {
      name :: FilePath,
      content :: String
   }
   deriving (Show, Eq)


write :: T -> IO ()
write (Cons fn cont) = writeFile fn cont

instance File.C T where
   write = write
