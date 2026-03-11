{- |
This module implements something
that is very similar to our asynchronous exception approach.
However, it does not expose a memory leak. Why?
-}
module Main where


unzipPattern :: [(a,b)] -> ([a], [b])
unzipPattern =
   foldr
      (\(a,b) ~(as,bs) -> (a:as, b:bs))
      ([], [])

unzipSelector :: [(a,b)] -> ([a], [b])
unzipSelector =
   foldr
      (\(a,b) asbs -> (a : fst asbs, b : snd asbs))
      ([], [])


noSpaceLeak :: IO ()
noSpaceLeak =
   let xs = repeat ('a', 42::Int)
       (ys,zs) = unzipSelector xs
   in  do mapM_ putChar ys
          print $ last zs


{-
ee-unzip +RTS -M1m -c30 -RTS
-}
main :: IO ()
main = noSpaceLeak
