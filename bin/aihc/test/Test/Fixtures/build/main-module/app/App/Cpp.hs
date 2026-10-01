{-# LANGUAGE CPP #-}
#if 1
module App.Cpp (main) where
#else
module Main (main) where
#endif

main :: IO ()
main = putStrLn "App.Cpp.main"
