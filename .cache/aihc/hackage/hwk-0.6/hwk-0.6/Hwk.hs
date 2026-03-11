module Hwk where

-- | modules to be imported in hint interpreter
-- These modules must be from installed packages
userModules :: [String]
userModules = [ "Prelude", "Data.Bool", "Data.Char", "Data.List"
              , "Data.List.Extra", "Data.Tuple.Extra", "Data.Version.Extra"
              , "System.FilePath"
              ]

int :: String -> Int
int str = read str :: Int

ints :: [String] -> [Int]
ints = map int
