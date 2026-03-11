module Main where

import SomeDictOf

main :: IO ()
main = putStrLn "it compiles at least"

-- note: if this changes, update the README
showValues
    :: [SomeDictOf Identity Show]
    -> [String]
showValues =
    map (\(SomeDictOf (Identity showable)) -> show showable)

slamMempty :: SomeDictOf Proxy Monoid -> SomeDictOf Identity Monoid
slamMempty = mapSomeDictOf (\pa -> Identity mempty)
