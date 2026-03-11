{-# LANGUAGE TemplateHaskell #-}

import System.Executable.Hash.Internal (injectedExecutableHash)

main :: IO ()
main =
    case $(injectedExecutableHash) of
        Nothing -> return ()
        Just x -> fail $ "Expected no injected executable hash. Instead got: " ++ show x
