{-# LANGUAGE TemplateHaskell #-}

import System.Executable.Hash.Internal (injectedExecutableHash)

main :: IO ()
main =
    case $(injectedExecutableHash) of
        Nothing -> fail "Expected executable hash."
        Just _ -> return ()
