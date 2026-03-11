import System.Executable.Hash

main :: IO ()
main = do
    mhash <- computeExecutableHash
    case mhash of
        Nothing ->
            fail "Failed to compute executable hash. This should only happen when running in GHCI."
        Just _ -> return ()
