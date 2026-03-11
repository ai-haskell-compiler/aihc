import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, postBuild)
import Distribution.Simple.LocalBuildInfo (buildDir)
import System.Executable.Hash.Internal (maybeInjectExecutableHash)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postBuild = \_ _ _ buildInfo ->
        maybeInjectExecutableHash (buildDir buildInfo </> "test-inject/test-inject")
    }
