import Control.Exception (bracket_)
import System.Directory
import System.FilePath ((</>))
import System.Info (os)
import System.IO (IOMode(..), withFile)
import System.Posix.User
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import System.Directory.Glob
import System.Directory.Glob.GNU
import System.Directory.Glob.GNU.Compat         -- Needed on OSX


main :: IO ()
main = do
    putStrLn $ "\nRunning on " ++ os
    tmp <- getTemporaryDirectory

    -- We only expect Linux to have GNU extensions
    let gnuHandling = if os == "linux"
                         then id
                         else ignoreTest

    defaultMain $ testGroup "Tests" [
        testGroup "POSIX Functionality" (posixTests tmp),
        gnuHandling $ testGroup "GNU Extensions" (gnuTests tmp)
        ]

posixTests :: FilePath -> [TestTree]
posixTests tmp = [
    testCase "Basic case" $
        glob globDefaults "/usr/bin" !@?= ["/usr/bin"],

    testCase "Non-existant path" $
        glob globDefaults "/foo" !@?= [],

    testCase "globMany" . withTempFile "foo" $ \f1 ->
        withTempFile "bar" $ \f2 ->
            globMany globDefaults [tmp </> "foo", tmp </> "bar"] !@?= [f1, f2],

    testCase "GLOB_MARK" $
        glob globMark "/usr/bin" !@?= ["/usr/bin/"],

    testCase "GLOB_NOCHECK" $
        glob globNoCheck "/foo" !@?= ["/foo"],

    testCase "GLOB_ESCAPE" . withTempFile "a?b" $ \f ->
        glob globDefaults (tmp </> "a\\?b") !@?= [f],

    testCase "GLOB_NOESCAPE" . withTempFile "a\\?b" $ \f ->
        glob globNoEscape (tmp </> "a\\?b") !@?= [f]

    ]

gnuTests :: FilePath -> [TestTree]
gnuTests tmp = [
    testCase "GLOB_NOMAGIC" $
        glob globNoMagic "/foo" !@?= ["/foo"],


    testCase "GLOB_PERIOD" . withTempFile ".ab" $ \f -> do
        glob globDefaults           (tmp </> "?ab") !@?= []
        glob globPeriod (tmp </> "?ab") !@?= [f],

    testCase "GLOB_BRACE" . withTempFile "foo" $ \f1 ->
        withTempFile "far" $ \f2 -> do
            glob globDefaults          (tmp </> "f{oo,ar}") !@?= []
            glob globBrace (tmp </> "f{oo,ar}") !@?= [f1, f2],

    testCase "GLOB_TILDE" $ do
        h <- getHomeDirectory
        user <- getUserName
        glob globDefaults          ('~':user) !@?= []
        glob globTilde ('~':user) !@?= [h]

    ]


-- Convenience operator that eliminates the need to bind the actual result before asserting it is equal to the expected result
(!@?=) :: (Eq a, Show a) => IO a -> a -> Assertion
(!@?=) x y = do
    x' <- x
    x' @?= y

-- Deterministic temp file creation & deletion
withTempFile :: FilePath -> (FilePath -> IO a) -> IO a
withTempFile f cb = do
    d <- getTemporaryDirectory
    let f' = d </> f
    let createFile fp = withFile fp WriteMode (\_ -> return ())

    bracket_
        (createFile f')
        (removeFile f')
        (cb f')

-- Gets the username of the current user
-- We can't use getLoginName here because it requires a controlling tty
getUserName :: IO String
getUserName = userName <$> (getUserEntryForID =<< getEffectiveUserID)

