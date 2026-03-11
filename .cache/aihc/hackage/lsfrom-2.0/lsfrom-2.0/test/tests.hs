{-# LANGUAGE CPP #-}

import Control.Monad (unless)
import SimpleCmd (cmdN, error')
import System.IO (hGetContents)
import System.Process (CreateProcess(..), StdStream(CreatePipe),
                       proc, withCreateProcess)

type Test = (Maybe String, FilePath, [String], [String])

lsfrom :: Test -> IO ()
lsfrom (mlocale, dir, args, expect) = do
  let menv = mlocale >>= \l -> Just [("LC_ALL",l)]
  withCreateProcess
    (proc "lsfrom" args)
    { cwd = Just dir, env = menv, std_out = CreatePipe } $
    \ _si mso _se _p ->
      case mso of
        Nothing -> error "no stdout handle found"
        Just so -> do
          out <- hGetContents so
          unless (lines out == expect) $ do
            print mlocale
            cmdN "lsfrom" args
            putStrLn $ "returned> " ++ show out
            putStrLn $ "expected> " ++ show expect
            error' "failed"

tests :: [Test]
tests =
  [ (Nothing, "test/files", ["-f", "B"], ["B","C"])
  , (Nothing, "test/files", ["-a", "B"], ["C"])
  , (Nothing, "test", ["-f", "files/C/"], ["files/C"])
  , (Nothing, "test/files", ["-f", "Bb"], ["C"])
  , (Nothing, "test/files", ["-u", "B", "-f", "A"], ["A", "B"])
  , (Nothing, "test/files", ["-b", "C", "-f", "A"], ["A", "B"])
  , (Nothing, "test/files", ["-b", "C", "-a", "A"], ["B"])
  , (Nothing, "test/files", ["-b", "A", "-a", "C"], [])
  , (Nothing, "test/files", ["-b", "B", "-f", "B"], [])
  , (Nothing, "test/files", ["-u", "B", "-a", "B"], [])
  , (Nothing, "test/files", ["-b", "B", "-a", "B"], [])
  , (Nothing, "test/files", ["-s", "-u", "B", "-f", "A"], ["A", "B"])
  , (Just "C", "test/cases", [], ["A", "B", "a", "b"])
  , (Just "ja_JP.utf8", "test/cases", [], ["A", "B", "a", "b"])
  , (Just "en_US.UTF-8", "test/cases", [], ["a", "A", "b", "B"])
  ]

main :: IO ()
main = do
  mapM_ lsfrom tests
  putStrLn $ show (length tests) ++ " tests run"

#if !MIN_VERSION_simple_cmd(0,1,4)
error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
