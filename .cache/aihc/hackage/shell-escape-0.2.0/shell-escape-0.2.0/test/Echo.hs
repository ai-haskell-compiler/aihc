{-# LANGUAGE OverloadedStrings
  #-}

import System.Environment
import System.Process (runInteractiveProcess, waitForProcess, ProcessHandle)
import System.IO (Handle, stderr, stdout, stdin, hClose)
import Data.ByteString (ByteString, pack, hGetContents)
import qualified Data.ByteString
import Data.ByteString.Char8 (unpack, hPutStrLn)
import qualified Data.ByteString.Char8 as Char8
import Data.Word
import Control.Applicative
import Control.Monad
import qualified Text.Printf
import Data.IORef
import Data.List

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.ByteString.ShellEscape hiding (sh, bash)


--  It is best to implement the echo test with `printf':
--
-- .  Some echo implementations always interprete backslash escapes like \3
--    and give us no way to turn it off. Dash is like this.
--
-- .  GNU echo can not be made to simply ignore options like `--help'.
--
--  The `printf' implementations available to me -- Bash and Dash --
--  consistently ignore backslash escape sequences and any options that follow
--  the format argument.
--
printf                      ::  (Shell t, Escape t) => t -> IO ByteString
printf escaped               =  do
  (i, o, e, p)              <-  shell escaped cmd
  exit                      <-  waitForProcess p
  hGetContents o <* mapM_ hClose [i, o, e]
 where
  cmd                        =  "printf '%s' " ++ unpack (bytes escaped)


prop_echoBash               ::  ByteString -> Property
prop_echoBash                =  something_prop escapeBash

prop_echoSh                 ::  ByteString -> Property
prop_echoSh                  =  something_prop escapeSh

something_prop esc b         =  monadicIO $ do
  assert =<< run (test printf b (esc b))

escapeSh                    ::  ByteString -> Sh
escapeSh b                   =  escape b

escapeBash                  ::  ByteString -> Bash
escapeBash b                 =  escape b



test cmd original escaped    =  do
  b                         <-  cmd escaped
  Data.ByteString.appendFile "./lengths" (displayLength original)
  Data.ByteString.appendFile "./chars" (Char8.unlines $ displayBytes original)
  when (b /= original) $ do
    err "Result differs from unescaped result:"
    err "Escaped form:"
    err (Char8.unwords . displayBytes $ bytes escaped)
    err "Output:"
    err (Char8.unwords . displayBytes $ b)
    err "Original:"
    err (Char8.unwords . displayBytes $ original)
  return (b == original)


displayBytes                 =  fmap Char8.pack
                             .  fmap (\w ->  Text.Printf.printf "0x%02x" w)
                             .  Data.ByteString.unpack

displayLength                =  Char8.pack
                             .  (\w ->  Text.Printf.printf "%4d\n" w)
                             .  Data.ByteString.length

class (Escape t) => Shell t where
  shell :: t -> String -> IO (Handle, Handle, Handle, ProcessHandle)
instance Shell Sh where
  shell _                    =  sh
instance Shell Bash where
  shell _                    =  bash


sh s = runInteractiveProcess "sh" ["-c", s] Nothing Nothing
bash s = runInteractiveProcess "bash" ["-c", s] Nothing Nothing

instance Arbitrary ByteString where
  arbitrary                  =  do
    bytes                   <-  arbitrary :: Gen [NonZero Word8]
    NonEmpty bytes'         <-  arbitrary :: Gen (NonEmptyList (NonZero Word8))
    pack `fmap` elements
                (fmap unNonZero `fmap` [bytes', bytes', bytes, bytes', bytes'])
   where
    unNonZero (NonZero t) = t

main                         =  do
  runSh                     <-  newIORef True
  runBash                   <-  newIORef True
  testsR                    <-  newIORef 10000
  let testsRwrite            =  writeIORef testsR
      noBash                 =  writeIORef runBash False
      noSh                   =  writeIORef runSh False
  args                      <-  getArgs
  case (sort . nub) args of
    ["--bash", "--sh", d]   ->  testsRwrite (read d)
    ["--bash", "--sh"   ]   ->  return ()
    ["--bash",         d]   ->  noSh >> testsRwrite (read d)
    [          "--sh", d]   ->  noBash >> testsRwrite (read d)
    [                  d]   ->  testsRwrite (read d)
    [                   ]   ->  return ()
    _                       ->  error "Invalid arguments."
  tests                     <-  readIORef testsR
  let msg                    =  "Performing " ++ show tests ++ " tests."
      qcArgs                 =  Args Nothing tests tests 32 False
      qc                     =  quickCheckWith qcArgs
  err "Tests are random ByteStrings, containing any byte but null."
  runSh ?> do
    err "Testing Sh escaping."
    err (Char8.pack msg)
    qc prop_echoSh
  runBash ?> do
    err "Testing Bash escaping."
    err (Char8.pack msg)
    qc prop_echoBash

err                          =  hPutStrLn stderr

(?>)                        ::  IORef Bool -> IO () -> IO ()
ref ?> action                =  readIORef ref >>= (`when` action)


