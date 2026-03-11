{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Internals related to reading and writing an injected executable
-- hash.
module System.Executable.Hash.Internal where

import           Control.Exception (SomeException, handle)
import "cryptohash" Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import           Data.FileEmbed (dummySpaceWith, injectWith)
import           Language.Haskell.TH (Q, Exp)
import           System.Directory (doesFileExist)

-- |  This generates an expression which yields the injected SHA1 hash.
--
-- The generated expression yields a 'Just' value when the injected
-- SHA1 hash is present in the executable.  This hash is usually
-- injected due to a usage of 'injectExecutableHash' /
-- 'maybeInjectExecutableHash'.
injectedExecutableHash :: Q Exp
injectedExecutableHash =
    [|
    let bs = $(dummySpaceWith "executable-hash" 20)
     in if BS.all (== toEnum (fromEnum '0')) bs
           then Nothing
           else Just bs
    |]

-- | Given the path to an executable, computes its hash and injects it
-- into the binary, such that when that program demands the value of
-- 'injectedExecutableHash', it yields a 'Just' value.
--
-- See the documentation in "System.Executable.Hash" for an example of
-- how to use this with a cabal @postBuild@ hook
injectExecutableHash :: FilePath -> IO ()
injectExecutableHash fp = handle addPathToException $ do
    binary <- BS.readFile fp
    let sha1 = hash binary
    case injectWith "executable-hash" sha1 binary of
        Nothing -> fail "Impossible: dummy space too small for executable-hash."
        Just binary' -> do
            BS.writeFile fp binary'
            putStrLn $ "Successfully wrote " ++ fp ++ " with injected hash."
  where
    addPathToException ex = fail $
        "While injecting hash into " ++ fp ++
        ", the following exception occurred: " ++ show (ex :: SomeException)

-- | Injects an executable hash into the specified binary.  If it
-- doesn't exist, then this prints a message to stdout indicating that
-- it failed to inject the hash.
maybeInjectExecutableHash :: FilePath -> IO ()
maybeInjectExecutableHash fp = do
    exists <- doesFileExist fp
    if exists
        then injectExecutableHash fp
        else putStrLn $ concat
            [ "Not injecting executable hash into "
            , fp
            , ", as it doesn't exist."
            ]
