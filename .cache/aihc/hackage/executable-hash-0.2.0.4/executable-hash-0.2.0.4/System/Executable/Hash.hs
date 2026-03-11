{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functions for accessing or computing a SHA1
-- hash of the program's executable.  Most users are expected to use
-- the 'executableHash' function.
--
-- To inject the hash into the executable, you can use the
-- @inject-executable-hash@ program installed along with this package.
-- Alternatively, to do this automatically with cabal, place this in
-- your @Setup.hs@:
--
-- @
--      import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, postBuild)
--      import Distribution.Simple.LocalBuildInfo (buildDir)
--      import System.Executable.Hash.Internal (maybeInjectExecutableHash)
--      import System.FilePath ((\</>))
--
--      main :: IO ()
--      main = defaultMainWithHooks $ simpleUserHooks
--          { postBuild = \_ _ _ buildInfo ->
--              maybeInjectExecutableHash (buildDir buildInfo \</> "exeName\/exeName")
--          }
-- @
--
-- (Note: you'll need to change the executable path)
module System.Executable.Hash
    ( executableHash
    , computeExecutableHash
    ) where

import           Control.Applicative ((<$>))
import           Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import           Language.Haskell.TH (Q, Exp)
import           System.Environment.Executable (getScriptPath, ScriptPath(..))
import           System.Executable.Hash.Internal

-- | This generates an expression which yields a SHA1 hash.  The
-- generated expression has the type @IO (Maybe ByteString)@, just
-- like 'computeExecutableHash'.
--
-- If a SHA1 hash of the executable has been injected into it, then
-- it's directly yielded by this expression.  Otherwise, a hash is
-- computed with 'computeExecutableHash'.
--
-- Note that you shouldn't rely on the result being the actual SHA1
-- hash of the executable, because injecting the hash modifies the
-- binary, and so changes the result of 'computeExecutableHash'.
-- Instead, this should only be used as a way to uniquely identify the
-- contents of the executable.
--
-- This yields 'Nothing' when run with @runhaskell@ or @ghci@.
executableHash :: Q Exp
executableHash =
    [|
    case $(injectedExecutableHash) of
        Just x -> return (Just x)
        Nothing -> computeExecutableHash
    |]

-- | Computes the SHA1 hash of the program executable.
--
-- This yields 'Nothing' when run with @runhaskell@ or @ghci@.
computeExecutableHash :: IO (Maybe BS.ByteString)
computeExecutableHash = do
    sp <- getScriptPath
    case sp of
        Executable fp -> Just . hash <$> BS.readFile fp
        _ -> return Nothing
