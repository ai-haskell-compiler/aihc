{-# LANGUAGE OverloadedStrings #-}

module Database.CQL.IO.Log
    ( Logger (..)
    , LogLevel (..)
    , nullLogger
    , stdoutLogger
    , logDebug
    , logInfo
    , logWarn
    , logError
    , module BB
    ) where

import Control.Monad (when)
import Data.ByteString.Builder as BB
import Data.ByteString.Lazy (ByteString)
import Database.CQL.IO.Hexdump
import Data.Semigroup ((<>))

import qualified Data.ByteString.Lazy.Char8 as Char8

-- | A 'Logger' provides functions for logging textual messages as well as
-- binary CQL protocol requests and responses emitted by the client.
data Logger = Logger
    { logMessage  :: LogLevel -> Builder -> IO ()
    , logRequest  :: ByteString -> IO ()
    , logResponse :: ByteString -> IO ()
    }

-- | Log levels used by the client.
data LogLevel
    = LogDebug
        -- ^ Verbose debug information that should not be enabled in
        -- production environments.
    | LogInfo
        -- ^ General information concerning client and cluster state.
    | LogWarn
        -- ^ Warnings of potential problems that should be investigated.
    | LogError
        -- ^ Errors that should be investigated and monitored.
    deriving (Eq, Ord, Show, Read)

-- | A logger that discards all log messages.
nullLogger :: Logger
nullLogger = Logger
    { logMessage  = \_ _ -> return ()
    , logRequest  = \_   -> return ()
    , logResponse = \_   -> return ()
    }

-- | A logger that writes all log messages to stdout, discarding log messages
-- whose level is less than the given level. Requests and responses are
-- logged on debug level, formatted in hexadecimal blocks.
stdoutLogger :: LogLevel -> Logger
stdoutLogger l = Logger
    { logMessage  = \l' m -> when (l <= l') $
        Char8.putStrLn (withLevel l' (toLazyByteString m))
    , logRequest  = \rq   -> when (l <= LogDebug) $
        Char8.putStrLn (hexdump rq)
    , logResponse = \rs   -> when (l <= LogDebug) $
        Char8.putStrLn (hexdump rs)
    }
  where
    withLevel LogDebug m = "[Debug] " <> m
    withLevel LogInfo  m = "[Info]  " <> m
    withLevel LogWarn  m = "[Warn]  " <> m
    withLevel LogError m = "[Error] " <> m

logDebug :: Logger -> Builder -> IO ()
logDebug l = logMessage l LogDebug
{-# INLINE logDebug #-}

logInfo :: Logger -> Builder -> IO ()
logInfo l = logMessage l LogInfo
{-# INLINE logInfo #-}

logWarn :: Logger -> Builder -> IO ()
logWarn l = logMessage l LogWarn
{-# INLINE logWarn #-}

logError :: Logger -> Builder -> IO ()
logError l = logMessage l LogError
{-# INLINE logError #-}

