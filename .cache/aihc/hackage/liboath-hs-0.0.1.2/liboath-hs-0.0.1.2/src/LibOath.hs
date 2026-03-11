{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : LibOath
Description : This function exports a safe interface to the liboath library.
Copyright   : (c) Matt Parsons, 2017
License     : GPL-3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module exports a safe and idiomatic interface to liboath. It is designed to
be imported qualified.
-}
module LibOath
    ( module LibOath
    , Error(..)
    , pattern Base32
    , Base32(unBase32)
    ) where

import           Control.Exception.Safe
import           Control.Monad          (join)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import qualified Foreign.C.String       as C
import qualified Foreign.Marshal.Alloc  as Alloc
import           Foreign.Storable       (peek)
import qualified Language.C.Inline      as C

import           LibOath.Internal

-- | Unwrap an 'Action', returning 'Either' an 'Error' caused by initialization
-- failure of
run :: Action a -> IO (Either Error a)
run = bracket initialize finalize . handle
  where
    initialize =
        fmap fromCInt oath_init
    finalize _ =
        fromCInt <$> oath_done
    handle action OK =
        Right <$> unAction action
    handle _ err =
        pure (Left err)

generateTOTPFromSecret :: ByteString -> IO (Either Error ByteString)
generateTOTPFromSecret bs = fmap join . run $ do
    eerrb32 <- decodeBase32 bs
    case eerrb32 of
        Left err  -> pure (Left err)
        Right b32 -> generateTOTP b32


-- | This function takes the given 'ByteString' and decodes into a 'Base32'
-- representation.
decodeBase32 :: ByteString -> Action (Either Error Base32)
decodeBase32 bs = Action $ do
    BS.useAsCString bs $ \cStr -> do
        secretPtr <- Alloc.malloc
        secretLenPtr <- Alloc.malloc
        oath_base32_decode cStr (fromIntegral (BS.length bs)) secretPtr secretLenPtr
            `whenOKM` do
                secretLen <- peek secretLenPtr
                secret <- peek secretPtr
                newBS <- BS.packCStringLen (secret, fromIntegral secretLen)
                pure (UnsafeBase32 newBS)

-- | This function generates a TOTP for the given 'Base32' with the default
-- configuration.
generateTOTP :: Base32 -> Action (Either Error ByteString)
generateTOTP (Base32 bs) = Action $ do
    BS.useAsCString bs $ \secret -> do
        let secretLen = fromIntegral (BS.length bs)
        -- assuming totp given
        -- this is going to
        now <- fromIntegral . floor <$> getPOSIXTime
        -- assume all defaults
        -- TODO: extract this to an options
        let whenTime = now
            t0 = 0
            timeStepSize = 0
            digits = 6
            window = 0
            iter = 1
        otpPtr <- C.newCString (replicate (fromIntegral digits + 1) ' ')
        oath_totp_generate
            secret secretLen
            (whenTime + iter * timeStepSize)
            (fromIntegral (case timeStepSize of C.CTime i -> i))
            t0
            digits
            otpPtr
            `whenOKM` do
                BS.packCString otpPtr

whenOK :: C.CInt -> IO a -> IO (Either Error a)
whenOK rc action =
    case fromCInt rc of
        OK  -> Right <$> action
        err -> pure (Left err)

whenOKM :: IO C.CInt -> IO a -> IO (Either Error a)
whenOKM mrc action = do
    rc <- mrc
    whenOK rc action
