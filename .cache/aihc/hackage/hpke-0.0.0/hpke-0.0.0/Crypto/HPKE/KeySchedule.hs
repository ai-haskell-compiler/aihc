{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.HPKE.KeySchedule (
    -- * Types
    Mode (..),

    -- * Key schedule
    keySchedule,
) where

import Crypto.KDF.HKDF (toPRK)
import qualified Data.ByteString as BS

import Crypto.HPKE.KDF
import Crypto.HPKE.Types

----------------------------------------------------------------

data Mode
    = ModeBase
    | ModePsk
    | ModeAuth
    | ModeAuthPsk
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}
fromMode :: Mode -> Word8
fromMode ModeBase    = 0x00
fromMode ModePsk     = 0x01
fromMode ModeAuth    = 0x02
fromMode ModeAuthPsk = 0x03
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

keySchedule
    :: forall h
     . (HashAlgorithm h, KDF h)
    => h
    -> Suite
    -> Int
    -> Int
    -> Mode
    -> Info
    -> PSK
    -> PSK_ID
    -> SharedSecret
    -> Either HPKEError (Key, Nonce, Int, PRK h)
keySchedule h suite nk nn mode info psk psk_id shared_secret =
    case toPRK exporter_secret of
        Nothing -> Left $ KeyScheduleError "cannot convert to PRK"
        Just prk -> Right (key, base_nonce, 0, prk)
  where
    psk_id_hash = labeledExtract suite "" "psk_id_hash" psk_id :: PRK h
    info_hash = labeledExtract suite "" "info_hash" info :: PRK h
    key_schedule_context =
        BS.singleton (fromMode mode) <> convert psk_id_hash <> convert info_hash
            :: ByteString

    secret = labeledExtract suite (convert shared_secret) "secret" psk :: PRK h

    key = labeledExpand suite secret "key" key_schedule_context nk
    base_nonce = labeledExpand suite secret "base_nonce" key_schedule_context nn

    exporter_secret = labeledExpand suite secret "exp" key_schedule_context $ hashDigestSize h
