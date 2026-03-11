{-# LANGUAGE RecordWildCards #-}

module Crypto.HPKE.Context (
    -- * Sender
    ContextS,
    newContextS,
    seal,
    exportS,

    -- * Receiver
    ContextR,
    newContextR,
    open,
    exportR,
) where

import qualified Control.Exception as E
import Data.ByteArray (xor)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Crypto.HPKE.Types

----------------------------------------------------------------

-- | Context for senders.
data ContextS = ContextS
    { seqRefS :: IORef Integer
    , sealS :: Seal
    , nonceBaseS :: Nonce
    , expandS :: Info -> Int -> Key
    }

-- | Context for receivers.
data ContextR = ContextR
    { seqRefR :: IORef Integer
    , openR :: Open
    , nonceBaseR :: Nonce
    , expandR :: Info -> Int -> Key
    }

----------------------------------------------------------------

-- | Encryption.
--   This throws 'HPKEError'.
seal :: ContextS -> AAD -> PlainText -> IO CipherText
seal ContextS{..} aad pt = do
    seqI <- readIORef seqRefS
    let len = BS.length nonceBaseS
        seqBS = i2ospOf_ len seqI :: ByteString
        nonce = seqBS `xor` nonceBaseS
        ect = sealS nonce aad pt
        seqI' = seqI + 1
    writeIORef seqRefS seqI'
    case ect of
        Right ct -> return ct
        Left err -> E.throwIO err

-- | Decryption.
--   This throws 'HPKEError'.
open
    :: ContextR -> AAD -> CipherText -> IO PlainText
open ContextR{..} aad ct = do
    seqI <- readIORef seqRefR
    let len = BS.length nonceBaseR
        seqBS = i2ospOf_ len seqI :: ByteString
        nonce = seqBS `xor` nonceBaseR
        ept = openR nonce aad ct
    case ept of
        Left err -> E.throwIO err
        Right pt -> do
            let seqI' = seqI + 1
            writeIORef seqRefR seqI'
            return pt

----------------------------------------------------------------

-- | Exporting secret.
exportS :: ContextS -> Info -> Int -> Key
exportS ContextS{..} exporter_context len =
    expandS exporter_context len

-- | Exporting secret.
exportR :: ContextR -> Info -> Int -> Key
exportR ContextR{..} exporter_context len =
    expandR exporter_context len

----------------------------------------------------------------

newContextS
    :: Key
    -> Nonce
    -> (Key -> Seal)
    -> (Info -> Int -> Key)
    -> IO ContextS
newContextS key nonce_base seal' expand = do
    seqref <- newIORef 0
    return $
        ContextS
            { seqRefS = seqref
            , sealS = seal' key
            , nonceBaseS = nonce_base
            , expandS = expand
            }

----------------------------------------------------------------

newContextR
    :: Key
    -> Nonce
    -> (Key -> Open)
    -> (Info -> Int -> Key)
    -> IO ContextR
newContextR key nonce_base open' expand = do
    seqref <- newIORef 0
    return $
        ContextR
            { seqRefR = seqref
            , openR = open' key
            , nonceBaseR = nonce_base
            , expandR = expand
            }
