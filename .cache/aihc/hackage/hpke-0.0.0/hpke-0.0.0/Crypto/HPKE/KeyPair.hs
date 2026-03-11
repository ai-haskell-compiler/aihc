{-# LANGUAGE RecordWildCards #-}

module Crypto.HPKE.KeyPair where

import qualified Control.Exception as E
import Crypto.ECC (
    EllipticCurve (..),
 )
import Crypto.HPKE.ID
import Crypto.HPKE.KEM (genKeyPairP)
import Crypto.HPKE.Types

----------------------------------------------------------------

-- | Generating a pair of public key and secret key based on
-- 'KEM_ID'.
genKeyPair
    :: HPKEMap -> KEM_ID -> IO (EncodedPublicKey, EncodedSecretKey)
genKeyPair HPKEMap{..} kem_id = case lookup kem_id kemMap of
    Nothing -> E.throwIO $ Unsupported $ show kem_id
    Just (KEMGroup proxy, _) -> do
        (pk, sk) <- genKeyPairP proxy
        let pkm = EncodedPublicKey $ encodePoint proxy pk
            skm = EncodedSecretKey $ encodeScalar proxy sk
        return (pkm, skm)
