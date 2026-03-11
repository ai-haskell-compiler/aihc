module Crypto.HPKE.PublicKey (
    SecretKey,
    PublicKey,
    serializePublicKey,
    deserializePublicKey,
    serializeSecretKey,
    deserializeSecretKey,
)
where

import Crypto.ECC (
    EllipticCurve (..),
    Point,
    Scalar,
    decodePoint,
    decodeScalar,
    encodePoint,
    encodeScalar,
 )

import Crypto.HPKE.Types

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Crypto.ECC
-- >>> import Crypto.Hash.Algorithms
-- >>> import Data.ByteString

----------------------------------------------------------------

type PublicKey group = Point group
type SecretKey group = Scalar group

----------------------------------------------------------------

serializePublicKey
    :: EllipticCurve group
    => Proxy group -> PublicKey group -> EncodedPublicKey
serializePublicKey proxy pk = EncodedPublicKey $ encodePoint proxy pk

deserializePublicKey
    :: EllipticCurve group
    => Proxy group -> EncodedPublicKey -> Either HPKEError (PublicKey group)
deserializePublicKey proxy (EncodedPublicKey pkm) =
    case decodePoint proxy pkm of
        CryptoPassed a -> Right a
        CryptoFailed _ -> Left $ DeserializeError "deserializePublicKey"

serializeSecretKey
    :: EllipticCurve group
    => Proxy group -> SecretKey group -> EncodedSecretKey
serializeSecretKey proxy pk = EncodedSecretKey $ encodeScalar proxy pk

deserializeSecretKey
    :: EllipticCurve group
    => Proxy group -> EncodedSecretKey -> Either HPKEError (SecretKey group)
deserializeSecretKey proxy (EncodedSecretKey pkm) =
    case decodeScalar proxy pkm of
        CryptoPassed a -> Right a
        CryptoFailed _ -> Left $ DeserializeError "deserializeSecretKey"
