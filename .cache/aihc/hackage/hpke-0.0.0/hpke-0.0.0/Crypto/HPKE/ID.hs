{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}

module Crypto.HPKE.ID (
    AEAD_ID (AES_128_GCM, AES_256_GCM, ChaCha20Poly1305, ..),
    AEADCipher (..),
    defaultAEADMap,
    --
    KDF_ID (HKDF_SHA256, HKDF_SHA384, HKDF_SHA512, ..),
    KDFHash (..),
    defaultKDFMap,
    --
    KEM_ID (
        DHKEM_P256_HKDF_SHA256,
        DHKEM_P384_HKDF_SHA384,
        DHKEM_P521_HKDF_SHA512,
        DHKEM_X25519_HKDF_SHA256,
        DHKEM_X448_HKDF_SHA512,
        ..
    ),
    defaultKEMMap,
    KEMGroup (..),
    --
    HPKEMap (..),
    defaultHPKEMap,
) where

import Crypto.Cipher.AES (AES128, AES256)
import Crypto.Cipher.ChaChaPoly1305 (ChaCha20Poly1305)
import Crypto.ECC (
    Curve_P256R1,
    Curve_P384R1,
    Curve_P521R1,
    Curve_X25519,
    Curve_X448,
    EllipticCurve (..),
    EllipticCurveDH (..),
 )
import Data.Proxy (Proxy (..))
import Data.Word (Word16)
import Text.Printf (printf)

import Crypto.HPKE.AEAD
import Crypto.HPKE.KDF

----------------------------------------------------------------

-- | ID for authenticated encryption with additional data
newtype AEAD_ID = AEAD_ID {fromAEAD_ID :: Word16} deriving (Eq)

{- FOURMOLU_DISABLE -}
pattern AES_128_GCM      :: AEAD_ID
pattern AES_128_GCM       = AEAD_ID 0x0001
pattern AES_256_GCM      :: AEAD_ID
pattern AES_256_GCM       = AEAD_ID 0x0002
pattern ChaCha20Poly1305 :: AEAD_ID
pattern ChaCha20Poly1305  = AEAD_ID 0x0003

instance Show AEAD_ID where
    show AES_128_GCM      = "AES_128_GCM"
    show AES_256_GCM      = "AES_256_GCM"
    show ChaCha20Poly1305 = "ChaCha20Poly1305"
    show (AEAD_ID n)      = "AEAD_ID 0x" ++ printf "%04x" n
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
aes128 :: Proxy AES128
aes128  = Proxy :: Proxy AES128
aes256 :: Proxy AES256
aes256  = Proxy :: Proxy AES256
chacha :: Proxy ChaCha20Poly1305
chacha  = Proxy :: Proxy ChaCha20Poly1305
{- FOURMOLU_ENABLE -}

data AEADCipher = forall a. Aead a => AEADCipher (Proxy a)

{- FOURMOLU_DISABLE -}
defaultAEADMap :: [(AEAD_ID, AEADCipher)]
defaultAEADMap =
    [ (AES_128_GCM,      AEADCipher aes128)
    , (AES_256_GCM,      AEADCipher aes256)
    , (ChaCha20Poly1305, AEADCipher chacha)
    ]
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | ID for key derivation function.
newtype KDF_ID = KDF_ID {fromKDF_ID :: Word16} deriving (Eq)

{- FOURMOLU_DISABLE -}
pattern HKDF_SHA256 :: KDF_ID
pattern HKDF_SHA256  = KDF_ID 0x0001
pattern HKDF_SHA384 :: KDF_ID
pattern HKDF_SHA384  = KDF_ID 0x0002
pattern HKDF_SHA512 :: KDF_ID
pattern HKDF_SHA512  = KDF_ID 0x0003

instance Show KDF_ID where
    show HKDF_SHA256 = "HKDF_SHA256"
    show HKDF_SHA384 = "HKDF_SHA384"
    show HKDF_SHA512 = "HKDF_SHA512"
    show (KDF_ID n)  = "HKDF_ID 0x" ++ printf "%04x" n
{- FOURMOLU_ENABLE -}

data KDFHash = forall h. (HashAlgorithm h, KDF h) => KDFHash h

defaultKDFMap :: [(KDF_ID, KDFHash)]
defaultKDFMap =
    [ (HKDF_SHA256, KDFHash SHA256)
    , (HKDF_SHA384, KDFHash SHA384)
    , (HKDF_SHA512, KDFHash SHA512)
    ]

----------------------------------------------------------------
----------------------------------------------------------------

-- | ID for key encapsulation mechanism.
newtype KEM_ID = KEM_ID {fromKEM_ID :: Word16} deriving (Eq)

{- FOURMOLU_DISABLE -}
pattern DHKEM_P256_HKDF_SHA256   :: KEM_ID
pattern DHKEM_P256_HKDF_SHA256    = KEM_ID 0x0010
pattern DHKEM_P384_HKDF_SHA384   :: KEM_ID
pattern DHKEM_P384_HKDF_SHA384    = KEM_ID 0x0011
pattern DHKEM_P521_HKDF_SHA512   :: KEM_ID
pattern DHKEM_P521_HKDF_SHA512    = KEM_ID 0x0012
pattern DHKEM_X25519_HKDF_SHA256 :: KEM_ID
pattern DHKEM_X25519_HKDF_SHA256  = KEM_ID 0x0020
pattern DHKEM_X448_HKDF_SHA512   :: KEM_ID
pattern DHKEM_X448_HKDF_SHA512    = KEM_ID 0x0021

instance Show KEM_ID where
    show DHKEM_P256_HKDF_SHA256   = "DHKEM(P-256, HKDF-SHA256)"
    show DHKEM_P384_HKDF_SHA384   = "DHKEM(P-384, HKDF-SHA384)"
    show DHKEM_P521_HKDF_SHA512   = "DHKEM(P-521, HKDF-SHA512)"
    show DHKEM_X25519_HKDF_SHA256 = "DHKEM(X25519, HKDF-SHA256)"
    show DHKEM_X448_HKDF_SHA512   = "DHKEM(X448, HKDF-SHA512)"
    show (KEM_ID n)               = "DHKEM_ID 0x" ++ printf "%04x" n
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
p256   :: Proxy Curve_P256R1
p256    = Proxy :: Proxy Curve_P256R1
p384   :: Proxy Curve_P384R1
p384    = Proxy :: Proxy Curve_P384R1
p521   :: Proxy Curve_P521R1
p521    = Proxy :: Proxy Curve_P521R1
x25519 :: Proxy Curve_X25519
x25519  = Proxy :: Proxy Curve_X25519
x448   :: Proxy Curve_X448
x448    = Proxy :: Proxy Curve_X448

data KEMGroup
    = forall c. (EllipticCurve c, EllipticCurveDH c) => KEMGroup (Proxy c)

defaultKEMMap :: [(KEM_ID, (KEMGroup, KDFHash))]
defaultKEMMap =
    [ (DHKEM_P256_HKDF_SHA256,   (KEMGroup p256,   KDFHash SHA256))
    , (DHKEM_P384_HKDF_SHA384,   (KEMGroup p384,   KDFHash SHA384))
    , (DHKEM_P521_HKDF_SHA512,   (KEMGroup p521,   KDFHash SHA512))
    , (DHKEM_X25519_HKDF_SHA256, (KEMGroup x25519, KDFHash SHA256))
    , (DHKEM_X448_HKDF_SHA512,   (KEMGroup x448,   KDFHash SHA512))
    ]
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

data HPKEMap = HPKEMap
    { kemMap :: [(KEM_ID, (KEMGroup, KDFHash))]
    , kdfMap :: [(KDF_ID, KDFHash)]
    , cipherMap :: [(AEAD_ID, AEADCipher)]
    }

defaultHPKEMap :: HPKEMap
defaultHPKEMap =
    HPKEMap
        { kemMap = defaultKEMMap
        , kdfMap = defaultKDFMap
        , cipherMap = defaultAEADMap
        }
