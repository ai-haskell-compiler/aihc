{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Crypto.Types.PubKey.DSA
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- references:
--   <https://tools.ietf.org/html/rfc6979>
--
module Crypto.Types.PubKey.DSA
    ( Params(..)
    , Signature(..)
    , PublicNumber
    , PublicKey(..)
    , PrivateNumber
    , PrivateKey(..)
    , KeyPair(..)
    , toPublicKey
    , toPrivateKey
    ) where

import Data.Data
import Data.ASN1.Types

-- | DSA Public Number, usually embedded in DSA Public Key
type PublicNumber = Integer

-- | DSA Private Number, usually embedded in DSA Private Key
type PrivateNumber = Integer

-- | Represent DSA parameters namely P, G, and Q.
data Params = Params
    { params_p :: Integer -- ^ DSA p
    , params_g :: Integer -- ^ DSA g
    , params_q :: Integer -- ^ DSA q
    } deriving (Show,Read,Eq,Data,Typeable)

instance ASN1Object Params where
    toASN1 params = \xs -> Start Sequence
                         : IntVal (params_p params)
                         : IntVal (params_q params)
                         : IntVal (params_g params)
                         : End Sequence
                         : xs
    fromASN1 (Start Sequence:IntVal p:IntVal q:IntVal g:End Sequence:xs) =
        Right (Params { params_p = p, params_g = g, params_q = q }, xs)
    fromASN1 _ = Left "fromASN1: DSA.Params: unexpected format"

-- | Represent a DSA signature namely R and S.
data Signature = Signature
    { sign_r :: Integer -- ^ DSA r
    , sign_s :: Integer -- ^ DSA s
    } deriving (Show,Read,Eq,Data,Typeable)

instance ASN1Object Signature where
    toASN1 sign = \xs -> Start Sequence
                         : IntVal (sign_r sign)
                         : IntVal (sign_s sign)
                         : End Sequence
                         : xs
    fromASN1 (Start Sequence:IntVal r:IntVal s:End Sequence:xs) =
        Right (Signature { sign_r = r, sign_s = s }, xs)
    fromASN1 _ = Left "fromASN1: DSA.Signature: unexpected format"

-- | Represent a DSA public key.
data PublicKey = PublicKey
    { public_params :: Params       -- ^ DSA parameters
    , public_y      :: PublicNumber -- ^ DSA public Y
    } deriving (Show,Read,Eq,Data,Typeable)

-- DSA public key serialization doesn't typically look like this.
-- However to provide an instance we serialize params and the public
-- number together.
instance ASN1Object PublicKey where
    toASN1 pubKey = \xs -> Start Sequence
                         : IntVal (params_p params)
                         : IntVal (params_q params)
                         : IntVal (params_g params)
                         : End Sequence
                         : IntVal (public_y pubKey)
                         : xs
        where params = public_params pubKey
    fromASN1 l = case fromASN1 l of
                    Left err -> Left err
                    Right (dsaParams, ls) -> case ls of
                                                IntVal dsaPub : ls2 -> Right (PublicKey dsaParams dsaPub, ls2)
                                                _                   -> Left "fromASN1: DSA.PublicKey: unexpected format"

-- | Represent a DSA private key.
--
-- Only x need to be secret.
-- the DSA parameters are publicly shared with the other side.
data PrivateKey = PrivateKey
    { private_params :: Params        -- ^ DSA parameters
    , private_x      :: PrivateNumber -- ^ DSA private X
    } deriving (Show,Read,Eq,Data,Typeable)

-- | Represent a DSA key pair
data KeyPair = KeyPair Params PublicNumber PrivateNumber
    deriving (Show,Read,Eq,Data,Typeable)

instance ASN1Object KeyPair where
    toASN1 (KeyPair params pub priv) = \xs ->
          Start Sequence
        : IntVal 0
        : IntVal (params_p params)
        : IntVal (params_q params)
        : IntVal (params_g params)
        : IntVal pub
        : IntVal priv
        : End Sequence
        : xs
    fromASN1 (Start Sequence : IntVal n : xs)
        | n == 0    = case xs of
                        IntVal p : IntVal q : IntVal g : IntVal pub : IntVal priv : End Sequence : xs2 ->
                            let params = Params { params_p = p, params_g = g, params_q = q }
                             in Right (KeyPair params pub priv, xs2)
                        _                                                                              ->
                            Left "fromASN1: DSA.KeyPair: invalid format (version=0)"
        | otherwise = Left "fromASN1: DSA.KeyPair: unknown format"
    fromASN1 _ = Left "fromASN1: DSA.KeyPair: unexpected format"

-- | Public key of a DSA Key pair
toPublicKey :: KeyPair -> PublicKey
toPublicKey (KeyPair params pub _) = PublicKey params pub

-- | Private key of a DSA Key pair
toPrivateKey :: KeyPair -> PrivateKey
toPrivateKey (KeyPair params _ priv) = PrivateKey params priv
