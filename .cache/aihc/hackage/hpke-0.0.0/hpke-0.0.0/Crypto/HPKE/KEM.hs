{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.HPKE.KEM (
    encapGen,
    encapEnv,
    decapEnv,
    genKeyPairP,
)
where

import qualified Control.Exception as E
import Crypto.ECC (
    EllipticCurve (..),
    EllipticCurveDH (..),
    KeyPair (..),
 )
import Crypto.Random (drgNew, withDRG)

import Crypto.HPKE.PublicKey
import Crypto.HPKE.Types

----------------------------------------------------------------

encap
    :: (EllipticCurve group, EllipticCurveDH group)
    => Env group
    -> Encap
encap Env{..} enc0@(EncodedPublicKey pkRm) = do
    pkR <- deserializePublicKey envProxy enc0
    let skE = envSecretKey
    dh0 <- ecdh' envProxy skE pkR $ EncapError "encap"
    (dh, pkSm) <- case envAuthKey of
        Nothing -> return (dh0, "")
        Just skS -> do
            let pkS = scalarToPoint envProxy skS
            dh1 <- ecdh' envProxy skS pkR $ EncapError "encap"
            let EncodedPublicKey pk = serializePublicKey envProxy pkS
            return (dh0 <> dh1, pk)
    let pkE = scalarToPoint envProxy skE
    let enc@(EncodedPublicKey pkEm) = serializePublicKey envProxy pkE
        kem_context = pkEm <> pkRm <> pkSm
        shared_secret = SharedSecret $ convert $ envDerive dh kem_context
    return (shared_secret, enc)

encapGen
    :: (EllipticCurve group, EllipticCurveDH group)
    => Proxy group
    -> KeyDeriveFunction
    -> Maybe EncodedSecretKey
    -> IO Encap
encapGen proxy derive mskSm = do
    mskS <- case mskSm of
        Nothing -> return $ Nothing
        Just skSm -> case deserializeSecretKey proxy skSm of
            Left err -> E.throwIO err
            Right x -> return $ Just x
    env <- genEnv proxy derive mskS
    return $ encap env

encapEnv
    :: (EllipticCurve group, EllipticCurveDH group)
    => Proxy group
    -> KeyDeriveFunction
    -> EncodedSecretKey
    -> Maybe EncodedSecretKey
    -> Encap
encapEnv proxy derive skRm skSm enc = do
    env <- newEnvDeserialize proxy derive skRm skSm
    encap env enc

----------------------------------------------------------------

decap
    :: (EllipticCurve group, EllipticCurveDH group)
    => Env group
    -> Decap
decap Env{..} enc@(EncodedPublicKey pkEm) = do
    pkE <- deserializePublicKey envProxy enc
    let skR = envSecretKey
    dh0 <- ecdh' envProxy skR pkE $ DecapError "decap"
    (dh, pkSm) <- case envAuthKey of
        Nothing -> return (dh0, "")
        Just skS -> do
            let pkS = scalarToPoint envProxy skS
            dh1 <- ecdh' envProxy skR pkS $ EncapError "decap"
            let EncodedPublicKey pk = serializePublicKey envProxy pkS
            return (dh0 <> dh1, pk)

    let pkR = scalarToPoint envProxy skR
    let EncodedPublicKey pkRm = serializePublicKey envProxy pkR
        kem_context = pkEm <> pkRm <> pkSm
        shared_secret = SharedSecret $ convert $ envDerive dh kem_context
    return shared_secret

decapEnv
    :: (EllipticCurve group, EllipticCurveDH group)
    => Proxy group
    -> KeyDeriveFunction
    -> EncodedSecretKey
    -> Maybe EncodedSecretKey
    -> Decap
decapEnv proxy derive skRm mskSm enc = do
    env <- newEnvDeserialize proxy derive skRm mskSm
    decap env enc

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data Env group = Env
    { envSecretKey :: SecretKey group
    , envAuthKey   :: Maybe (SecretKey group)
    , envProxy     :: Proxy group
    , envDerive    :: KeyDeriveFunction
    }
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

newEnv
    :: forall group
     . EllipticCurve group
    => KeyDeriveFunction
    -> SecretKey group
    -> Maybe (SecretKey group)
    -> Env group
newEnv derive skR mskS =
    Env
        { envSecretKey = skR
        , envAuthKey = mskS
        , envProxy = proxy
        , envDerive = derive
        }
  where
    proxy = Proxy :: Proxy group

----------------------------------------------------------------

genEnv
    :: EllipticCurve group
    => Proxy group
    -> KeyDeriveFunction
    -> Maybe (SecretKey group)
    -> IO (Env group)
genEnv proxy derive mskS = do
    (_, sk) <- genKeyPairP proxy
    return $ newEnv derive sk mskS

genKeyPairP
    :: EllipticCurve curve
    => proxy curve -> IO (Point curve, Scalar curve)
genKeyPairP proxy = do
    gen <- drgNew
    let (KeyPair pk sk, _) = withDRG gen $ curveGenerateKeyPair proxy
    return (pk, sk)

----------------------------------------------------------------

newEnvDeserialize
    :: EllipticCurve group
    => Proxy group
    -> KeyDeriveFunction
    -> EncodedSecretKey
    -> Maybe EncodedSecretKey
    -> Either HPKEError (Env group)
newEnvDeserialize proxy derive skRm mskSm = do
    skR <- deserializeSecretKey proxy skRm
    mskS <- case mskSm of
        Nothing -> Right $ Nothing
        Just skSm -> Just <$> deserializeSecretKey proxy skSm
    return $ newEnv derive skR mskS

----------------------------------------------------------------

ecdh'
    :: EllipticCurveDH group
    => Proxy group
    -> SecretKey group
    -> PublicKey group
    -> a
    -> Either a SharedSecret
ecdh' proxy sk pk err = case ecdh proxy sk pk of
    CryptoPassed a -> Right a
    CryptoFailed _ -> Left err
