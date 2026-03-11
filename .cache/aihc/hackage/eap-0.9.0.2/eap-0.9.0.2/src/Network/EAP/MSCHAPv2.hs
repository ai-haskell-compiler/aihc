{-# LANGUAGE NamedFieldPuns, RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Network.EAP.MSCHAPv2
Description : MSCHAPv2 EAP authentication method and related utility functions
Copyright   : (c) Erick Gonzalez, 2017
License     : BSD3
Maintainer  : erick@codemonkeylabs.de
Stability   : experimental
Portability : POSIX

This module provides functions to implement the EAP MSCHAPv2 authentication scheme.

-}
module Network.EAP.MSCHAPv2 (authenticateMSCHAPv2,
                             generateAuthenticatorResponse,
                             generateNTResponse,
                             ntPasswordHash,
                             deriveMPPEKeys) where

import Prelude hiding (concatMap, replicate, take)
import Data.Bits                  ((.|.), (.&.), complement, shiftL, shiftR, xor)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.ByteString.Builder    (toLazyByteString, byteStringHex)
import Data.ByteString.Lazy       (ByteString,
                                   append,
                                   cons,
                                   concatMap,
                                   fromStrict,
                                   replicate,
                                   toStrict)
import Data.ByteArray             (convert)
import Data.Char                  (toUpper)
import Control.Monad.Except       (ExceptT(..), Except, throwError)
import Control.Monad.State.Lazy   (State, execState, modify)
import Crypto.Cipher.DES          (DES)
import Crypto.Cipher.Types        (cipherInit, ecbEncrypt)
import Crypto.Hash.Algorithms     (MD4, SHA1(..))
import Crypto.Hash                (Context,
                                   Digest,
                                   hashFinalize,
                                   hashInitWith,
                                   hashUpdate,
                                   hashlazy)
import Crypto.Error               (CryptoError, CryptoFailable(..))
import Network.EAP.Types
import qualified Data.ByteString     as SB

-- | Authenticate the MSCHAPv2 response data to a given challenge request, using the supplied
-- cleartext password.
authenticateMSCHAPv2
    :: MSCHAPv2Data  -- ^ Decoded data from the MSCHAPv2 response
     -> ByteString    -- ^ Authenticator challenge sent to the peer on a previous request
     -> ByteString    -- ^ Authenticating user password NT hash (MD4)
     -> Except CryptoError Bool -- ^ Returns either an error from one of the encryption
                               -- routines or a boolean indicating whether the user
                               -- response matches the expected value
authenticateMSCHAPv2
  MSCHAPv2ResponseData{ getMSCHAPv2ResponseData = MSCHAPv2ResponseDataField{..}, .. }
  challenge
  passwordHash = do
  let peerChallenge = getMSCHAPv2ResponsePeerChallenge
      username      = getMSCHAPv2ResponseName
  r <- generateNTResponse challenge peerChallenge username passwordHash
  return $ r == toStrict getMSCHAPv2ResponseNTResponse

authenticateMSCHAPv2 msCHAPv2Data _ _ =
  error $ "Invalid authentication attempt of " ++ show msCHAPv2Data

-- | Calculate the NT Response as per [RFC2759], Section 8.1
generateNTResponse :: ByteString    -- ^ Authenticator challenge sent to the peer on a previous
                                    -- request
                   ->  ByteString    -- ^ Challenge sent back by authenticating peer
                   ->  ByteString    -- ^ MSCHAP username
                   ->  ByteString    -- ^ NT hash (MD4) of user password
                   ->  Except CryptoError SB.ByteString -- ^ Returns either an error from one of
                                                       -- the encryption routines or the
                                                       -- calculated NT response
generateNTResponse authenticatorChallenge peerChallenge username passwordHash = do
    let challenge        = challengeHash peerChallenge authenticatorChallenge username
        zPasswordHash    = (toStrict passwordHash) `SB.append` SB.replicate 5 0 -- pad to 21 octets
        (pHash0, rest)   = SB.splitAt 7 zPasswordHash
        (pHash1, pHash2) = SB.splitAt 7 rest
    r0 <- encryptDES pHash0 challenge
    r1 <- encryptDES pHash1 challenge
    r2 <- encryptDES pHash2 challenge
    return $ r0 `SB.append` r1 `SB.append` r2

-- | Calculate authenticator response as per [RFC2759], Section 8.7
generateAuthenticatorResponse :: ByteString -- ^ Username
                               -> ByteString -- ^ NT password hash 
                               -> ByteString -- ^ NT Response
                               -> ByteString -- ^ Authenticator challenge
                               -> ByteString -- ^ Peer challenge
                               -> Except CryptoError ByteString -- ^ Returns either an error from
                                                               -- one of the crypto routines or
                                                               -- upon success, a 42 byte
                                                               -- authenticator response
generateAuthenticatorResponse username passwordHash ntResponse authChallenge peerChallenge = do
  let passwordHashHash = md4Hash passwordHash
      digest1          = sha1Hash $ do
                           hash . fromStrict $ passwordHashHash
                           hash ntResponse
                           hash magic1
      challenge        = challengeHash peerChallenge authChallenge username
      digest2          = sha1Hash $ do
                           hash . fromStrict $ digest1
                           hash . fromStrict $ challenge
                           hash magic2
  return $ "S=" `append` (pack . fmap toUpper . unpack . toLazyByteString $ byteStringHex digest2)
  where magic1 = "Magic server to client signing constant"   -- seriously Microsoft?
        magic2 = "Pad to make it do more than one iteration"

-- | Derive Microsoft Point-to-Point Encryption (MPPE) keys see [RFC3079]
deriveMPPEKeys :: ByteString -- ^ NT Password hash 
               -> ByteString -- ^ NT Response
               -> Except CryptoError (SB.ByteString, SB.ByteString) -- ^ (MPPE send key,
                                                                    --    MPPE recv key)
deriveMPPEKeys ntHash ntResponse = do
  let ntHashHash = md4Hash ntHash
      masterKey  = fromStrict . SB.take 16 . sha1Hash $ do
                    hash . fromStrict $ ntHashHash
                    hash ntResponse
                    hash magic1
      assymetricStartKey magic = SB.take 16 . sha1Hash $ do
                                   hash masterKey
                                   hash shsPad1
                                   hash magic
                                   hash shsPad2
      recvKey = assymetricStartKey magic2
      sendKey = assymetricStartKey magic3
  return (sendKey, recvKey)
      where magic1 = "This is the MPPE Master Key"
            magic2 = "On the client side, this is the send key; on the server side, it is the receive key."
            magic3 = "On the client side, this is the receive key; on the server side, it is the send key."
            shsPad1 = replicate 40 0
            shsPad2 = replicate 40 0xf2

-- | Used internally to derive 8 octet challenge hash
challengeHash :: ByteString -- ^ Peer challenge
               -> ByteString -- ^ Authenticator challenge
               -> ByteString -- ^ Username
               -> SB.ByteString -- ^ Resulting SHA1 hash
challengeHash peerChallenge authenticatorChallenge username =
  SB.take 8 . sha1Hash $ do
    hash peerChallenge
    hash authenticatorChallenge
    hash username

-- | Hash an NT ascii plain-text. Password with MD4. Note that this function converts then
-- password internally to Unicode, so feeding a Unicode password to it will *not* work
ntPasswordHash :: ByteString -> SB.ByteString
ntPasswordHash = md4Hash . concatMap with0s
    where with0s = flip cons "\NUL"

-- | Used internally to calculate an MD4 hash.
md4Hash :: ByteString -> SB.ByteString
md4Hash = convert . (hashlazy :: ByteString -> Digest MD4)

-- | Used internally to calculate a hash digest incrementally
sha1Hash :: State (Context SHA1) () -> SB.ByteString
sha1Hash = convert . hashFinalize . flip execState ctx0
    where ctx0 = hashInitWith SHA1

-- | Used internally to update an incremental hash function in the state monad
hash :: ByteString -> State (Context SHA1) ()
hash = modify . flip hashUpdate . toStrict

-- | Used internally to encrypt a message using a DES cipher in ECB mode
encryptDES :: SB.ByteString -> SB.ByteString -> Except CryptoError SB.ByteString
encryptDES key msg = do
  (cipher :: DES) <- ExceptT . return $ initCipher
  ExceptT . return . Right $ ecbEncrypt cipher msg
    where initCipher = case cipherInit $ addParity key of
                         CryptoFailed e -> throwError e
                         CryptoPassed c -> Right c

-- | Used internally to add the parity bits to a 56 bit (7 octet key) thus becoming an 8
-- octet key
addParity :: SB.ByteString -> SB.ByteString
addParity = expand . SB.foldl f ((0, 0), SB.empty)
    where f ((i, carry), acc) word =
              let v      = carry  .|. (word `shiftR` i)
                  carry' = word `shiftL` (7 - i)
                  v'     = v .&. 0xfe
                  v''    = v' .|. (complement $ parity v') .&. 1
                  acc'   = acc `SB.snoc` v''
              in ((i+1, carry'), acc')
          expand ((_, carry), str) = str `SB.snoc` carry
          parity x0 = foldl (\x i -> x `xor` (x `shiftR` i)) x0 [1, 2, 4, 8, 16] .&. 1
