{-# LANGUAGE DeriveDataTypeable #-}
module Crypto.Cipher.ChaChaPoly1305.Conduit
  ( encrypt
  , decrypt
  , ChaChaException (..)
  ) where

import           Control.Exception            (assert)
import           Control.Monad.Catch          (Exception, MonadThrow, throwM)
import qualified Crypto.Cipher.ChaChaPoly1305 as Cha
import qualified Crypto.Error                 as CE
import qualified Crypto.MAC.Poly1305          as Poly1305
import qualified Data.ByteArray               as BA
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 (ConduitM, await, leftover, yield)
import qualified Data.Conduit.Binary          as CB
import           Data.Typeable                (Typeable)

cf :: MonadThrow m
   => (CE.CryptoError -> ChaChaException)
   -> CE.CryptoFailable a
   -> m a
cf _ (CE.CryptoPassed x) = return x
cf f (CE.CryptoFailed e) = throwM (f e)

data ChaChaException
  = EncryptNonceException !CE.CryptoError
  | EncryptKeyException !CE.CryptoError
  | DecryptNonceException !CE.CryptoError
  | DecryptKeyException !CE.CryptoError
  | MismatchedAuth
  deriving (Show, Typeable)
instance Exception ChaChaException

encrypt
  :: MonadThrow m
  => ByteString -- ^ nonce (12 random bytes)
  -> ByteString -- ^ symmetric key (32 bytes)
  -> ConduitM ByteString ByteString m ()
encrypt nonceBS key = do
  nonce <- cf EncryptNonceException $ Cha.nonce12 nonceBS
  state0 <- cf EncryptKeyException $ Cha.initialize key nonce
  yield nonceBS
  let loop state1 = do
        mbs <- await
        case mbs of
          Nothing -> yield $ BA.convert $ Cha.finalize state1
          Just bs -> do
            let (bs', state2) = Cha.encrypt bs state1
            yield bs'
            loop state2
  loop $ Cha.finalizeAAD state0

decrypt
  :: MonadThrow m
  => ByteString -- ^ symmetric key (32 bytes)
  -> ConduitM ByteString ByteString m ()
decrypt key = do
  nonceBS <- CB.take 12
  nonce <- cf DecryptNonceException $ Cha.nonce12 $ BL.toStrict nonceBS
  state0 <- cf DecryptKeyException $ Cha.initialize key nonce
  let loop state1 = do
        ebs <- awaitExcept16 id
        case ebs of
          Left final ->
            case Poly1305.authTag final of
              CE.CryptoPassed final' | Cha.finalize state1 == final' -> return ()
              _ -> throwM MismatchedAuth
          Right bs -> do
            let (bs', state2) = Cha.decrypt bs state1
            yield bs'
            loop state2
  loop $ Cha.finalizeAAD state0
  where
    awaitExcept16 front = do
      mbs <- await
      case mbs of
        Nothing -> return $ Left $ front B.empty
        Just bs -> do
          let bs' = front bs
          if B.length bs' > 16
            then do
              let (x, y) = B.splitAt (B.length bs' - 16) bs'
              assert (B.length y == 16) leftover y
              return $ Right x
            else awaitExcept16 (B.append bs')
