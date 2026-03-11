{-# LANGUAGE CPP #-}
module Crypto.PubKey.ECIES.Conduit
  ( encrypt
  , decrypt
  ) where

import           Control.Monad.Catch                  (MonadThrow, throwM)
import           Control.Monad.Trans.Class            (lift)
import qualified Crypto.Cipher.ChaCha                 as ChaCha
import qualified Crypto.Cipher.ChaChaPoly1305.Conduit as ChaCha
import qualified Crypto.ECC                           as ECC
import qualified Crypto.Error                         as CE
import           Crypto.Hash                          (SHA512 (..), hashWith)
import           Crypto.PubKey.ECIES                  (deriveDecrypt,
                                                       deriveEncrypt)
import           Crypto.Random                        (MonadRandom)
import qualified Data.ByteArray                       as BA
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as BL
import           Data.Conduit                         (ConduitM, yield)
import qualified Data.Conduit.Binary                  as CB
import           Data.Proxy                           (Proxy (..))
import           System.IO.Unsafe                     (unsafePerformIO)

getNonceKey :: ECC.SharedSecret -> (ByteString, ByteString)
getNonceKey shared =
  let state1 = ChaCha.initializeSimple $ B.take 40 $ BA.convert $ hashWith SHA512 shared
      (nonce, state2) = ChaCha.generateSimple state1 12
      (key, _) = ChaCha.generateSimple state2 32
   in (nonce, key)

type Curve = ECC.Curve_P256R1

proxy :: Proxy Curve
proxy = Proxy

pointBinarySize :: Int
pointBinarySize = B.length $ ECC.encodePoint proxy point
  where
    point = unsafePerformIO (ECC.keypairGetPublic <$> ECC.curveGenerateKeyPair proxy)
{-# NOINLINE pointBinarySize #-}

throwOnFail :: MonadThrow m => CE.CryptoFailable a -> m a
throwOnFail (CE.CryptoPassed a) = pure a
throwOnFail (CE.CryptoFailed e) = throwM e


encrypt
  :: (MonadThrow m, MonadRandom m)
  => ECC.Point Curve
  -> ConduitM ByteString ByteString m ()
encrypt point = do
  (point', shared) <- lift (deriveEncryptCompat proxy point) >>= throwOnFail
  let (nonce, key) = getNonceKey shared
  yield $ ECC.encodePoint proxy point'
  ChaCha.encrypt nonce key
  where
    deriveEncryptCompat prx p = deriveEncrypt prx p

decrypt
  :: (MonadThrow m)
  => ECC.Scalar Curve
  -> ConduitM ByteString ByteString m ()
decrypt scalar = do
  pointBS <- fmap BL.toStrict $ CB.take pointBinarySize
  point   <- throwOnFail (ECC.decodePoint proxy pointBS)
  shared  <- throwOnFail (deriveDecryptCompat proxy point scalar)
  let (_nonce, key) = getNonceKey shared
  ChaCha.decrypt key
  where
    deriveDecryptCompat prx p s = deriveDecrypt prx p s
