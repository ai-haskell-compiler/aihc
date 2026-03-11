
module Data.Radius.Implements (
  signPacket, signedPacket,

  AuthenticatorError (..),

  checkSignedRequest, checkSignedResponse,
  ) where

import Control.Monad (unless)
import Data.Monoid ((<>))
import Data.Word (Word16)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize.Put (Put, runPut)
import qualified Data.ByteArray as BA
import Crypto.Hash (Digest, hash, MD5)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)

import Data.Radius.Packet (Code (..), Header (..), Packet (..))
import Data.Radius.Scalar (AtString (..), Bin128, mayBin128, fromBin128, bin128Zero)
import Data.Radius.Attribute
  (Number (MessageAuthenticator), messageAuthenticator,
   NumberAbstract (Standard), Attribute' (Attribute'), TypedNumberSets, )
import Data.Radius.StreamGet (Attributes)
import qualified Data.Radius.StreamGet as Get
import qualified Data.Radius.StreamPut as Put


hmacMD5 :: ByteString -> ByteString -> Bin128
hmacMD5 rsk bs =
  maybe (error "hmacMD5: BUG? Invalid result length") id
  . mayBin128 . BA.convert $ hmacGetDigest (hmac rsk bs :: HMAC MD5)

md5 :: ByteString -> Bin128
md5 bs = maybe (error "md5: BUG? Invalid result length") id
         . mayBin128 $ BA.convert (hash bs :: Digest MD5)

-- | Make signatures for response packet.
--   When you don't want to use message authenticator attribute,
--   pass a function to make attributes which doesn't use message authenticator argument.
signPacket :: (a -> ByteString -> Put)     -- ^ Printer for vendor specific attribute
           -> ByteString                   -- ^ Radius secret key
           -> Bin128                       -- ^ Request authenticator
           -> (Word16 -> Bin128 -> Header) -- ^ Function to make header
           -> (Bin128 -> [Attribute' a])   -- ^ Function to make attributes from message authenticator
           -> (Word16, Bin128, Bin128)     -- ^ Packet length, message authenticator and response authenticator
signPacket va rsk auth mkH mkA = (len, msgAuth, respAuth)
  where
    asMsgAuth0 = mkA bin128Zero
    pput = runPut . Put.upacket va
    len = fromIntegral . BS.length . pput
          $ Packet { header = mkH 0 auth, attributes = asMsgAuth0 }
    msgAuth = hmacMD5 rsk . pput
              $ Packet { header = mkH len auth, attributes = asMsgAuth0 }
    respAuth = md5 $ (pput $ Packet { header = mkH len auth, attributes = mkA msgAuth }) <> rsk

signedPacket :: (a -> ByteString -> Put)     -- ^ Printer for vendor specific attribute
             -> ByteString                   -- ^ Radius secret key
             -> Bin128                       -- ^ Request authenticator
             -> (Word16 -> Bin128 -> Header) -- ^ Function to make header
             -> (Bin128 -> [Attribute' a])   -- ^ Function to make attributes from message authenticator
             -> Packet [Attribute' a]        -- ^ Signed packet
signedPacket va rsk auth mkH mkA = case code $ mkH len auth of
  AccessAccept     ->  response
  AccessReject     ->  response
  AccessChallenge  ->  response

  AccessRequest    ->  other
  Other _          ->  other

  where
    (len, msgAuth, respAuth) = signPacket va rsk auth mkH mkA
    response  =  Packet { header = mkH len respAuth, attributes = mkA msgAuth }
    other     =  Packet { header = mkH len auth    , attributes = mkA msgAuth }

data AuthenticatorError v
  = NoMessageAuthenticator (Attributes v) -- ^ No Message-Authenticator attribute

  | BadMessageAuthenticator               -- ^ Message-Authenticator attribute is not matched
  | MoreThanOneMessageAuthenticator       -- ^ More than one Message-Authenticator attribute pairs found

  | BadAuthenticator                      -- ^ Radius packet authenticator is not matched

  | AttributesDecodeError String          -- ^ Fail to decode attributes, attribute type error etc.

  | NotRequestPacket Code                 -- ^ Not request packet is passed to function to check request packet
  | NotResponsePacket Code                -- ^ Not response packet is passed to function to check response packet

instance Show (AuthenticatorError v) where
  show = d  where
    d (NoMessageAuthenticator _)        =  "no messageAuthenticator found"
    d  BadMessageAuthenticator          =  "bad messageAuthenticator"
    d  MoreThanOneMessageAuthenticator  =  "more than one messageAuthenticator found"
    d  BadAuthenticator                 =  "bad radius packet authenticator"
    d (AttributesDecodeError s)         =  "fail to decode attributes: " ++ s
    d (NotRequestPacket c)              =  "not request packet: code: " ++ show c
    d (NotResponsePacket c)             =  "not response packet: code: " ++ show c

checkSignedRequest :: (TypedNumberSets a, Ord a)
                   => (a -> ByteString -> Put)     -- ^ Printer for vendor specific attribute
                   -> ByteString
                   -> Packet [Attribute' a]
                   -> Either (AuthenticatorError a) (Attributes a)
checkSignedRequest va rsk upkt = case code $ header upkt of
  c@AccessAccept     ->  notRequestCode c
  c@AccessReject     ->  notRequestCode c
  c@AccessChallenge  ->  notRequestCode c

  AccessRequest      ->  check
  Other _            ->  check
  where
    notRequestCode = Left . NotRequestPacket

    check  =  checkMA calcMsgAuth $ attrs
    attrs  =  attributes upkt
    calcMsgAuth = hmacMD5 rsk . runPut . Put.upacket va
                  $ upkt { attributes = replace0MA attrs }

checkSignedResponse :: (TypedNumberSets a, Ord a)
                    => (a -> ByteString -> Put)     -- ^ Printer for vendor specific attribute
                    -> ByteString
                    -> Bin128
                    -> Packet [Attribute' a]
                    -> Either (AuthenticatorError a) (Attributes a)
checkSignedResponse va rsk reqAuth upkt = case code $ header upkt of
  AccessAccept     ->  check
  AccessReject     ->  check
  AccessChallenge  ->  check

  c@AccessRequest  ->  notResponseCode c
  c@(Other _)      ->  notResponseCode c
  where
    notResponseCode = Left . NotResponsePacket

    check  =  do
      unless (authenticator (header upkt) == calcRespAuth) $ Left BadAuthenticator
      checkMA calcMsgAuth attrs
    attrs  =  attributes upkt
    calcRespAuth = md5
                   $ (runPut . Put.upacket va $ upkt { header = (header upkt) { authenticator = reqAuth } }) <> rsk
    calcMsgAuth = hmacMD5 rsk . runPut . Put.upacket va
                  $ upkt { header = (header upkt) { authenticator = reqAuth }
                         , attributes = replace0MA attrs }


checkMA :: (TypedNumberSets a, Ord a)
        => Bin128 -> [Attribute' a] -> Either (AuthenticatorError a) (Attributes a)
checkMA calcMsgAuth attrs  = do
  ta  <-  either (Left . AttributesDecodeError) return . Get.extractAttributes $ mapM Get.tellT attrs
  case Get.takeTyped ta messageAuthenticator of
    []             ->  Left $ NoMessageAuthenticator ta
    [AtString bs]  ->  do
      unless (bs == fromBin128 calcMsgAuth) $ Left BadMessageAuthenticator
      return ta
    _:_:_          ->  Left MoreThanOneMessageAuthenticator

replace0MA :: [Attribute' a] -> [Attribute' a]
replace0MA = rec'  where
  rec'  []                                       =
    []
  rec' (Attribute' n@(Standard MessageAuthenticator) _ : xs)  =
    Attribute' n (fromBin128 bin128Zero) : rec' xs
  rec' (x                                              : xs)  =
    x : rec' xs
