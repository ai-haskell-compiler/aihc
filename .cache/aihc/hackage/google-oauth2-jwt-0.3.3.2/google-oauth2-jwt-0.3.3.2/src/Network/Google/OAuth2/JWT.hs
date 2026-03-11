-- | Create a signed JWT needed to make the access token request
-- to gain access to Google APIs for server to server applications.
--
-- For all usage details, see https://developers.google.com/identity/protocols/OAuth2ServiceAccount
--

module Network.Google.OAuth2.JWT
    (
       SignedJWT
    ,  Email
    ,  Scope
    ,  getSignedJWT

    -- * Utils
    , fromPEMString
    , fromPEMFile

    ) where

import           Codec.Crypto.RSA.Pure
import           Control.Monad              (unless)
import qualified Data.ByteString            as B
import           Data.ByteString.Base64.URL (encode)
import           Data.ByteString.Char8      (unpack)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.UnixTime              (getUnixTime, utSeconds)
import           Foreign.C.Types
import           OpenSSL.EVP.PKey           (toKeyPair)
import           OpenSSL.PEM                (PemPasswordSupply (PwNone),
                                             readPrivateKey)
import           OpenSSL.RSA

newtype SignedJWT =
  SignedJWT B.ByteString
  deriving (Eq)

instance Show SignedJWT where
  show (SignedJWT t) = unpack t

type Email = T.Text

type Scope = T.Text

-- | Get the private key obtained from the
-- Google API Console from a PEM file.
fromPEMFile :: FilePath -> IO PrivateKey
fromPEMFile f = readFile f >>= fromPEMString

-- | Get the private key obtained from the
-- Google API Console from a PEM 'String'.
--
-- >fromPEMString "-----BEGIN PRIVATE KEY-----\nB9e [...] bMdF\n-----END PRIVATE KEY-----\n"
-- >
fromPEMString :: String -> IO PrivateKey
fromPEMString s =
  readPrivateKey s PwNone >>= (
    \k -> return
      PrivateKey
        { private_pub =
            PublicKey
              { public_size = rsaSize k
              , public_n    = rsaN k
              , public_e    = rsaE k
              }
        , private_d    = rsaD k
        , private_p    = rsaP k
        , private_q    = rsaQ k
        , private_dP   = 0
        , private_dQ   = 0
        , private_qinv = 0
        }) . fromJust . toKeyPair

-- | Create the signed JWT ready for transmission
-- in the access token request as assertion value.
--
-- >grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=
--
getSignedJWT
  :: Email
  -- ^ The email address of the service account.
  -> Maybe Email
  -- ^ The email address of the user for which the
  -- application is requesting delegated access.
  -> [Scope]
  -- ^ The list of the permissions that the application requests.
  -> Maybe Int
  -- ^ Expiration time (maximun and default value is an hour, 3600 seconds).
  -> PrivateKey
  -- ^ The private key gotten from the PEM string obtained from the
  -- Google API Console.
  -> IO (Either String SignedJWT)
  -- ^ Either an error message or a signed JWT.
getSignedJWT iss msub scs mxt pk =
  let toT = T.pack . show
      toB64 = encode . encodeUtf8
      header = toB64 "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"
  in do
    let xt = fromIntegral (fromMaybe 3600 mxt)
    unless (xt >= 1 && xt <= 3600) (fail "Bad expiration time")
    t <- getUnixTime
    let i = header <> "." <> toB64 ("{\"iss\":\"" <> iss <> "\","
            <> maybe T.empty (\e -> "\"sub\":\"" <> e <> "\",") msub
            <> "\"scope\":\"" <> T.intercalate " " scs <> "\",\"aud\
            \\":\"https://www.googleapis.com/oauth2/v4/token\",\"ex\
            \p\":" <> toT (utSeconds t + CTime xt) <> ",\"iat\":"
            <> toT (utSeconds t) <> "}")
    return $
      either
        (pure $ Left "RSAError")
        (\s -> pure $ SignedJWT $ i <> "." <> encode (toStrict s))
        (rsassa_pkcs1_v1_5_sign hashSHA256 pk $ fromStrict i)

