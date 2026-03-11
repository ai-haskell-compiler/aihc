{-# LANGUAGE OverloadedStrings #-}
module Network.SES
    ( -- * Email creation 
      sendEmailBlaze
    , sendEmail
      -- * Types
    , From
    , To
    , Subject
    , PublicKey    (..)
    , SecretKey    (..)
    , Region       (..)
    , SESErrorType (..)
    , SESError     (..)
    , SESResult    (..)
    , SESErrorCode
    , SESErrorMessage
    ) where

import           Crypto.Hash                   ( Digest, SHA256
                                               , hmac, hmacGetDigest
                                               )
import           Data.Byteable                 ( toBytes )
import           Data.ByteString               (ByteString)
import           Data.ByteString.Char8         ( pack, unpack, concat )
import qualified Data.ByteString.Lazy          as L
import           Data.ByteString.Base64        ( encode )
import           Data.Monoid                   ( (<>) )
import           Data.Maybe                    ( fromMaybe )
import           Data.Time.Clock               ( getCurrentTime )
import           Data.Time.Format              ( formatTime, defaultTimeLocale )
import           Network.Http.Client           ( buildRequest, http, Method(POST), setContentType
                                               , setHeader,baselineContextSSL, openConnectionSSL
                                               , Connection, sendRequest, encodedFormBody
                                               , receiveResponse, closeConnection, getStatusCode
                                               , concatHandler, getStatusCode
                                               )
import           OpenSSL                       ( withOpenSSL )
import           Prelude                hiding ( concat )
import           Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import           Text.Blaze.Html5              ( Html )
import           Text.Read                     ( readMaybe )
import           Control.Exception             ( try, SomeException )
import           Text.HTML.TagSoup             ( parseTags, Tag(..) )

------------------------------------------------------------------------------
-- | Types for Email creation
type ConnectionError a = IO (Either SomeException a)
type Subject = L.ByteString
type From    = L.ByteString
type To      = [L.ByteString]

------------------------------------------------------------------------------
-- | Types for AWS Keys
newtype PublicKey = PublicKey ByteString deriving (Show, Eq)
newtype SecretKey = SecretKey ByteString deriving (Show, Eq)

------------------------------------------------------------------------------
-- | The result of invoking an SES action
data SESResult = Error SESError -- ^ The encapsulated 'SESError'
               | Success        -- ^ A successful email has been sent
                 deriving Show

------------------------------------------------------------------------------
-- | Types for AWS 'Region's
data Region = USEast1  -- ^ US East 1 Region
            | USWest2  -- ^ US West 2 Region
            | EUWest1  -- ^ EU West 1 Region

------------------------------------------------------------------------------
-- | Custome 'Show' instance for 'Region'
instance Show Region where
    show USEast1 = "us-east-1"
    show USWest2 = "us-west-2"
    show EUWest1 = "eu-west-1"
------------------------------------------------------------------------------
-- | Send Emails templated with Blaze using SES
--
--  > main :: IO ()
--  > main = print =<< sendEmailBlaze publicKey secretKey region from to subject html
--  >  where
--  >    publicKey = PublicKey "public key goes here"
--  >    secretKey = SecretKey "secret key goes here"
--  >    region    = USEast1
--  >    from    = "verifiedSender@domain.com"
--  >    to      = ["recipient@domain.com"]
--  >    subject = "Test Subject"
--  >    html = H.html $ do
--  >             H.body $ do
--  >                H.h1 "Html email!"
--
sendEmailBlaze
    :: PublicKey -- ^ AWS Public Key
    -> SecretKey -- ^ AWS Secret Key
    -> Region    -- ^ The Region to send the Request
    -> From      -- ^ The Email sender
    -> To        -- ^ The Email recipient
    -> Subject   -- ^ The Subject of the Email
    -> Html      -- ^ The Html of the email body
    -> IO SESResult
sendEmailBlaze 
    publicKey
    secretKey
    region
    from
    to
    subject
    html = sendEmail publicKey secretKey region from to subject (renderHtml html)

------------------------------------------------------------------------------
-- | Send emails without using Blaze, raw bytes are expected to be valid HTML
sendEmail
    :: PublicKey    -- ^ AWS Public Key
    -> SecretKey    -- ^ AWS Secret Key
    -> Region       -- ^ The Region to send the Request
    -> From         -- ^ The Email sender
    -> To           -- ^ The Email recipient
    -> Subject      -- ^ The Subject of the Email
    -> L.ByteString -- ^ Raw Html 
    -> IO SESResult
sendEmail = makeRequest 

------------------------------------------------------------------------------
-- | Types to hold SES Errors
type SESErrorCode    = Int        -- ^ Error Code returned from SES XML response
type SESErrorMessage = ByteString -- ^ Error Message returned from SES Message
data SESError = 
          -- | Connection Error, can occur on open & close or on send & receive of a Request or Response
          SESConnectionError ByteString    
          -- | If a request is made successfully but the parameters specifed were incorrect
        | SESError SESErrorCode SESErrorType SESErrorMessage 
          deriving (Show)

------------------------------------------------------------------------------
-- | Common Error Types for SES 
-- 
-- <http://s3.amazonaws.com/awsdocs/ses/latest/ses-api.pdf>
--
data SESErrorType = 
          IncompleteSignature
        | InternalFailure
        | InvalidAction
        | InvalidClientTokenId
        | InvalidParameterCombination
        | InvalidParameterValue
        | InvalidQueryParameter
        | MalformedQueryString
        | MissingAction
        | MissingAuthenticationToken
        | SignatureDoesNotMatch
        | MissingParameter
        | MessageRejected
        | OptInRequired
        | RequestExpired
        | ServiceUnavailable
        | Throttling
        | UnknownErrorType
        | ValidationError
          deriving (Show, Read)

------------------------------------------------------------------------------
-- | SES Request Dispatcher
makeRequest 
    :: PublicKey    -- ^ AWS Public Key
    -> SecretKey    -- ^ AWS Secret Key
    -> Region       -- ^ The Region to send the Request
    -> From         -- ^ The Email sender
    -> To           -- ^ The Email recipient
    -> Subject      -- ^ The Subject of the Email
    -> L.ByteString -- ^ Raw Html 
    -> IO SESResult
makeRequest 
    (PublicKey publicKey)
    (SecretKey secretKey)
    region
    from
    to
    subject
    msg = withOpenSSL $ do
  now <- getCurrentTime
  let date = pack $ format now
      sig  = makeSig date secretKey
      format = formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z"
      auth = concat
            [ "AWS3-HTTPS AWSAccessKeyId="
            , publicKey
            , ", Algorithm=HmacSHA256, Signature="
            , sig
            ]
      queryString =
          ("Action", "SendEmail")
        : ("Source", L.toStrict from)
        : ("Message.Subject.Data", L.toStrict subject)
        : ("Message.Body.Html.Data", L.toStrict msg)
        : zipWith mkDest [1 :: Int ..] to
      mkDest num addr = (pack $ "Destination.ToAddresses.member." ++ show num, L.toStrict addr)
  req <- buildRequest $ do
          http POST "/"
          setContentType "application/x-www-form-urlencoded"
          setHeader "X-Amzn-Authorization" auth
          setHeader "Date" date
  ctx <- baselineContextSSL
  let 
  connResult <- try (openConnectionSSL ctx ("email." <> pack (show region) <> ".amazonaws.com") 443)
         :: ConnectionError Connection
  case connResult of
    Left s -> connectionError s
    Right con -> do
      result <- try (sendRequest con req $ encodedFormBody queryString) :: ConnectionError ()
      case result of
        Left s -> connectionError s
        Right _ -> do
            sesResult <- receiveResponse con $ \resp is -> 
                if getStatusCode resp == 200 
                    then returnSuccess
                    else do bs <- concatHandler resp is
                            let tags = parseTags bs 
                                code = let c = getFromTagSoup "Code" tags
                                        in fromMaybe UnknownErrorType (readMaybe (unpack c) :: Maybe SESErrorType)
                                sesMsg  = getFromTagSoup "Message" tags
                            return $ Error $ SESError (getStatusCode resp) code sesMsg
            closeConnection con
            return sesResult
  where
    getFromTagSoup x tags = let [ _, TagText d] = filterFront . filterBack $ tags 
                                filterFront = dropWhile (/=(TagOpen x [])) 
                                filterBack  = takeWhile (/=(TagClose x))
                            in d
    connectionError = return . Error . SESConnectionError . pack . show
    returnSuccess   = return Success

------------------------------------------------------------------------------
-- | Digital Signature creation
makeSig
    :: ByteString  -- ^ Payload
    -> ByteString  -- ^ Key
    -> ByteString
makeSig payload key =
    encode $ toBytes (hmacGetDigest $ hmac key payload :: Digest SHA256)

