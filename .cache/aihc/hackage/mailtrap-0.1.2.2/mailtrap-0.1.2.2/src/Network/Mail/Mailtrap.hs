----------------------------------------------------------------------------------------------------
-- | Mailtrap API.
module Network.Mail.Mailtrap
  ( -- * Tokens
    Token (..)
  , Exception (..)
    -- * Accounts
  , AccountID (..)
  , Account (..)
  , getAllAccounts
    -- * Attachments
  , Disposition (..)
  , setDisposition
  , Attachment (..)
  , attachmentFromFile
    -- * Templates
  , Template (..)
  , template
  , setTemplateVariable
    -- * Testing inboxes
  , InboxID (..)
  , Inbox (..)
  , getInboxes
  , InboxMessageID (..)
  , InboxMessage (..)
  , getInboxMessages
  , downloadMessageRaw
  , downloadMessageEML
  , downloadMessageText
  , downloadMessageHTML
    -- * Sending e-mails
  , EmailAddress
  , parseEmailAddress
  , NamedEmailAddress (..)
  , MessageID (..)
  , Message (..)
  , EmailBody (..)
  , Email (..)
  , sendEmail
  , sendTestEmail
    ) where

-- base
import Control.Exception qualified as Base
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.String (fromString)
-- text
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- email-validate
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate qualified as Email
-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
-- mime-types
import Network.Mime (MimeType, defaultMimeLookup)
-- aeson
import Data.Aeson (ToJSON, (.=), FromJSON, (.:))
import Data.Aeson qualified as JSON
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
#else
import Data.HashMap.Strict qualified as HashMap
#endif
-- base64
import Data.ByteString.Base64 (encodeBase64)
#if MIN_VERSION_base64(1,0,0)
import Data.Base64.Types (extractBase64)
#endif
-- blaze-html
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
-- uuid
import Data.UUID.Types (UUID)
-- http-conduit
import Network.HTTP.Simple qualified as HTTP
-- filepath
import System.FilePath (takeFileName)
-- time
import Data.Time.Clock (UTCTime)

-- | Authorization token.
newtype Token = Token Text deriving (Eq, Show)

instance FromJSON Token where
  parseJSON = JSON.withText "Mailtrap Token" (pure . Token)

instance ToJSON Token where
  toJSON (Token token) = JSON.toJSON token

-- | Exceptions thrown by functions from this module.
data Exception =
    -- | API request returned list of errors.
    --   HTTP status code and error messages.
    MultipleErrors Int [Text]
    -- | API request returned a single error message.
    --   HTTP status code and error message.
  | SingleError Int Text
    -- | Parsing failed.
    --   Input that failed to parse plus error message.
  | ParsingError ByteString String
    deriving Show

instance Base.Exception Exception

-- | Constructor of simple error.
singleError :: Int -> JSONResp "error" Text -> Exception
singleError code (JSONResp err) = SingleError code err

-- | Constructor of multiple errors.
multipleErrors :: Int -> JSONResp "errors" [Text] -> Exception
multipleErrors code (JSONResp errs) = MultipleErrors code errs

-- | JSON object wrapper to help with parsing HTTP response.
data JSONResp (k :: Symbol) a = JSONResp { fromJSONResp :: a }

instance (KnownSymbol k, FromJSON a) => FromJSON (JSONResp k a) where
  parseJSON =
    let k = fromString $ symbolVal $ Proxy @k
    in  JSON.withObject "JSONResp" $ fmap JSONResp . (.: k)

-- | Wrapper that provides a text-based 'FromJSON' instance.
newtype AsText a = AsText { asText :: a }

instance FromJSON a => FromJSON (AsText a) where
  parseJSON = JSON.withText "AsText" $
    either fail (pure . AsText) . JSON.eitherDecodeStrict . encodeUtf8

-- | Generic API query that returns JSON.
genericQuery
  :: (FromJSON err, ToJSON a, FromJSON b)
  => ByteString -- ^ HTTP method.
  -> ByteString -- ^ API URL.
  -> ByteString -- ^ HTTP path.
  -> Token -- ^ API token.
  -> (Int -> err -> Exception) -- ^ Error parsing.
  -> Maybe a -- ^ Body.
  -> IO b -- ^ Response.
genericQuery method url path (Token token) ferr mbody = do
  let req = HTTP.setRequestMethod method
          $ HTTP.setRequestHost url
          $ HTTP.setRequestPort 443
          $ HTTP.setRequestPath path
          $ HTTP.setRequestSecure True
          $ maybe id HTTP.setRequestBodyJSON mbody
          $ HTTP.addRequestHeader "Api-Token" (encodeUtf8 token)
          $ HTTP.defaultRequest
  resp <- HTTP.httpBS req
  let code = HTTP.getResponseStatusCode resp
      body = HTTP.getResponseBody resp
  case code of
    200 ->
      either (Base.throwIO . ParsingError body) pure $
        JSON.eitherDecodeStrict body
    _ ->
      either (Base.throwIO . ParsingError body) (Base.throwIO . ferr code) $
        JSON.eitherDecodeStrict body

-- | Helper to set an empty body when using 'genericQuery'.
noBody :: Maybe ()
noBody = Nothing

-- | Generic API query to download files.
genericDownload
  :: ByteString -- ^ API URL.
  -> ByteString -- ^ HTTP path.
  -> Token -- ^ API token.
  -> IO ByteString -- ^ Response.
genericDownload url path (Token token) = do
  let req = HTTP.setRequestMethod "GET"
          $ HTTP.setRequestHost url
          $ HTTP.setRequestPort 443
          $ HTTP.setRequestPath path
          $ HTTP.setRequestSecure True
          $ HTTP.addRequestHeader "Api-Token" (encodeUtf8 token)
          $ HTTP.defaultRequest
  resp <- HTTP.httpBS req
  let code = HTTP.getResponseStatusCode resp
      body = HTTP.getResponseBody resp
  case code of
    200 -> pure body
    404 -> Base.throwIO $ SingleError 404 "File not found."
    _ ->
      either (Base.throwIO . ParsingError body) (Base.throwIO . singleError code) $
        JSON.eitherDecodeStrict body

-- | Mailtrap account ID.
newtype AccountID = AccountID Int deriving (Eq, Show, FromJSON)

-- | Mailtrap account.
data Account = Account
  { account_id :: AccountID
  , account_name :: Text
    } deriving Show

instance FromJSON Account where
  parseJSON = JSON.withObject "Account" $ \o ->
    Account <$> o .: "id" <*> o .: "name"

-- | Get all the accounts the given token has access to.
getAllAccounts :: Token -> IO [Account]
getAllAccounts token = genericQuery "GET" "mailtrap.io" "/api/accounts" token singleError noBody

-- | 'EmailAddress' wrapper to provide 'ToJSON' and 'FromJSON' instances.
newtype EmailAddressJSON = EmailAddressJSON { fromEmailAddressJSON :: EmailAddress }

instance ToJSON EmailAddressJSON where
  toJSON = JSON.String . decodeUtf8 . Email.toByteString . fromEmailAddressJSON

instance FromJSON EmailAddressJSON where
  parseJSON = JSON.withText "EmailAddressJSON" $ \t ->
    either fail (pure . EmailAddressJSON) $ Email.validate $ encodeUtf8 t

-- | An e-mail address with a name.
data NamedEmailAddress = NamedEmailAddress
  { emailAddress :: EmailAddress
  , emailAddressName :: Text 
    } deriving Show

instance ToJSON NamedEmailAddress where
  toJSON addr = JSON.object
    [ "email" .= EmailAddressJSON (emailAddress addr)
    , "name" .= emailAddressName addr
      ]

-- | Attempt to parse an e-mail address
parseEmailAddress :: ByteString -> Either String EmailAddress
parseEmailAddress = Email.validate

-- | Attachment disposition.
data Disposition =
    -- | Inline with identifier.
    Inline Text
  | Attached
    deriving Show

-- | File that can be attached to an e-mail.
data Attachment = Attachment
  { -- | File name.
    attachment_name :: Text
    -- | MIME type of the content.
  , attachment_type :: MimeType
    -- | Attachment content.
  , attachment_content :: ByteString
    -- | Attachment disposition.
  , attachment_disposition :: Disposition
    } deriving Show

-- | Create an attachment from a file. It guesses the mime type from
--   the file extension. Disposition is set to 'Attached'.
--   The file is read strictly.
attachmentFromFile :: FilePath -> IO Attachment
attachmentFromFile fp = do
  let fptext :: Text
      fptext = Text.pack $ takeFileName fp
  bytes <- ByteString.readFile fp
  pure $ Attachment
    { attachment_name = fptext
    , attachment_type = defaultMimeLookup fptext
    , attachment_content = bytes
    , attachment_disposition = Attached
      }

-- | Set an attachment's disposition.
setDisposition :: Disposition -> Attachment -> Attachment
setDisposition d a = a { attachment_disposition = d }

instance ToJSON Attachment where
  toJSON att = JSON.object $
    [ "filename" .= attachment_name att
    , "type" .= decodeUtf8 (attachment_type att)
#if MIN_VERSION_base64(1,0,0)
    , "content" .= extractBase64 (encodeBase64 $ attachment_content att)
#else
    , "content" .= encodeBase64 (attachment_content att)
#endif
    , "disposition" .=
      (case attachment_disposition att of
         Inline _ -> "inline" :: Text
         Attached -> "attachment"
         )
      ] ++ (case attachment_disposition att of
              Inline i -> [ "content_id" .= i ]
              Attached -> []
              )

-- | An e-mail body.
data EmailBody =
    -- | Plain-text body.
    PlainTextBody Text
    -- | HTML-only body.
  | HTMLOnlyBody Html
    -- | HTML body with text fallback.
  | HTMLBody Html Text

-- | E-mail message, including subject and body.
data Message = Message
  { message_subject :: Text
  , message_body :: EmailBody
    -- | Message category.
  , message_category :: Text
    }

-- | Template that can be used when sending e-mails.
data Template = Template
  { -- | ID of the template.
    template_id :: UUID
    -- | Template variable assignments.
  , template_variables :: JSON.Object
    } deriving Show

-- | Template with no variable set.
template :: UUID -> Template
#if MIN_VERSION_aeson(2,0,0)
template i = Template i KeyMap.empty
#else
template i = Template i HashMap.empty
#endif

-- | Set template variable.
setTemplateVariable :: ToJSON a => Text -> a -> Template -> Template
setTemplateVariable k x t =
#if MIN_VERSION_aeson(2,0,0)
  t { template_variables = KeyMap.insert (Key.fromText k) (JSON.toJSON x) $ template_variables t }
#else
  t { template_variables = HashMap.insert k (JSON.toJSON x) $ template_variables t }
#endif

-- | E-mail that can be sent using 'sendEmail'.
data Email = Email
  { -- | Sender address.
    email_from :: NamedEmailAddress
    -- | Recipient list. Max 1000.
  , email_to :: [NamedEmailAddress]
    -- | Carbon Copy recipients.
  , email_cc :: [NamedEmailAddress]
    -- | Blind Carbon Copy recipients.
  , email_bcc :: [NamedEmailAddress]
    -- | Files attached to the e-mail.
  , email_attachments :: [Attachment]
    -- | Custom JSON object.
  , email_custom :: JSON.Object
    -- | Message to send.
  , email_message :: Either Template Message
    }

instance ToJSON Email where
  toJSON email = JSON.object $
    [ "from" .= email_from email
    , "to" .= email_to email
    , "cc" .= email_cc email
    , "bcc" .= email_bcc email
    , "attachments" .= email_attachments email
    , "custom_variables" .= email_custom email
      ] ++ (case email_message email of
              Left temp ->
                [ "template_uuid" .= template_id temp
                , "template_variables" .= template_variables temp
                  ]
              Right msg ->
                [ "subject" .= message_subject msg
                , "category" .= message_category msg
                  ] ++ (case message_body msg of
                          PlainTextBody t -> [ "text" .= t ]
                          HTMLOnlyBody h -> [ "html" .= renderHtml h ]
                          HTMLBody h t -> [ "html" .= renderHtml h, "text" .= t ]
                          )
              )

-- | Testing inbox identifier.
newtype InboxID = InboxID Int deriving (Eq, Show, FromJSON)

-- | Testing inbox.
data Inbox = Inbox
  { inbox_id :: InboxID
  , inbox_name :: Text
    -- | Number of emails in the inbox.
  , inbox_emailCount :: Int
    -- | Number of unread emails in the inbox.
  , inbox_unreadCount :: Int
  , inbox_maxSize :: Int
    } deriving Show

instance FromJSON Inbox where
  parseJSON = JSON.withObject "Inbox" $ \o -> Inbox
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "emails_count"
    <*> o .: "emails_unread_count"
    <*> o .: "max_size"

-- | Get all inboxes from an account.
getInboxes :: Token -> AccountID -> IO [Inbox]
getInboxes token (AccountID i) =
  let path = fromString $ "/api/accounts/" ++ show i ++ "/inboxes"
  in  genericQuery "GET" "mailtrap.io" path token singleError noBody

-- | Inbox message identifier.
newtype InboxMessageID = InboxMessageID Int deriving (Eq, Show, FromJSON)

-- | A message in a testing inbox.
data InboxMessage = InboxMessage
  { inboxMessage_id :: InboxMessageID
  , inboxMessage_inbox :: InboxID
  , inboxMessage_sentAt :: UTCTime
  , inboxMessage_from :: EmailAddress
  , inboxMessage_to :: EmailAddress
  , inboxMessage_subject :: Text
  , inboxMessage_isRead :: Bool
    } deriving Show

instance FromJSON InboxMessage where
  parseJSON = JSON.withObject "InboxMessage" $ \o -> InboxMessage
    <$> o .: "id"
    <*> o .: "inbox_id"
    <*> o .: "sent_at"
    <*> (fromEmailAddressJSON <$> o .: "from_email")
    <*> (fromEmailAddressJSON <$> o .: "to_email")
    <*> o .: "subject"
    <*> o .: "is_read"

-- | Get all inbox messages from an testing inbox.
getInboxMessages :: Token -> AccountID -> InboxID -> IO [InboxMessage]
getInboxMessages token (AccountID accid) (InboxID inboxid) =
  let path = fromString $ "/api/accounts/" ++ show accid
          ++ "/inboxes/" ++ show inboxid ++ "/messages"
  in  genericQuery "GET" "mailtrap.io" path token singleError noBody

-- | Generic function to implement all the message download functions in one place.
downloadMessageGeneric
  :: String -- ^ Extension
  -> Token -> AccountID -> InboxID -> InboxMessageID -> IO Text
downloadMessageGeneric ext token (AccountID accid) (InboxID inboxid) (InboxMessageID msgid) =
  let path = fromString $ "/api/accounts/" ++ show accid
          ++ "/inboxes/" ++ show inboxid
          ++ "/messages/" ++ show msgid
          ++ "/body." ++ ext
  in  decodeUtf8 <$> genericDownload "mailtrap.io" path token

-- | Download inbox message raw email body.
downloadMessageRaw :: Token -> AccountID -> InboxID -> InboxMessageID -> IO Text
downloadMessageRaw = downloadMessageGeneric "raw"

-- | Download inbox message in EML format.
downloadMessageEML :: Token -> AccountID -> InboxID -> InboxMessageID -> IO Text
downloadMessageEML = downloadMessageGeneric "eml"

-- | Download inbox message text part.
downloadMessageText :: Token -> AccountID -> InboxID -> InboxMessageID -> IO Text
downloadMessageText = downloadMessageGeneric "txt"

-- | Download inbox message HTML part.
downloadMessageHTML :: Token -> AccountID -> InboxID -> InboxMessageID -> IO Text
downloadMessageHTML = downloadMessageGeneric "html"

-- | Production message identifier.
newtype MessageID = MessageID UUID deriving (Eq, Show, FromJSON)

-- | Send an e-mail and return the list of IDs of the messages sent (one per recipient).
sendEmail :: Token -> Email -> IO [MessageID]
sendEmail = genericSendEmail Nothing

-- | Send a testing e-mail to the given inbox and return the list of IDs of the messages
--   sent (one per recipient).
sendTestEmail :: Token -> InboxID -> Email -> IO [InboxMessageID]
sendTestEmail token i = fmap (fmap asText) . genericSendEmail (Just i) token

-- | Unified implementation for sending testing and production e-mails.
genericSendEmail :: FromJSON a => Maybe InboxID -> Token -> Email -> IO [a]
genericSendEmail minbox token email =
  let url = case minbox of
              Nothing -> "send.api.mailtrap.io"
              _ -> "sandbox.api.mailtrap.io"
      path = case minbox of
               Just (InboxID i) -> fromString $ "/api/send/" ++ show i
               Nothing -> "/api/send"
  in  fromJSONResp @"message_ids" <$> genericQuery "POST" url path token multipleErrors (Just email)
