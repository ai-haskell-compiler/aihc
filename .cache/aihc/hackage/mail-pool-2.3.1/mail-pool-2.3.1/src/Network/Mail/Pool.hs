{-# LANGUAGE DeriveAnyClass #-}

-- | SMTP is s an incredibly stable and well supported protocol.
--   Using this rather then API's prevents vendorlocking.
--
--   This module provides a ready to go connection pool for SMTP.
--   Which has been used in various deployments already.
module Network.Mail.Pool
  (
    sendEmail
  , smtpPool
  , defSettings
  , SmtpCred(..)
  , PoolSettings(..)
  -- ** specify connection type
  , openTls
  , openPlain
  , openTls'
  , createPoolConfig
  -- ** optparse applicative
  , emailOptions
  -- ** lenses
  , poolCred
  , poolConnf
  , poolUnused
  , poolStripeMax
  , smtpHost
  , smtpLogin
  , smtpPassword
  , smtpPort
  -- * Exceptions
  , ServiceAuthFailure
  -- * re exports
  , module X
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Pool                   as X
import           Lens.Micro
import           Network.HaskellNet.SMTP     as X
import           Network.HaskellNet.SMTP.SSL as X
import           Network.Mail.Mime
import           Network.Socket
import           Options.Applicative
import           Type.Reflection             (Typeable)
import GHC.Conc(numCapabilities)

-- | Failed to authetnicate with some upstream service (smtp for example)
newtype ServiceAuthFailure a = ServiceAuthFailure a
  deriving (Typeable, Show)
  deriving anyclass Exception

-- | Authentication information for the SMTP connection
data SmtpCred = SmtpCred
  { _smtpPassword :: String
  , _smtpLogin    :: String
  , _smtpHost     :: String
  , _smtpPort     :: PortNumber
  } deriving (Show)

instance FromJSON SmtpCred where
     parseJSON = withObject "SmtpCred" $ \v -> SmtpCred
        <$> v .: "password"
        <*> v .: "login"
        <*> v .: "host"
        <*> (fromInteger <$> v .: "port")

smtpHost :: Lens' SmtpCred String
smtpHost = lens _smtpHost (\a b -> a{_smtpHost= b})
smtpLogin :: Lens' SmtpCred String
smtpLogin = lens _smtpLogin (\a b -> a{_smtpLogin= b})
smtpPassword :: Lens' SmtpCred String
smtpPassword = lens _smtpPassword (\a b -> a{_smtpPassword= b})
smtpPort :: Lens' SmtpCred PortNumber
smtpPort = lens _smtpPort (\a b -> a{_smtpPort= b})

-- | This allows you to override the default settings from 'defSettings'
data PoolSettings = PoolSettings

  { -- | credentials for smtp connection
    _poolCred      :: SmtpCred
   -- | allows overriding of the opening function, for example 'openPlain' or 'openTls'
  , _poolConnf     :: SmtpCred -> IO SMTPConnection
   -- | specify how long connections are kept open.
  , _poolUnused    :: Double
   -- | how many connections per stripe.
  , _poolStripeMax :: Int
  }

poolCred      :: Lens' PoolSettings SmtpCred
poolCred      = lens _poolCred (\a b -> a{_poolCred=b})
poolConnf     :: Lens' PoolSettings (SmtpCred -> IO SMTPConnection)
poolConnf      = lens _poolConnf (\a b -> a{_poolConnf=b})
poolUnused    :: Lens' PoolSettings Double
poolUnused      = lens _poolUnused (\a b -> a{_poolUnused=b})
poolStripeMax :: Lens' PoolSettings Int
poolStripeMax      = lens _poolStripeMax (\a b -> a{_poolStripeMax=b})

-- | Create settings with good defaults from 'SmtpCred'.
defSettings :: SmtpCred -> PoolSettings
defSettings cred = PoolSettings
  { _poolCred = cred
  , _poolConnf = openTls
  , _poolUnused = 60
  , _poolStripeMax = numCapabilities
  }

openPlain :: SmtpCred -> IO SMTPConnection
openPlain smtp = connectSMTPPort (smtp ^. smtpHost) (smtp ^. smtpPort)

openTls :: SmtpCred -> IO SMTPConnection
openTls = openTls' defaultSettingsSMTPSTARTTLS

openTls' :: Settings -> SmtpCred -> IO SMTPConnection
openTls' def smtp = connectSMTPSTARTTLSWithSettings (smtp ^. smtpHost) $ def {
    sslPort = smtp ^. smtpPort
  }


-- | Construct a connection pool from settings.
smtpPool :: PoolSettings -> IO (Pool SMTPConnection)
smtpPool =
  newPool . createPoolConfig

-- | allows manipulation of the underlying resourcepool config
createPoolConfig :: PoolSettings -> PoolConfig SMTPConnection
createPoolConfig smtp =
    defaultPoolConfig
      (do
        conn <- smtp ^. poolConnf $ smtp ^. poolCred
        authorize conn (smtp ^. poolCred)
        pure conn
      )
      closeSMTP
      (smtp ^. poolUnused)
      (smtp ^. poolStripeMax )

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

-- | we need to auth only once per connection.
--   this is annoying because we want to crash on failure to auth.
authorize :: SMTPConnection -> SmtpCred -> IO ()
authorize conn smtp = do
  handleAny
    (\ex -> do
       closeSMTP conn -- don't leak
       throwIO ex) $ do
    isSuccess <-
      authenticate LOGIN (smtp ^. smtpLogin) (smtp ^. smtpPassword) conn
    if isSuccess
      then pure ()
      else throwIO $
           ServiceAuthFailure $
           smtpPassword .~ "obfuscated, see the running instance CLI" $ smtp

emailOptions :: Parser SmtpCred
emailOptions =
  SmtpCred <$>
  strOption
    (long "smtp-pass" <> metavar "SMTP-PASS" <>
     help
       "the smtp password, in case of mailjet: https://app.mailjet.com/transactional/smtp") <*>
  strOption
    (long "smtp-login" <> metavar "SMTP-LOGIN" <>
     help
       "the smtp login name, in case of mailjet: https://app.mailjet.com/transactional/smtp") <*>
  strOption
    (long "smtp-host" <> metavar "SMTP-HOST" <> value "in-v3.mailjet.com" <>
     showDefault <>
     help "the smtp host, excluding port") <*>
  option
    auto
    (long "smtp-port" <> help "The port on which the smtp server listens" <>
     showDefault <>
     value 587 <>
     metavar "SMTP-PORT")

-- | Send a 'Mail' with help of a connection pool.
sendEmail :: MonadIO m => Pool SMTPConnection -> Mail -> m ()
sendEmail pool = liftIO . withResource pool . sendMail
