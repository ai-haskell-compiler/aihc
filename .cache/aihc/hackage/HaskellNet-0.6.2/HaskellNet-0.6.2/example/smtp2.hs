{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

import           Network.HaskellNet.Auth
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Network.Mail.Mime
import Network.HaskellNet.Debug
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B

-- | Your settings
server       = "smtp.yandex.ru"
username     = "no-reply@cheops.olimpiada.ru"
password     = "Voo3gahm"
authType     = PLAIN
from         = Address (Just "A V") "no-reply@cheops.olimpiada.ru"
to           = Address (Just "V A") "alexander.vershilov@gmail.com"
subject      = "Network.HaskellNet.SMTP Test :)"
plainBody    = "Hello world!\r\n.\r\nsomething else"
htmlBody     = "<html><head></head><body><h1>Hello <i>world!</i></h1></body></html>"
attachments  = [] -- example [("application/octet-stream", "/path/to/file1.tar.gz), ("application/pdf", "/path/to/file2.pdf")]

main = do
  conn <- connectSMTPSSLWithSettings server defaultSettingsSMTPSSL{sslLogToConsole=True}
  authSucceed <- authenticate PLAIN (T.unpack username) (T.unpack password) conn
  when authSucceed $ do
    let newMail = Mail from [to] [] [] [("Subject", T.pack subject)] [[htmlPart htmlBody, plainPart plainBody]]
    newMail' <- addAttachments attachments newMail
    sendMail newMail' conn
  Network.HaskellNet.SMTP.SSL.closeSMTP conn

