-- | mail
module Main where

import           Network.Mail.Mime
import           Network.Mail.Pool
import           Options.Applicative

main :: IO ()
main = do
  smtp <- readSettings
  putStrLn "smtp settings:"
  print smtp
  settings <- smtpPool $ defSettings smtp
  sendEmail settings email
  where
    email =
      (emptyMail
         Address
           {addressName = Just "beterjappie", addressEmail = "beterjappie@raster.click"})
        { mailTo =
            [ Address
                { addressName = Just "jappie"
                , addressEmail = "jappieklooster@hotmail.com"
                }
            ]
        , mailHeaders =
            [ ("Subject", "geweldig raster -- Raster")
            , ("Reply-To", "superpwnzormegaman@gmail.com")
            ]
        , mailParts =
            [ [ plainPart
                  "Hello there, I'd like you to introduce you to rater.click. Click here for a quick demo. Or click here to unsubscribe."
              ]
            ]
        }

readSettings :: IO SmtpCred
readSettings =
  customExecParser (prefs showHelpOnError) $
  info
    (helper <*> emailOptions)
    (fullDesc <> Options.Applicative.header "Email" <> progDesc "Send an email")
