-- SPDX-FileCopyrightText: 2022 Frank Doepper
--
-- SPDX-License-Identifier: GPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Utils.Connection where

import qualified Data.ByteString            as B
import           Data.Default.Class
import qualified Data.Text                  as T
import           Network.AMQP
import           Network.AMQP.Utils.Helpers
import           Network.AMQP.Utils.Options
import qualified Network.Connection         as N
import           Network.TLS
import           Network.TLS.Extra
import           System.Timeout
import           System.X509

-- | opens a connection and a channel
connect :: Args -> IO (Connection, Channel)
connect args = do
  printparam "server" $ server args
  printparam "port" $ portnumber args
  printparam "vhost" $ vHost args
  printparam "connection_name" $ connectionName args
  printparam "heartbeat" $ liftA2 (\ n m -> show n ++ m) (heartBeat args) (Just " s")
  printparam "connect timeout" $ [show (connect_timeout args), "s"]
  globalCertificateStore <- getSystemCertificateStore
  let myTLS =
        N.TLSSettings
          (defaultParamsClient "" B.empty)
            { clientShared =
                def
                  { sharedValidationCache = def
                  , sharedCAStore = globalCertificateStore
                  }
            , clientSupported = def {supportedCiphers = ciphersuite_default}
            , clientHooks =
                def {onCertificateRequest = myCert (cert args) (key args)}
            }
  Just conn <-
    timeout to $
    openConnection''
      defaultConnectionOpts
        { coAuth =
            [ SASLMechanism "EXTERNAL" B.empty Nothing
            , plain (T.pack (user args)) (T.pack (pass args))
            ]
        , coVHost = T.pack $ vHost args
        , coTLSSettings =
            if (tls args)
              then Just (TLSCustom myTLS)
              else Nothing
        , coServers = [(server args, portnumber args)]
        , coHeartbeatDelay = heartBeat args
        , coName = fmap T.pack $ connectionName args
        }
  getServerProperties conn >>= return . (formatheaders fieldshow) >>=
    printparam "server properties"
  Just chan <- timeout to $ openChannel conn
  return (conn, chan)
  where
    to = connect_timeout args * 1000000

--  addChannelExceptionHandler chan
--                             (\exception -> closeConnection conn >>
--                                  printparam "exiting" (show exception) >>
--                                  killThread tid)
--
-- -- noop sharedValidationCache, handy when debugging
-- noValidation :: ValidationCache
-- noValidation = ValidationCache
--                  (\_ _ _ -> return ValidationCachePass)
--                  (\_ _ _ -> return ())
--
--
-- | provides the TLS client certificate
myCert :: Maybe FilePath -> Maybe FilePath -> t -> IO (Maybe Credential)
myCert (Just cert') (Just key') _ = do
  result <- credentialLoadX509 cert' key'
  case result of
    Left x  -> printparam "ERROR" x >> return Nothing
    Right x -> return $ Just x
myCert _ _ _ = return Nothing
