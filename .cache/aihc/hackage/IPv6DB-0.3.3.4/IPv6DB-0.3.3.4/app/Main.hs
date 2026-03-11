{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import           Control.Monad.Reader
import           Data.Aeson               as A
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe               (fromJust)
import           Data.String              (fromString)
import qualified Data.Vector              as V
import           Database.Redis           hiding (String)
import           Network.HTTP.Types       hiding (noContent204)
import           Network.IPv6DB.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Options.Applicative      (execParser)
import           Prelude                  hiding (error)
import           System.Log.FastLogger
import           Text.IPv6Addr

import           Options
import           Queries
import           Types

main :: IO ()
main = do
  Options{..} <- execParser opts
  timeCache <- newTimeCache simpleTimeFormat
  apLog <- apacheLogger <$>
             initLogger
               FromSocket
               (LogFileNoRotate logFile defaultBufSize)
               timeCache
  runSettings
    (setPort appPort $ setHost (fromString appHost) defaultSettings)
    (ipv6db apLog)

ipv6db :: ApacheLogger -> Application
ipv6db logger req res = do
  Options{..} <- execParser opts
  conn <- checkedConnect $
    defaultConnectInfo
      { connectHost     = redisHost
      , connectPort     = PortNumber (fromInteger redisPort)
      , connectAuth     = redisAuth
      , connectDatabase = redisDatabase }
  withEnv Env { redisConn = conn } $
    case parseMethod (requestMethod req) of
      Right mtd ->
        case pathInfo req of

          ["ipv6db", "v1", "batch"] ->
            batchHandler mtd

          ["ipv6db", "v1", "list", list, "addresses"] ->
            listHandler mtd list

          ["ipv6db", "v1", "list", list, "addresses", addr] ->
            listAddressHandler mtd list addr

          _ -> liftIO (jsonError "Bad URI Request")
      Left _ -> liftIO (jsonError "Bad HTTP Method")
    where

      withEnv = flip runReaderT

      maybeJSONBody :: FromJSON a => IO (Maybe a)
      maybeJSONBody = A.decode <$> strictRequestBody req

      logWith status = liftIO (logger req status Nothing)

      -- -------------------------------------------------------------------- --
      -- Endpoint handlers                                                    --
      -- -------------------------------------------------------------------- --

      batchHandler mtd = do
        env@Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST ->
            maybeJSONBody >>= maybe badJSONRequest answer
            where
              answer (Resources rsrcs) = do
                  results <- mapM (setSource redisConn mtd') rsrcs
                  if all (== RedisOk) results
                    then noContent204
                    else jsonRes400 (encode results)

          GET -> maybeJSONBody >>= maybe badJSONRequest answer
                 where
                   answer ents =
                     runRedis redisConn (getByEntries ents) >>= \case
                       Right srcs -> withEnv env (fromEntries ents srcs) >>= jsonOk
                       Left _     -> jsonError "Backend Error"

          DELETE -> maybeJSONBody >>= maybe badJSONRequest answer
                    where
                      answer ents =
                        runRedis redisConn (delByEntries ents) >>= \case
                          Right d ->
                            case d of
                              0 -> jsonRes404 (justError "No Deletion Performed")
                              _ -> noContent204
                          Left _  -> jsonError "Backend Error"

          _      -> methodNotAllowed

      listHandler mtd list = do
        env@Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST ->
            maybeJSONBody >>= maybe badJSONRequest answer
            where
              answer (Array v) = do
                let rsrcs =
                      (\o -> maybeResource o [("list",String list)]) <$> V.toList v
                if Nothing `notElem` rsrcs
                  then do
                    results <- mapM (setSource redisConn mtd' . fromJust) rsrcs
                    if all (== RedisOk) results
                      then noContent204
                      else jsonRes400 (encode $ filter (/= RedisOk) results)
                  else badJSONRequest

          GET -> maybeJSONBody >>= maybe badJSONRequest answer
                 where
                   answer addrs =
                     runRedis redisConn (getByAddresses list addrs) >>= \case
                       Right msrcs ->
                         withEnv env (fromAddresses list addrs msrcs) >>= jsonOk
                       Left  _     -> jsonServerError "Backend Error"

          DELETE ->
            maybeJSONBody >>= maybe badJSONRequest answer
            where
              answer addrs =
                runRedis redisConn (delByAddresses list addrs) >>= \case
                  Right d ->
                    case d of
                      0 -> jsonRes404 (justError "Resource To Delete Not Found")
                      _ -> noContent204
                  Left _  -> jsonServerError "Backend Error"

          _      -> methodNotAllowed

      listAddressHandler mtd list addr = do
        Env{..} <- ask
        liftIO $ case mtd of

          mtd' | mtd' == PUT || mtd' == POST ->
            maybeJSONBody >>= maybe badJSONRequest answer
            where
              answer o =
                case maybeResource o [("list", String list),("address", String addr)] of
                  Just rsrc ->
                    setSource redisConn mtd' rsrc >>= \case
                      RedisOk -> noContent204
                      error   -> jsonRes400 (encode error)
                  Nothing   -> jsonRes400 (justError "Bad JSON Request")

          GET ->
            case maybeIPv6Addr addr of
              Just (IPv6Addr addr') ->
                liftIO (runRedis redisConn $ getSource list addr') >>= \case
                  Right msrc ->
                    case msrc of
                      Just src -> do
                        ttls <- ttlSource redisConn list addr'
                        case toResource list addr' ttls src of
                          Just rsrc -> jsonOk (A.encode rsrc)
                          Nothing   -> jsonError "Can't Build Resource"
                      Nothing  ->
                        jsonRes404 $
                          encode $
                            ResourceError
                              list
                              (IPv6Addr addr')
                              "Resource Not Found"
                  Left _     -> jsonError "Backend Error"
              Nothing -> jsonError "Not IPv6 Address in URI"

          DELETE ->
            case maybeIPv6Addr addr of
              Just (IPv6Addr addr') ->
                liftIO (runRedis redisConn $ delSource list addr') >>= \case
                  Right i ->
                    case i of
                      1 -> noContent204
                      _ ->
                        jsonRes404 $
                          encode $
                            ResourceError
                              list
                              (IPv6Addr addr')
                              "The Resource Doesn't Exist"
                  Left _ -> jsonError "Backend Error"
              Nothing -> jsonError "Not an IPv6 Address in URI"

          _      -> methodNotAllowed

      -- -------------------------------------------------------------------- --
      -- JSON Responses                                                       --
      -- -------------------------------------------------------------------- --

      jsonOk bs = logWith status200 >> res (jsonRes status200 bs)

      noContent204 = logWith status204 >> res (responseLBS status204 [] BSL.empty)

      jsonRes400 bs = logWith status400 >> res (jsonRes status400 bs)

      badJSONRequest = logWith status400 >> jsonError "Bad JSON Request"

      jsonError err = logWith status400 >> res (jsonRes status400 $ justError err)

      jsonRes404 bs = logWith status404 >> res (jsonRes status404 bs)

      methodNotAllowed = logWith status405 >> res (jsonRes status405 $ justError "Method Not Allowed")

      jsonServerError err = logWith status500 >> res (jsonRes status500 $ justError err)

      jsonRes status =
        responseLBS
          status
          [ ("Content-Type", "application/json; charset=utf-8") ]

      justError err = "{\"error\":\"" <> err <> "\"}"

