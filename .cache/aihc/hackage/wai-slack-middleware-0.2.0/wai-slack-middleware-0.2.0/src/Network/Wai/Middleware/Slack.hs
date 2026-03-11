{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Slack
  (
   -- * Usage
   -- $usage

   -- * Integration with Yesod Scaffolding templates
   -- $yesod
   
   SlackConfig(..)
  , slack
  ) where

import Network.Wai
       (Middleware(..), Application(..), requestHeaders, Response, Request,
        responseStatus)
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Client hiding (responseStatus, Response, Request)
import Data.Aeson (encode, object, (.=))
import Data.Monoid ((<>))
import Control.Concurrent (forkIO)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>))
#endif

-- $usage
-- Settings are controlled via the type 'SlackConfig':
--
-- @
-- import Network.HTTP.Types.Status
-- let slackConfig = SlackConfig {
--     webHookUrl = "https://hooks.slack.com/services/xxx/xxxxxxxx",
--     httpManager = appHttpManager foundation,
--     responseFilter = \\resp -> status400 == responseStatus resp,
--     requestFilter = \\_ -> True
-- }
-- @
--
-- The above configuration will send slack notification for all 400 http status code.
--
-- $yesod
--
-- Go to Application.hs and change the 'makeApplication' function to something like this:
--
-- @
-- makeApplication :: App -> IO Application
-- makeApplication foundation = do
--   logWare <- makeLogWare foundation
--   let slackConfig = SlackConfig {
--                                webHookUrl = "https://hooks.slack.com/services/xxxx/xxxxxxx",
--                                httpManager = appHttpManager foundation,
--                                responseFilter = \\resp -> status400 == responseStatus resp,
--                                requestFilter = \\_ -> True
--                              }
--   -- Create the WAI application and apply middlewares
--   appPlain <- toWaiAppPlain foundation
--   return $ slack slackConfig $ logWare $ defaultMiddlewaresNoLogging appPlain
-- @

-- | Slack configuration for the middleware.
-- Slack notification will be emitted only if this evaluates to True:
-- 'responseFilter' && 'requestFilter'. This comes handy in situations
-- where you don't want to send notifications for certain 'Request's
-- even if 'responseFilter' results in 'True'.
data SlackConfig = SlackConfig
  { webHookUrl :: String -- ^ Slack webhook URL
  , httpManager :: Manager
  , responseFilter :: Response -> Bool -- ^ If 'True', check
                                       -- 'requestFilter' to see if
                                       -- slack notiication needs to
                                       -- be sent. On False, no slack
                                       -- notification is sent.
  , requestFilter :: Request -> Bool -- ^ If 'True', show slack
                                     -- notification. Else no slack
                                     -- notification is sent. Note
                                     -- that this condition is only
                                     -- checked if 'responseFilter' is
                                     -- 'True'.
  }

slackCall :: SlackConfig -> String -> IO ()
slackCall sconfig payload = do
  let url = webHookUrl sconfig
  req <- parseRequest url
  let reqObj = object ["text" .= payload]
  let req' =
        req
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode reqObj
        }
  _ <- forkIO $ httpLbs req' (httpManager sconfig) >> return ()
  return ()

emitNotification :: SlackConfig -> Response -> Request -> Bool
emitNotification sconfig resp req =
  (responseFilter sconfig $ resp) && (requestFilter sconfig $ req)


-- | Slack middleware for Wai. Use the `logStatus` to control on which
-- status you want to log the request information in Slack.
slack :: SlackConfig -> Middleware
slack sconfig (app :: Application) env sendResponse =
  app (env :: Request) $
  \res ->
     if emitNotification sconfig res env
       then slackCall
              sconfig
              ("Status " <> (show $ statusCode $ responseStatus res) <> " " <>
               show env) *> sendResponse res
       else sendResponse res
