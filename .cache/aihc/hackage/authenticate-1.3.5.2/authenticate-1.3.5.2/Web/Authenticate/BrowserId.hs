{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Web.Authenticate.BrowserId
    ( browserIdJs
    , checkAssertion
    ) where

import Data.Text (Text)
import Network.HTTP.Conduit (parseUrlThrow, responseBody, httpLbs, Manager, method, urlEncodedBody)
#if MIN_VERSION_aeson(2,2,0)
import Data.Aeson (Value (Object, String))
import Data.Aeson.Parser (json)
#else
import Data.Aeson (json, Value (Object, String))
#endif
import Data.Attoparsec.Lazy (parse, maybeResult)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Map
#else
import qualified Data.HashMap.Lazy as Map
#endif
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | Location of the Javascript file hosted by browserid.org
browserIdJs :: Text
browserIdJs = "https://login.persona.org/include.js"

checkAssertion :: MonadIO m
               => Text -- ^ audience
               -> Text -- ^ assertion
               -> Manager
               -> m (Maybe Text)
checkAssertion audience assertion manager = do
    req' <- liftIO $ parseUrlThrow "https://verifier.login.persona.org/verify"
    let req = urlEncodedBody
                [ ("audience", encodeUtf8 audience)
                , ("assertion", encodeUtf8 assertion)
                ] req' { method = "POST" }
    res <- httpLbs req manager
    let lbs = responseBody res
    return $ maybeResult (parse json lbs) >>= getEmail
  where
    getEmail (Object o) =
        case (Map.lookup "status" o, Map.lookup "email" o) of
            (Just (String "okay"), Just (String e)) -> Just e
            _ -> Nothing
    getEmail _ = Nothing
