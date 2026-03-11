
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module YesodAuthHttpBasicSpec where

import           Data.Monoid ((<>))
import           Test.Hspec
import           Yesod       hiding (get)
import           Yesod.Test


-- TODO create real tests

spec :: SpecWith ()
spec = describe "Yesod HTTP Basic Authentication" $
  yesodSpec app $
    ydescribe "Yesod HTTP Basic Authentication" $
      ydescribe "tests1" $ do
        yit "Has Basic Auth" $ do
           -- addRequestHeader (hUserAgent, "Chrome/41.0.2228.0")
           get $ LiteAppRoute []
           statusIs 200
        yit "Denies Failed Basic Auth" $ do
          get $ LiteAppRoute []
          statusIs 200
          -- statusIs 403


instance RenderMessage LiteApp FormMessage where
  renderMessage _ _ = defaultFormMessage


app :: LiteApp
app = liteApp $
  dispatchTo $ do
    mfoo <- lookupGetParam "foo"
    case mfoo of
     Nothing -> return "Hello world!"
     Just foo -> return $ "foo=" <> foo
