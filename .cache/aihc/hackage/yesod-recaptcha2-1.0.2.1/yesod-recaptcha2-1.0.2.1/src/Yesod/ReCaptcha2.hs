{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
module Yesod.ReCaptcha2
  ( YesodReCaptcha(..)
    -- * ReCaptcha V2
  , reCaptcha
  , mReCaptcha
    -- * Invisible ReCaptcha
    -- $invisibleReCaptcha
  , reCaptchaInvisible
  , mReCaptchaInvisible
  , reCaptchaInvisibleForm
  ) where

import           Control.Monad         (when)
import           Data.Maybe            (isNothing)
import           Data.String.Transform (ToByteStringStrict (toByteStringStrict))
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple   (getResponseBody, httpJSON, parseRequest,
                                        setRequestBodyURLEncoded)
import           Yesod.Core            (FromJSON, HandlerFor, MonadIO (liftIO), MonadTrans (lift),
                                        ToJSON, ToWidgetHead (toWidgetHead), WidgetFor,
                                        addScriptRemote, hamlet, handlerToWidget, lookupPostParam,
                                        newIdent, whamlet)
import           Yesod.Form.Functions  (formToAForm)
import           Yesod.Form.Types      (AForm, FieldView (..), FormResult (..), MForm)

-- | default key is testing. you should impl reCaptchaSiteKey and reCaptchaSecretKey
class YesodReCaptcha site where
  reCaptchaSiteKey :: HandlerFor site Text
  reCaptchaSiteKey = pure "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI"
  reCaptchaSecretKey :: HandlerFor site Text
  reCaptchaSecretKey = pure "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe"
  -- | with specific language from
  -- <https://developers.google.com/recaptcha/docs/language>
  --
  -- > reCaptchaLanguage = pure (Just "ru")
  reCaptchaLanguage :: HandlerFor site (Maybe Text)
  reCaptchaLanguage = pure Nothing

newtype SiteverifyResponse
  = SiteverifyResponse
  { success :: Bool
  }
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

-- | for Applicative style form
reCaptcha :: YesodReCaptcha site => AForm (HandlerFor site) ()
reCaptcha = formToAForm mReCaptcha

-- | for Monadic style form
mReCaptcha
  :: YesodReCaptcha site
  => MForm (HandlerFor site) (FormResult (), [FieldView site])
mReCaptcha = do
  result <- lift formResult
  return (result, [fieldViewSite])
 where
  formResult = do
    postParam <- lookupPostParam "g-recaptcha-response"
    case postParam of
      Nothing       -> return FormMissing
      Just response -> do
        secret                         <- reCaptchaSecretKey
        SiteverifyResponse { success } <- liftIO $ do
          req <- parseRequest
            "POST https://www.google.com/recaptcha/api/siteverify"
          res <- httpJSON $ setRequestBodyURLEncoded
            [("secret", toByteStringStrict secret), ("response", toByteStringStrict response)]
            req
          return $ getResponseBody res
        return $ if success
          then FormSuccess ()
          else FormFailure ["reCaptcha error"]
  fieldViewSite = FieldView
    { fvLabel    = mempty
    , fvTooltip  = Nothing
    , fvId       = ""
    , fvInput    = do
      mReCaptchaLanguage <- handlerToWidget reCaptchaLanguage
      case mReCaptchaLanguage of
        Nothing -> addScriptRemote "https://www.google.com/recaptcha/api.js"
        Just hl ->
          addScriptRemote $ "https://www.google.com/recaptcha/api.js?hl=" <> hl
      siteKey <- handlerToWidget reCaptchaSiteKey
      [whamlet|<div .g-recaptcha data-sitekey=#{siteKey}>|]
    , fvErrors   = Nothing
    , fvRequired = True
    }

-- $invisibleReCaptcha
--
-- The Invisible ReCaptcha is not as easy as the V2.
--
-- 1. Function to check the response: 'reCaptchaInvisible' or 'mReCaptchaInvisible'.
--
-- 2. Add the following to the code which creates the form:
--
--     > (reCaptchaFormId, reCaptchaWidget, reCaptchaButtonAttributes) <-
--     > reCaptchaInvisibleForm Nothing
--
-- 3. Add the id to the form, class and attributes to the button and the widget somewhere.
--    Example:
--
--     @
--     \<form \#\#{reCaptchaFormId} method=post action=@{route} enctype=#{enctype}\>
--       ^{widget}
--       ^{reCaptchaWidget}
--
--       \<button .g-recaptcha *{reCaptchaButtonAttributes}\>
--         Submit
--     @

-- | check for Applicative style form
reCaptchaInvisible :: YesodReCaptcha site => AForm (HandlerFor site) ()
reCaptchaInvisible = formToAForm ((, []) <$> mReCaptchaInvisible)

-- | check for Monadic style form
mReCaptchaInvisible
  :: YesodReCaptcha site => MForm (HandlerFor site) (FormResult ())
mReCaptchaInvisible = fst <$> mReCaptcha

-- | generate all required parts (except the check) for a Invisible ReCaptcha
reCaptchaInvisibleForm
  :: YesodReCaptcha site
  => Maybe Text -- ^ The id of the form, a new will be created when 'Nothing' is passed
  -> Maybe Text
    -- ^ The javascript to call after a successful captcha,
    -- it has to submit the form, a simple one will be generated when 'Nothing' is passed
  -> HandlerFor site (Text, WidgetFor site (), [(Text, Text)])
reCaptchaInvisibleForm mIdent mScript = do
  mReCaptchaLanguage <- reCaptchaLanguage
  siteKey            <- reCaptchaSiteKey
  identForm          <- maybe newIdent return mIdent
  scriptName <- maybe (("reCaptchaOnSubmit_" <>) <$> newIdent) return mScript
  let widget = do
        case mReCaptchaLanguage of
          Nothing -> addScriptRemote "https://www.google.com/recaptcha/api.js"
          Just hl ->
            addScriptRemote
              $  "https://www.google.com/recaptcha/api.js?hl="
              <> hl
        when (isNothing mScript) $ toWidgetHead [hamlet|
<script>function #{scriptName}(token) { document.getElementById("#{identForm}").submit(); }
|]
  return
    ( identForm
    , widget
    , [("data-sitekey", siteKey), ("data-callback", scriptName)]
    )
