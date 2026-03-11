{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Bootstrap where

import Control.Monad
#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

stylesheet :: AttributeValue -> Html
stylesheet path =
    link ! href path ! rel "stylesheet" ! type_ "text/css"

javascript :: AttributeValue -> Html
javascript path =
    script ! src path $ emptyEl

emptyEl :: Html
emptyEl = mempty

container :: Html -> Html
container x =
    H.div ! class_ "container" $ x

row :: Html -> Html
row x =
    H.div ! class_ "row" $ x

dataToggle :: AttributeValue -> Attribute
dataToggle = dataAttribute "toggle"

dataTarget :: AttributeValue -> Attribute
dataTarget = dataAttribute "target"

dataDismiss :: AttributeValue -> Attribute
dataDismiss = dataAttribute "dismiss"

ariaHidden :: Bool -> Attribute
ariaHidden bool =
    customAttribute  "aria-hidden" (if bool then "true" else "false")

role :: AttributeValue -> Attribute
role = customAttribute "role"

data BootAlertType
   = BootAlertDanger
   | BootAlertWarn
   | BootAlertInfo
   | BootAlertSuccess

alertBox :: BootAlertType -> Html -> Html
alertBox alertType alertVal =
    H.div ! class_ (toValue $ T.concat ["alert alert-dismissable ", t]) $
    do button ! type_ "button" ! class_ "close" ! dataDismiss "alert" ! ariaHidden True $ (unsafeByteString "&times;")
       alertVal
    where
      t =
          case alertType of
            BootAlertDanger -> "alert-danger"
            BootAlertWarn -> "alert-warning"
            BootAlertInfo -> "alert-info"
            BootAlertSuccess -> "alert-success"

mainNavigation :: AttributeValue
               -> Html -> [(AttributeValue, Html)] -> Html
mainNavigation indexPath pageTitle navPoints =
    nav ! class_ "navbar navbar-default navbar-fixed-top" $
     container $
      do H.div ! class_ "navbar-header page-scroll" $
          do button ! type_ "button" ! class_ "navbar-toggle" ! dataToggle "collapse" ! dataTarget "#main-nav" $
              do H.span ! class_ "sr-only" $ "Toggle navigation"
                 H.span ! class_ "icon-bar" $ emptyEl
                 H.span ! class_ "icon-bar" $ emptyEl
                 H.span ! class_ "icon-bar" $ emptyEl
             a ! class_ "navbar-brand" ! href indexPath $ pageTitle
         H.div ! class_ "collapse navbar-collapse" ! A.id "main-nav" $
           ul ! class_ "nav navbar-nav navbar-right" $
              forM_ navPoints $ \(url, val) ->
                li (a ! href url $ val)

formGroup :: Html -> Html
formGroup formBody =
    H.div ! class_ "form-group" $ formBody

formSelect :: (Eq k, ToValue k, ToMarkup v)
           => T.Text -> AttributeValue -> [(k, v)] -> Maybe k -> Html
formSelect selLabel selName keyValues selectedV =
    formGroup $
    do H.label ! for selName $ (toHtml selLabel)
       H.select ! name selName ! class_ "form-control" $
        forM_ keyValues $ \(k, v) ->
          H.option ! value (toValue k) !? (Just k == selectedV, selected "selected") $ toMarkup v

formSubmit :: Html -> Html
formSubmit buttonVal =
    H.button ! type_ "submit" ! class_ "btn btn-lg btn-success btn-block" $ buttonVal

tableResponsive :: Html -> Html -> Html
tableResponsive tblHead tblBody =
    H.div ! class_ "table-responsive" $
    table ! class_ "table table-striped table-bordered table-hover" $
          do thead tblHead
             tbody tblBody
