{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | An example which should always compile and demonstrates \"real\"
-- code.

module Example1 where

import Control.Monad
import Lucid

demo :: Html ()
demo =
  doctypehtml_
    (do head_ (do meta_ [charset_ "utf-8"]
                  meta_ [name_ "viewport"
                        ,content_ "width=device-width, initial-scale=1"]
                  link_ [href_ "//fonts.googleapis.com/css?family=Open+Sans"
                        ,rel_ "stylesheet"
                        ,type_ "text/css"]
                  link_ [href_ "//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/css/bootstrap.min.css"
                        ,rel_ "stylesheet"
                        ,type_ "text/css"]
                  title_ "YSU Closing Status")
        body_ (div_ [class_ "container"]
                    (do h1_ "YSU Closing Status"
                        t_ [class_ "deal"] "So, here's the deal:"
                        t_ (do "The weather is currently "
                               strong_ "(unknown)"
                               " and "
                               strong_ "(unknown)"
                               ".")
                        t_ (do "There are currently "
                               strong_ "Closings!"
                               " delays/closings according to a local (Youngstown) news source.")
                        t_ (do "Youngstown State University "
                               strong_ (if False
                                           then span_ [style_ "color: green;"] "WAS mentioned"
                                           else span_ [style_ "color: red;"] "was NOT mentioned")
                               " among them.")
                        t_ (do "There are currently "
                               strong_ (toHtml (maybe "unknown" show (Just 123 :: Maybe Int)))
                               " weather alert(s) covering Youngstown as of "
                               strong_ "2014-23-23"
                               ".")
                        when (0 /= 1)
                             (ul_ (mapM_ (\w ->
                                            li_ (do strong_ "Foo"
                                                    " expiring "
                                                    toHtml (show w)))
                                         [1 .. 5]))
                        hr_ []
                        p_ [style_ "text-align: center;"]
                           (small_ (do "This website is not affiliated Youngstown "
                                       "State University in any way. It was "
                                       (a_ [href_ "https://github.com/relrod/isysuclosed.com/"]
                                           "written")
                                       " to make a point."))
                        p_ [style_ "text-align: center;"]
                           (small_ (do "While hopefully accurate, this is NOT an official "
                                       "resource. Always confirm "
                                       a_ [href_ "https://swww.ysu.edu/downloads/closing_procedure.pdf"]
                                          "official"
                                       " resources."))
                        p_ [style_ "text-align: center; color: #888888"]
                           (small_ "Valid HTML5. Weather information via Weather Underground.")
                        img_ [style_ "display: block; margin: 0 auto; width: 180px;"
                             ,src_ "http://icons.wxug.com/logos/images/wundergroundLogo_4c_horz.jpg"
                             ,alt_ "Weather Underground Logo"])))
  where t_ :: Term a r
           => a -> r
        t_ = termWith "p" [class_ " t "]
