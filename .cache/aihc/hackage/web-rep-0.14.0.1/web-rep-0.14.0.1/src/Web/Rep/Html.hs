{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Key generators and miscellaneous html utilities.
module Web.Rep.Html
  ( libCss,
    libJs,
  )
where

import Data.ByteString (ByteString)
import MarkupParse

-- $setup
--
-- >>> import Web.Rep.Html
-- >>> import MarkupParse
-- >>> :set -XOverloadedStrings

-- | Convert a link to a css library from text to html.
--
-- >>> markdown_ Compact Html $ libCss "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
-- "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\">"
libCss :: ByteString -> Markup
libCss url =
  element_
    "link"
    [ Attr "rel" "stylesheet",
      Attr "href" url
    ]

-- | Convert a link to a js library from text to html.
--
-- >>> markdown_ Compact Html $ libJs "https://code.jquery.com/jquery-3.3.1.slim.min.js"
-- "<script src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\"></script>"
libJs :: ByteString -> Markup
libJs url = element_ "script" [Attr "src" url]
