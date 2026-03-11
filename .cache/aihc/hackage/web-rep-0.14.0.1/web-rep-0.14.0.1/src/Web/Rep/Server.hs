{-# LANGUAGE OverloadedLabels #-}

-- | Serve pages via 'ScottyM'
module Web.Rep.Server
  ( servePageWith,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Data.ByteString qualified as B
import Data.Text.Lazy (pack)
import MarkupParse
import Network.Wai.Middleware.Static (addBase, noDots, only, staticPolicy)
import Optics.Core hiding (only)
import Web.Rep.Page
import Web.Rep.Render
import Web.Scotty

-- | serve a Page via a ScottyM
servePageWith :: RoutePattern -> PageConfig -> Page -> ScottyM ()
servePageWith rp pc p =
  sequence_ $ servedir <> [getpage]
  where
    getpage = case pc ^. #concerns of
      Inline ->
        get rp (html $ pack $ utf8ToStr $ markdown_ Compact Html $ renderPageHtmlWith pc p)
      Separated ->
        let (css, js, h) = renderPageWith pc p
         in do
              middleware $ staticPolicy $ only [(cssfp, cssfp), (jsfp, jsfp)]
              get
                rp
                ( do
                    lift $ writeFile' cssfp css
                    lift $ writeFile' jsfp js
                    html $ pack $ utf8ToStr $ markdown_ Compact Html h
                )
    cssfp = pc ^. #filenames % #cssConcern
    jsfp = pc ^. #filenames % #jsConcern
    writeFile' fp s = unless (s == mempty) (B.writeFile fp s)
    servedir = (\x -> middleware $ staticPolicy (noDots <> addBase x)) <$> pc ^. #localdirs
