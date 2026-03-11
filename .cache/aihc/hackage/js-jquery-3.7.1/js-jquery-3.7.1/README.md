# js-jquery [![Hackage version](https://img.shields.io/hackage/v/js-jquery.svg?label=Hackage)](https://hackage.haskell.org/package/js-jquery) [![Stackage version](https://www.stackage.org/package/js-jquery/badge/nightly?label=Stackage)](https://www.stackage.org/package/js-jquery) [![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/js-jquery/ci.yml?branch=master)](https://github.com/ndmitchell/js-jquery/actions)

This package bundles the minified [jQuery](http://jquery.com/) code into a Haskell package,
so it can be depended upon by Cabal packages. The first three components of
the version number match the upstream jQuery version. The package is designed
to meet the redistribution requirements of downstream users (e.g. Debian).
As an example:

```haskell
import qualified Language.Javascript.JQuery as JQuery

main = do
    putStrLn $ "jQuery version " ++ show JQuery.version ++ " source:"
    putStrLn =<< readFile =<< JQuery.file
```

This package installs data files containing the jQuery sources, which must be available at runtime. If you want to produce an executable with no dependency on associated data files, you can use the [`file-embed`](https://hackage.haskell.org/package/file-embed) library:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Language.Javascript.JQuery as JQuery
import Language.Haskell.TH.Syntax

main = print jQueryContents

jQueryContents :: BS.ByteString
jQueryContents = $(embedFile =<< runIO JQuery.file)
```

Or, within the Yesod framework, [yesod-static](https://hackage.haskell.org/package/yesod-static):

```haskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}

import Yesod
import Yesod.EmbeddedStatic
import qualified Language.Javascript.JQuery as JQuery

data MySite = MySite {
    ...
    , getStatic :: EmbeddedStatic
    ...
    }

-- Create a Route `jquery_js` to a statically-serveable version of the local jQuery lib.
mkEmbeddedStatic False "myStatic" . pure . embedFileAt "jquery.js" =<< runIO JQuery.file

mkYesod "PresentationServer" [parseRoutes|
/ HomeR Get
...
/static StaticR EmbeddedStatic getStatic
|]
instance Yesod PresentationServer where
  addStaticContent = embedStaticContent getStatic StaticR Right

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
   addScript $ StaticR jquery_js
   ...

```
(See also [`sample-embed.hs`](https://github.com/yesodweb/yesod/blob/master/yesod-static/sample-embed.hs).)
