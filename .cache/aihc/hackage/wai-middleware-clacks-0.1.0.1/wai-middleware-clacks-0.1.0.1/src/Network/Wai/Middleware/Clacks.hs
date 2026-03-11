{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
The @Clacks@ middleware adds a @X-Clacks-Overhead@ header to every response
served by a Wai server.

To use this package to keep the legacy of Terry Pratchett alive, simply
pass your wai application to @clacks gnuTerryPratchett@ before passing it
to the @run@ function:

> import Network.Wai.Handler.Warp (run)
> import Network.Wai.Middleware.Clacks (clacks, gnuTerryPratchett)
>
> import MyLib.App (myApp)
>
> main :: IO ()
> main = run 8080 $ clacks gnuTerryPratchett myApp

You can use the 'Clacks' type to build a custom configuration for the
'clacks' function, allowing you to pass anything into the header:

> import Data.List.NonEmpty (NonEmpty(..))
> import Network.Wai (Middleware)
> import Network.Wai.Middleware.Clacks (Clacks(..), clacks)
>
> myClacks :: Middleware
> myClacks = clacks $ Clacks $ "GNU Ada Lovelace" :| ["GNU Hoban Washburne", "GNU Shephard Book"]

For more information about the Clacks or the @X-Clacks-Overhead@ header,
check out the <http://www.gnuterrypratchett.com GNU Terry Pratchett website>.

-}
module Network.Wai.Middleware.Clacks
    ( clacks
    , Clacks(..)
    , gnuTerryPratchett
    , clacksHeaderName
    )
where

import           Prelude.Compat

import           Data.List.NonEmpty.Compat      ( NonEmpty(..)
                                                , toList
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.HTTP.Types.Header      ( ResponseHeaders
                                                , HeaderName
                                                )
import           Network.Wai                    ( Middleware
                                                , modifyResponse
                                                , mapResponseHeaders
                                                )

import qualified Data.ByteString               as BS
import qualified Data.CaseInsensitive          as CI
import qualified Data.List                     as L
import qualified Data.Text                     as T


-- | A "Network.Wai" 'Middleware' that adds a @X-Clacks-Overhead@ header
-- containing the messages in the 'Clacks' configuration.
clacks :: Clacks -> Middleware
clacks settings = modifyResponse $ mapResponseHeaders addHeader
  where
    headerContents :: BS.ByteString
    headerContents =
        encodeUtf8 . T.intercalate "," . toList $ clacksMessages settings
    addHeader :: ResponseHeaders -> ResponseHeaders
    addHeader hs = case L.find ((== clacksHeaderName) . fst) hs of
        Nothing      -> (clacksHeaderName, headerContents) : hs
        Just (_, "") -> (clacksHeaderName, headerContents) : hs
        Just (name, contents) ->
            (name, contents <> "," <> headerContents)
                : filter ((/= clacksHeaderName) . fst) hs

-- | Configuration for the Clacks WAI Middleware.
newtype Clacks =
    Clacks
        { clacksMessages :: NonEmpty T.Text
        -- ^ The Clacks Messages to Include in the Header.
        } deriving (Show, Read, Eq)


-- | Sends a Clacks message of @GNU Terry Pratchett@.
gnuTerryPratchett :: Clacks
gnuTerryPratchett = Clacks $ "GNU Terry Pratchett" :| []


-- | The name of the Clacks header.
clacksHeaderName :: HeaderName
clacksHeaderName = CI.mk "X-Clacks-Overhead"
