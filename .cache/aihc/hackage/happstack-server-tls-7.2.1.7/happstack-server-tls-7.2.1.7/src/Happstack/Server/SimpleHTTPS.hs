module Happstack.Server.SimpleHTTPS
     ( TLSConf(..)
     , nullTLSConf
     , simpleHTTPS
     , simpleHTTPS'
     ) where

import Data.Maybe                    (fromMaybe)
import Happstack.Server              (ToMessage(..), UnWebT, ServerPartT, simpleHTTP'', mapServerPartT, runValidator)
import Happstack.Server.Internal.TLS (TLSConf(..), nullTLSConf, listenTLS)

-- |start the https:\/\/ server, and handle requests using the supplied
-- 'ServerPart'.
--
-- This function will not return, though it may throw an exception.
--
simpleHTTPS :: (ToMessage a) =>
               TLSConf           -- ^ tls server configuration
            -> ServerPartT IO a  -- ^ server part to run
            -> IO ()
simpleHTTPS = simpleHTTPS' id

-- | similar 'simpleHTTPS' but allows you to supply a function to convert 'm' to 'IO'.
simpleHTTPS' :: (ToMessage b, Monad m, Functor m) =>
                (UnWebT m a -> UnWebT IO b)
            -> TLSConf
            -> ServerPartT m a
            -> IO ()
simpleHTTPS' toIO tlsConf hs =
    listenTLS tlsConf (\req -> runValidator (fromMaybe return (tlsValidator tlsConf)) =<< (simpleHTTP'' (mapServerPartT toIO hs) req))
