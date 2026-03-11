
{- | Description: DohApi implementation. -}
module OM.DoH.Server (
  server,
) where


import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.DNS (sendRaw)
import OM.DoH.Api (DoHApi(DoHApi, getQuery, postQuery), Query(unQuery),
  Response(Response))
import Servant.Server.Generic (AsServerT)


{- |
  An implementation of the DoH api that delegates to the local machine's
  DNS resolver, using the @resolv@ package (see "Network.DNS").
-}
server :: (MonadIO m) => DoHApi (AsServerT m)
server = DoHApi {
    getQuery = liftIO . fmap Response . sendRaw . unQuery,
    postQuery = liftIO . fmap Response . sendRaw . unQuery
  }



