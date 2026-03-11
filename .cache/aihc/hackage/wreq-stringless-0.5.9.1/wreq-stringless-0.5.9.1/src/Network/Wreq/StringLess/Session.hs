-- |
-- Use this module instead of @Network.Wreq.Session@ to use string-like datatypes.
--
-- see https://hackage.haskell.org/package/wreq/docs/Network-Wreq-Session.html
--
module Network.Wreq.StringLess.Session
    (
    -- * Session creation
      WreqS.Session
    , WreqS.newSession
    , WreqS.newAPISession
    , WreqS.withSession
    , WreqS.withAPISession
    -- ** More control-oriented session creation
    , WreqS.newSessionControl
    , WreqS.withSessionWith
    , WreqS.withSessionControl
    -- ** Get information about session state
    , WreqS.getSessionCookieJar
    -- * HTTP verbs
    -- ** GET
    , get
    , getWith
    -- ** POST
    , post
    , postWith
    -- ** HEAD
    , head_
    , headWith
    -- ** OPTIONS
    , options
    , optionsWith
    -- ** PUT
    , put
    , putWith
    -- ** DELTE
    , delete
    , deleteWith
    -- ** Custom Method
    , customMethod
    , customMethodWith
    -- ** Custom Payload Method
    , customPayloadMethodWith
    , customHistoriedMethodWith
    , customHistoriedPayloadMethodWith
    -- * Extending a session
    , WreqS.seshRun
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wreq         as Wreq
import qualified Network.Wreq.Session as WreqS
import           Network.Wreq.Types
import Network.Wreq.StringLess.StringLike

get :: StringLike s => WreqS.Session -> s -> IO (Wreq.Response LBS.ByteString)
get sesh = WreqS.get sesh . toString


getWith :: StringLike s => Wreq.Options -> WreqS.Session -> s -> IO (Wreq.Response LBS.ByteString)
getWith opts sesh = WreqS.getWith opts sesh . toString


post :: StringLike s => Postable a => WreqS.Session -> s -> a -> IO (Wreq.Response LBS.ByteString)
post sesh = WreqS.post sesh . toString


postWith :: StringLike s => Postable a => Wreq.Options -> WreqS.Session -> s -> a -> IO (Wreq.Response LBS.ByteString)
postWith opts sesh = WreqS.postWith opts sesh . toString


head_ :: StringLike s => WreqS.Session -> s -> IO (Wreq.Response ())
head_ sesh = WreqS.head_ sesh . toString


headWith :: StringLike s => Wreq.Options -> WreqS.Session -> s -> IO (Wreq.Response ())
headWith opts sesh = WreqS.headWith opts sesh . toString


options :: StringLike s => WreqS.Session -> s -> IO (Wreq.Response ())
options sesh = WreqS.options sesh . toString


optionsWith :: StringLike s => Wreq.Options -> WreqS.Session -> s -> IO (Wreq.Response ())
optionsWith opts sesh = WreqS.optionsWith opts sesh . toString


put :: StringLike s => Putable a => WreqS.Session -> s -> a -> IO (Wreq.Response LBS.ByteString)
put sesh = WreqS.put sesh . toString


putWith :: StringLike s => Putable a => Wreq.Options -> WreqS.Session -> s -> a -> IO (Wreq.Response LBS.ByteString)
putWith opts sesh = WreqS.putWith opts sesh . toString


delete :: StringLike s => WreqS.Session -> s -> IO (Wreq.Response LBS.ByteString)
delete sesh = WreqS.delete sesh . toString


customMethod :: StringLike s => s -> WreqS.Session -> s -> IO (Wreq.Response LBS.ByteString)
customMethod = flip customMethodWith Wreq.defaults


deleteWith :: StringLike s => Wreq.Options -> WreqS.Session -> s -> IO (Wreq.Response LBS.ByteString)
deleteWith opts sesh = WreqS.deleteWith opts sesh . toString


customMethodWith :: StringLike s => s -> Wreq.Options -> WreqS.Session -> s -> IO (Wreq.Response LBS.ByteString)
customMethodWith method opts sesh url = WreqS.customMethodWith (toString method) opts sesh (toString url)


customHistoriedMethodWith :: StringLike s => s -> Options -> WreqS.Session -> s -> IO (Wreq.HistoriedResponse LBS.ByteString)
customHistoriedMethodWith method opts sesh url = WreqS.customHistoriedMethodWith (toString method) opts sesh (toString url)


customPayloadMethodWith :: (Postable a, StringLike s) => s -> Wreq.Options -> WreqS.Session -> s -> a -> IO (Wreq.Response LBS.ByteString)
customPayloadMethodWith method opts sesh url = WreqS.customPayloadMethodWith (toString method) opts sesh (toString url)


customHistoriedPayloadMethodWith :: (Postable a, StringLike s) => s -> Wreq.Options -> WreqS.Session -> s -> a -> IO (Wreq.HistoriedResponse LBS.ByteString)
customHistoriedPayloadMethodWith method opts sesh url = WreqS.customHistoriedPayloadMethodWith (toString method) opts sesh (toString url)
