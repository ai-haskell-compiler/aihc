-- |
-- Module: Network.Shed.Httpd 
-- Copyright: Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: GHC
--
--
-- A trivial web server.
--
-- This web server promotes a Request to IO Response function
-- into a local web server. The user can decide how to interpret
-- the requests, and the library is intended for implementing Ajax APIs.
--
-- initServerLazy (and assocated refactorings), and Chunking support
-- was written by Henning Thielemann.
-- Handling of POST-based payloads was been written by Brandon Moore.
-- initServerBind support was written by John Van Enk.

{-# LANGUAGE CPP #-}
module Network.Shed.Httpd 
    ( Server
    , initServer
    , initServerLazy
    , initServerBind
    , initServerLazyBind
    , Request(..)
    , Response(..)
    , queryToArguments
    , addCache
    , noCache
    , contentType
    ) where

import qualified Network.Socket as Socket
import Network.URI (URI, parseURIReference, unEscapeString)
import Network.BSD (getProtocolNumber)
#if MIN_VERSION_network(3,0,0)
#else
import Network.Socket (iNADDR_ANY)
#endif
import Network.Socket (
          SockAddr(SockAddrInet),
          setSocketOption, socket)

import Control.Concurrent (forkIO)
import Control.Exception (finally)

import System.IO (Handle, hPutStr, hClose, hGetLine, hGetContents, IOMode(..))

import qualified Data.Char as Char
import Numeric (showHex)


#if MIN_VERSION_network(3,0,0)
iNADDR_ANY :: Socket.HostAddress
iNADDR_ANY = Socket.tupleToHostAddress (0,0,0,0)
#endif

type Server = () -- later, we might have a handle for shutting down a server.

{- |
This server transfers documents as one parcel, using the content-length header.
-}

initServer
   :: Int                       -- ^ The port number
   -> (Request -> IO Response)  -- ^ The functionality of the Server
   -> IO Server                 -- ^ A token for the Server
initServer port =
  initServerMain
     (\body -> ([("Content-Length", show (length body))], body))
     (SockAddrInet (fromIntegral port) iNADDR_ANY)

{- |
This server transfers documents in chunked mode
and without content-length header.
This way you can ship infinitely big documents.
It inserts the transfer encoding header for you.
The server binds to all interfaces
-}
initServerLazy
   :: Int                       -- ^ Chunk size
   -> Int                       -- ^ The port number
   -> (Request -> IO Response)  -- ^ The functionality of the Server
   -> IO Server                 -- ^ A token for the Server
initServerLazy chunkSize port =
  initServerLazyBind chunkSize port iNADDR_ANY

{- |
This server transfers documents in chunked mode
and without content-length header.
This way you can ship infinitely big documents.
It inserts the transfer encoding header for you.
The server binds to the specified address.
-}
initServerLazyBind
   :: Int                       -- ^ Chunk size
   -> Int                       -- ^ The port number
   -> Socket.HostAddress        -- ^ The host address
   -> (Request -> IO Response)  -- ^ The functionality of the Server
   -> IO Server                 -- ^ A token for the Server
initServerLazyBind chunkSize port addr =
  initServerMain
     (\body ->
        ([("Transfer-Encoding", "chunked")],
         foldr ($) "" $
         map
            (\str ->
               showHex (length str) . showCRLF .
               showString str . showCRLF)
            (slice chunkSize body) ++
         -- terminating chunk
         showString "0" . showCRLF :
         -- terminating trailer
         showCRLF :
         []))
     (SockAddrInet (fromIntegral port) addr)
     

showCRLF :: ShowS
showCRLF = showString "\r\n"

-- cf. Data.List.HT.sliceVertical
slice :: Int -> [a] -> [[a]]
slice n =
  map (take n) . takeWhile (not . null) . iterate (drop n)

{- |
This server transfers documents as one parcel, using the content-length header,
and takes an additional 
-}
initServerBind
   :: Int                               -- ^ The port number
   -> Socket.HostAddress                -- ^ The host address
   -> (Request -> IO Response)          -- ^ The functionality of the Server
   -> IO Server                         -- ^ A token for the Server
initServerBind port addr =
  initServerMain
      (\body -> ([("Content-Length", show (length body))], body)) 
      (SockAddrInet (fromIntegral port) addr)


initServerMain
   :: (String -> ([(String, String)], String))
   -> SockAddr
   -> (Request -> IO Response)
   -> IO Server
initServerMain processBody sockAddr callOut = do
--        installHandler sigPIPE Ignore Nothing    
--        sock  <- listenOn (PortNumber $ fromIntegral portNo)
        num <- getProtocolNumber "tcp"
        sock <- socket Socket.AF_INET Socket.Stream num
        setSocketOption sock Socket.ReuseAddr 1
        Socket.bind sock sockAddr
        Socket.listen sock Socket.maxListenQueue

        loopIO  
           (do (acceptedSock,_) <- Socket.accept sock
               h <- Socket.socketToHandle acceptedSock ReadWriteMode
               forkIO $ do 
                 ln <- hGetLine h
                 case words ln of
                   [mode,uri,"HTTP/1.1"] ->
                       case parseURIReference uri of
                         Just uri' -> readHeaders h mode uri' [] Nothing
                         _ -> do print uri 
                                 hClose h
                   _                      -> hClose h
                 return ()
           ) `finally` Socket.close sock
  where 
      loopIO m = m >> loopIO m

      readHeaders h mode uri hds clen = do
        line <- hGetLine h
        case span (/= ':') line of
          ("\r","") -> sendRequest h mode uri hds clen
          (name,':':rest) ->
            case map Char.toLower name of
              "content-length" ->
                readHeaders h mode uri (hds ++ [(name,dropWhile Char.isSpace rest)]) (Just (read rest))
              _ ->
                readHeaders h mode uri (hds ++ [(name,dropWhile Char.isSpace rest)]) clen
          _ -> hClose h -- strange format

      message code = show code ++ " " ++ 
                     case lookup code longMessages of
                       Just msg -> msg
                       Nothing -> "-"
      sendRequest h mode uri hds clen = do
          reqBody' <- case clen of
            Just l -> fmap (take l) (hGetContents h)
            Nothing -> return ""
          resp <- callOut $ Request { reqMethod = mode
                                    , reqURI    = uri
                                    , reqHeaders = hds
                                    , reqBody   = reqBody'
                                    } 
          let (additionalHeaders, body) =
                processBody $ resBody resp
          writeLines h $
            ("HTTP/1.1 " ++ message (resCode resp)) :
            ("Connection: close") :
            (map (\(hdr,val) -> hdr ++ ": " ++ val) $
                resHeaders resp ++ additionalHeaders) ++
            "" :
            []
          hPutStr h body
          hClose h

writeLines :: Handle -> [String] -> IO ()
writeLines h =
  hPutStr h . concatMap (++"\r\n")

-- | Takes an escaped query, optionally starting with '?', and returns an unescaped index-value list.
queryToArguments :: String -> [(String,String)]
queryToArguments ('?':rest) = queryToArguments rest
queryToArguments input = findIx input
   where
     findIx = findIx' . span (/= '=') 
     findIx' (index,'=':rest) = findVal (unEscapeString index) rest
     findIx' _ = []

     findVal index = findVal' index . span (/= '&')
     findVal' index (value,'&':rest) = (index,unEscapeString value) : findIx rest
     findVal' index (value,[])       = [(index,unEscapeString value)]
     findVal' _ _ = []

data Request = Request 
     { reqMethod  :: String
     , reqURI     :: URI
     , reqHeaders :: [(String,String)]
     , reqBody    :: String
     }
     deriving Show

data Response = Response
    { resCode    :: Int
    , resHeaders :: [(String,String)]
    , resBody    :: String
    }
     deriving Show

addCache :: Int -> (String,String)
addCache n = ("Cache-Control","max-age=" ++ show n)

noCache :: (String,String)
noCache = ("Cache-Control","no-cache")

-- examples include "text/html" and "text/plain"

contentType :: String -> (String,String)
contentType msg = ("Content-Type",msg)

------------------------------------------------------------------------------
longMessages :: [(Int,String)]
longMessages = 
    [ (200,"OK")
    , (404,"Not Found")
    ]
