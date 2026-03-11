{- |
Module      : Network.Socks5.Wire
License     : BSD-style
Copyright   : (c) 2010-2019 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Network.Socks5.Wire
  ( SocksHello (..)
  , SocksHelloResponse (..)
  , SocksRequest (..)
  , SocksResponse (..)
  ) where

import           Control.Monad ( liftM4, replicateM )
import qualified Data.ByteString as B
import           Data.Serialize
                   ( Get, Put, PutM (..), Serialize (..), getByteString
                   , getWord16be, getWord32host, getWord8, putByteString
                   , putWord16be, putWord32host, putWord8
                   )
import           Network.Socket ( PortNumber )
import           Network.Socks5.Types
                   ( SocksCommand, SocksHostAddress (..), SocksMethod
                   , SocksReply
                   )

-- | Type representing initial messages sent by a client with the list of
-- authentification methods supported.
newtype SocksHello = SocksHello
  { getSocksHelloMethods :: [SocksMethod]
  }
  deriving (Eq, Show)

-- | Type representing initial messages sent by a server in response to Hello,
-- with the server's chosen method of authentication.
newtype SocksHelloResponse = SocksHelloResponse
  { getSocksHelloResponseMethod :: SocksMethod
  }
  deriving (Eq, Show)

-- | Type representing SOCKS requests.
data SocksRequest = SocksRequest
  { requestCommand :: SocksCommand
  , requestDstAddr :: SocksHostAddress
  , requestDstPort :: PortNumber
  }
  deriving (Eq, Show)

-- | Type representing SOCKS responses.
data SocksResponse = SocksResponse
    { responseReply    :: SocksReply
    , responseBindAddr :: SocksHostAddress
    , responseBindPort :: PortNumber
    }
    deriving (Eq, Show)

getAddr :: (Eq a, Num a, Show a) => a -> Get SocksHostAddress
getAddr 1 = SocksAddrIPV4 <$> getWord32host
getAddr 3 = SocksAddrDomainName <$> (getLength8 >>= getByteString)
getAddr 4 = SocksAddrIPV6 <$>
  liftM4 (,,,) getWord32host getWord32host getWord32host getWord32host
getAddr n = error ("cannot get unknown socket address type: " <> show n)

putAddr :: SocksHostAddress -> PutM ()
putAddr (SocksAddrIPV4 h) = putWord8 1 >> putWord32host h
putAddr (SocksAddrDomainName b) =
  putWord8 3 >> putLength8 (B.length b) >> putByteString b
putAddr (SocksAddrIPV6 (a, b, c, d)) =
  putWord8 4 >> mapM_ putWord32host [a,b,c,d]

putEnum8 :: Enum e => e -> Put
putEnum8 = putWord8 . fromIntegral . fromEnum

getEnum8 :: Enum e => Get e
getEnum8 = toEnum . fromIntegral <$> getWord8

putLength8 :: Int -> Put
putLength8 = putWord8 . fromIntegral

getLength8 :: Get Int
getLength8 = fromIntegral <$> getWord8

getSocksRequest :: (Eq a, Num a, Show a) => a -> Get SocksRequest
getSocksRequest 5 = do
  cmd <- getEnum8
  _ <- getWord8
  addr <- getWord8 >>= getAddr
  port <- fromIntegral <$> getWord16be
  return $ SocksRequest cmd addr port
getSocksRequest v =
  error ("unsupported version of the protocol " <> show v)

getSocksResponse :: (Eq a, Num a, Show a) => a -> Get SocksResponse
getSocksResponse 5 = do
  reply <- getEnum8
  _ <- getWord8
  addr <- getWord8 >>= getAddr
  port <- fromIntegral <$> getWord16be
  return $ SocksResponse reply addr port
getSocksResponse v =
  error ("unsupported version of the protocol " <> show v)

instance Serialize SocksHello where
  put (SocksHello ms) = do
    putWord8 5
    putLength8 (length ms)
    mapM_ putEnum8 ms
  get = do
    v <- getWord8
    case v of
      5 -> SocksHello <$> (getLength8 >>= flip replicateM getEnum8)
      _ -> error "unsupported sock hello version"

instance Serialize SocksHelloResponse where
  put (SocksHelloResponse m) = putWord8 5 >> putEnum8 m
  get = do
    v <- getWord8
    case v of
      5 -> SocksHelloResponse <$> getEnum8
      _ -> error "unsupported sock hello response version"

instance Serialize SocksRequest where
  put req = do
    putWord8 5
    putEnum8 $ requestCommand req
    putWord8 0
    putAddr $ requestDstAddr req
    putWord16be $ fromIntegral $ requestDstPort req

  get = getWord8 >>= getSocksRequest

instance Serialize SocksResponse where
  put req = do
    putWord8 5
    putEnum8 $ responseReply req
    putWord8 0
    putAddr $ responseBindAddr req
    putWord16be $ fromIntegral $ responseBindPort req
  get = getWord8 >>= getSocksResponse
