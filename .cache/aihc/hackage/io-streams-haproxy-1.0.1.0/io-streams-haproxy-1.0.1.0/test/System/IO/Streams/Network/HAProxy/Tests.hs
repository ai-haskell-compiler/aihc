{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Streams.Network.HAProxy.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Applicative                        ((<$>))
import           Control.Concurrent
import qualified Control.Exception                          as E
import           Control.Monad                              (forever)
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import qualified Data.ByteString                            as S8
import           Data.ByteString.Char8                      (ByteString)
import qualified Data.ByteString.Char8                      as S
import           Data.Typeable
import qualified Network.Socket                             as N
import           System.IO                                  (hPutStrLn, stderr)
import           System.IO.Streams                          (InputStream, OutputStream)
import qualified System.IO.Streams                          as Streams
import qualified System.IO.Streams.Network.HAProxy          as HA
import           System.IO.Streams.Network.Internal.Address (AddressNotSupportedException (..), getSockAddr, getSockAddrImpl)
import           System.Timeout                             (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                                 hiding (Test)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testOldHaProxy
        , testOldHaProxy6
        , testOldHaProxyFailure
        , testOldHaProxyLocal
        , testOldHaProxyLocal6
        , testOldHaProxyBadAddress
        , testBlackBox
        , testBlackBoxLocal
        , testNewHaProxy
#ifndef WINDOWS
        , testNewHaProxyUnix
#endif
        , testNewHaProxy6
        , testNewHaProxyTooBig
        , testNewHaProxyTooSmall
        , testNewHaProxyBadVersion
        , testNewHaProxyLocal
        , testGetSockAddr
        , testTrivials
        ]


------------------------------------------------------------------------------
runInput :: ByteString
         -> N.SockAddr
         -> N.SockAddr
         -> (HA.ProxyInfo -> InputStream ByteString -> OutputStream ByteString -> IO a)
         -> IO a
runInput input sa sb action = do
    is <- Streams.fromList [input]
    (os, _) <- Streams.listOutputStream
    let pinfo = HA.makeProxyInfo sa sb (addrFamily sa) N.Stream
    HA.behindHAProxyWithLocalInfo pinfo (is, os) action


------------------------------------------------------------------------------
addrFamily :: N.SockAddr -> N.Family
addrFamily s = case s of
                 (N.SockAddrInet _ _)      -> N.AF_INET
                 (N.SockAddrInet6 _ _ _ _) -> N.AF_INET6
#ifndef WINDOWS
                 (N.SockAddrUnix _ )       -> N.AF_UNIX
#endif

------------------------------------------------------------------------------
blackbox :: (Chan Bool
             -> HA.ProxyInfo
             -> InputStream ByteString
             -> OutputStream ByteString
             -> IO ())
         -> ByteString
         -> IO ()
blackbox action input = withTimeout 10 $ do
    chan <- newChan
    E.bracket (startServer chan) (killThread . fst) client
    readChan chan >>= assertBool "success"
  where
    client (_, port) = do
        (family, addr) <- getSockAddr port "127.0.0.1"
        E.bracket (N.socket family N.Stream 0) N.close $ \sock -> do
            N.connect sock addr
            (_, os) <- Streams.socketToStreams sock
            threadDelay 10000
            Streams.write (Just input) os
            Streams.write Nothing os
            threadDelay 10000

    withTimeout n m = timeout (n * 1000000) m >>= maybe (fail "timeout") return

    startServer :: Chan Bool -> IO (ThreadId, Int)
    startServer = E.bracketOnError getSock N.close . forkServer

    getBindAddr = do
#if MIN_VERSION_network(2,7,0)
        getSockAddr (fromIntegral N.defaultPort) "127.0.0.1"
#else
        getSockAddr (fromIntegral N.aNY_PORT) "127.0.0.1"
#endif

    getSock = do
        (family, addr) <- getBindAddr
        sock           <- N.socket family N.Stream 0
        N.setSocketOption sock N.ReuseAddr 1
        N.setSocketOption sock N.NoDelay 1

#if MIN_VERSION_network(2,7,0)
        N.bind sock addr
#else
        N.bindSocket sock addr
#endif
        N.listen sock 150
        return $! sock

    forkServer chan sock = do
        port <- fromIntegral <$> N.socketPort sock
        tid  <- E.mask_ $ forkIOWithUnmask $ server chan sock
        return (tid, port)

    server :: Chan Bool -> N.Socket -> (forall z. IO z -> IO z) -> IO ()
    server chan boundSocket restore = loop `E.finally` N.close boundSocket
      where
        loop = forever $
               E.bracketOnError (restore $ N.accept boundSocket)
                                (N.close . fst)
                                (\(sock, sa) ->
                                   forkIOWithUnmask
                                     $ \r -> flip E.finally (N.close sock)
                                       $ r $ HA.behindHAProxy sock sa (action chan))


------------------------------------------------------------------------------
testBlackBox :: Test
testBlackBox = testCase "test/blackbox" $
               blackbox action
                        "PROXY TCP4 127.0.0.1 127.0.0.1 10000 80\r\nblah"
   where
    action chan proxyInfo !is !_ = do
        sa <- localhost 10000
        sb <- localhost 80
        x  <- E.try $ do assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
                         assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
                         Streams.toList is >>= assertEqual "rest" ["blah"]
        case x of
          Left (e :: E.SomeException) -> do hPutStrLn stderr $ show e
                                            writeChan chan False
          Right !_ -> writeChan chan True


------------------------------------------------------------------------------
testBlackBoxLocal :: Test
testBlackBoxLocal = testCase "test/blackbox_local" $
                    blackbox action "PROXY UNKNOWN\r\nblah"
  where
    action chan proxyInfo !is !_ = do
        let q = HA.getSourceAddr proxyInfo `seq` HA.getDestAddr proxyInfo
                                           `seq` ()
        x  <- q `seq` E.try $ go is proxyInfo
        case x of
          Left (e :: E.SomeException) -> do hPutStrLn stderr $ show e
                                            writeChan chan False
          Right !_ -> writeChan chan True
    go is proxyInfo = do
        Streams.toList is >>= assertEqual "rest" ["blah"]
        assertEqual "family" N.AF_INET $ HA.getFamily proxyInfo
        assertEqual "type" N.Stream $ HA.getSocketType proxyInfo


------------------------------------------------------------------------------
testOldHaProxy :: Test
testOldHaProxy = testCase "test/old_ha_proxy" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    runInput "PROXY TCP4 127.0.0.1 127.0.0.1 10000 80\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost 10000
        sb <- localhost 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_INET $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testOldHaProxy6 :: Test
testOldHaProxy6 = testCase "test/old_ha_proxy6" $ do
    sa <- localhost6 1111
    sb <- localhost6 2222
    runInput "PROXY TCP6 ::1 ::1 10000 80\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost6 10000
        sb <- localhost6 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_INET6 $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testOldHaProxyLocal :: Test
testOldHaProxyLocal = testCase "test/old_ha_proxy_local" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    runInput "PROXY UNKNOWN\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost 1111
        sb <- localhost 2222
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_INET $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testOldHaProxyLocal6 :: Test
testOldHaProxyLocal6 = testCase "test/old_ha_proxy_local6" $ do
    sa <- localhost6 1111
    sb <- localhost6 2222
    runInput "PROXY UNKNOWN\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost6 1111
        sb <- localhost6 2222
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_INET6 $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testOldHaProxyFailure :: Test
testOldHaProxyFailure = testCase "test/old_ha_proxy_failure" $ do
     sa <- localhost 1111
     sb <- localhost 2222
     expectException "bad family"
       $ runInput "PROXY ZZZ qqqq wwww 10000 80\r\nblah" sa sb action
     expectException "short"
       $ runInput "PROXY TCP4 \r\nblah" sa sb action
     expectException "non-integral"
       $ runInput "PROXY TCP4 127.0.0.1 127.0.0.1 xxx yyy\r\nblah" sa sb action
  where
    action _ _ _ = return ()


------------------------------------------------------------------------------
testOldHaProxyBadAddress :: Test
testOldHaProxyBadAddress = testCase "test/old_ha_proxy_bad_address" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    expectException "bad address" $
      runInput "PROXY TCP4 @~!@#$%^ (*^%$ 10000 80\r\nblah" sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost 10000
        sb <- localhost 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
protocolHeader :: ByteString
protocolHeader = S8.pack [ 0x0D, 0x0A, 0x0D, 0x0A, 0x00, 0x0D
                         , 0x0A, 0x51, 0x55, 0x49, 0x54, 0x0A ]
{-# NOINLINE protocolHeader #-}


------------------------------------------------------------------------------
testNewHaProxy :: Test
testNewHaProxy = testCase "test/new_ha_proxy" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    let input = S.concat [ protocolHeader
                         , "\x21\x11"    -- TCP over v4
                         , "\x00\x0c"        -- 12 bytes of address (network ordered)
                         , "\x7f\x00\x00\x01" -- localhost
                         , "\x7f\x00\x00\x01"
                         , "\x27\x10" -- 10000 in network order
                         , "\x00\x50" -- 80 in network order
                         , "blah"     -- the rest
                         ]
    runInput input sa sb action

  where
    action proxyInfo !is !_ = do
        sa <- localhost 10000
        sb <- localhost 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_INET $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
#ifndef WINDOWS
unixPath :: ByteString -> ByteString
unixPath s = S.append s (S.replicate (108 - S.length s) '\x00')


------------------------------------------------------------------------------
unixSock :: ByteString -> N.SockAddr
unixSock = N.SockAddrUnix . S.unpack


------------------------------------------------------------------------------
testNewHaProxyUnix :: Test
testNewHaProxyUnix = testCase "test/new_ha_proxy_unix" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    let input = S.concat [ protocolHeader
                         , "\x21\x31"    -- unix stream
                         , "\x00\xd8"    -- 216 bytes
                         , unixPath "/foo"
                         , unixPath "/bar"
                         , "blah"     -- the rest
                         ]
    runInput input sa sb action

    let input2 = S.concat [ protocolHeader
                          , "\x21\x32"    -- unix datagram
                          , "\x00\xd8"    -- 216 bytes
                          , unixPath "/foo"
                          , unixPath "/bar"
                          , "blah"     -- the rest
                          ]
    runInput input2 sa sb action2
  where
    action proxyInfo !is !_ = do
        let sa = unixSock "/foo"
        let sb = unixSock "/bar"
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_UNIX $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]

    action2 proxyInfo !is !_ = do
        let sa = unixSock "/foo"
        let sb = unixSock "/bar"
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_UNIX $ HA.getFamily proxyInfo
        assertEqual "stype" N.Datagram $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]
#endif

------------------------------------------------------------------------------
testNewHaProxy6 :: Test
testNewHaProxy6 = testCase "test/new_ha_proxy_6" $ do
    sa <- localhost6 1111
    sb <- localhost6 2222
    let input = S.concat [ protocolHeader
                         , "\x21\x21"    -- TCP over v6
                         , "\x00\x24"        -- 36 bytes of address (network ordered)
                         , lhBinary
                         , lhBinary
                         , "\x27\x10" -- 10000 in network order
                         , "\x00\x50" -- 80 in network order
                         , "blah"     -- the rest
                         ]
    runInput input sa sb action

  where
    lhBinary = S.concat [ S.replicate 15 '\x00', S.singleton '\x01' ]

    action proxyInfo !is !_ = do
        sa <- localhost6 10000
        sb <- localhost6 80
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_INET6 $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]


------------------------------------------------------------------------------
testNewHaProxyBadVersion :: Test
testNewHaProxyBadVersion = testCase "test/new_ha_proxy_bad_version" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    let input = S.concat [ protocolHeader
                         , "\x31\x11"    -- TCP over v4, bad version
                         , "\x00\x0c"        -- 12 bytes of address (network ordered)
                         , "\x7f\x00\x00\x01" -- localhost
                         , "\x7f\x00\x00\x01"
                         , "\x27\x10" -- 10000 in network order
                         , "\x00\x50" -- 80 in network order
                         , "blah"     -- the rest
                         ]
    expectException "bad version" $ runInput input sa sb action

    let input2 = S.concat [ protocolHeader
                          , "\x2F\x11"    -- TCP over v4, bad command
                          , "\x00\x0c"        -- 12 bytes of address (network ordered)
                          , "\x7f\x00\x00\x01" -- localhost
                          , "\x7f\x00\x00\x01"
                          , "\x27\x10" -- 10000 in network order
                          , "\x00\x50" -- 80 in network order
                          , "blah"     -- the rest
                          ]
    expectException "bad version" $ runInput input2 sa sb action

    let input3 = S.concat [ protocolHeader
                          , "\x21\x41"    -- bad family
                          , "\x00\x0c"        -- 12 bytes of address (network ordered)
                          , "\x7f\x00\x00\x01" -- localhost
                          , "\x7f\x00\x00\x01"
                          , "\x27\x10" -- 10000 in network order
                          , "\x00\x50" -- 80 in network order
                          , "blah"     -- the rest
                          ]
    expectException "bad family" $ runInput input3 sa sb action

    let input4 = S.concat [ protocolHeader
                          , "\x21\x14"    -- bad family
                          , "\x00\x0c"        -- 12 bytes of address (network ordered)
                          , "\x7f\x00\x00\x01" -- localhost
                          , "\x7f\x00\x00\x01"
                          , "\x27\x10" -- 10000 in network order
                          , "\x00\x50" -- 80 in network order
                          , "blah"     -- the rest
                          ]
    expectException "bad type" $ runInput input4 sa sb action
  where
    action _ !_ !_ = return ()


------------------------------------------------------------------------------
testNewHaProxyTooSmall :: Test
testNewHaProxyTooSmall = testCase "test/new_ha_proxy_too_small" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    let input = S.concat [ protocolHeader
                         , "\x21\x11"    -- TCP over v4
                         , "\x00\x02"        -- 2 bytes
                         , "\x00\x00"
                         , "blah"     -- the rest
                         ]
    expectException "too small" $ runInput input sa sb action
    let input2 = S.concat [ protocolHeader
                          , "\x21\x21"    -- TCP over v6
                          , "\x00\x02"        -- 2 bytes
                          , "\x00\x00"
                          , "blah"     -- the rest
                          ]
    expectException "too small" $ runInput input2 sa sb action
#ifndef WINDOWS
    let input3 = S.concat [ protocolHeader
                          , "\x21\x31"    -- unix
                          , "\x00\x02"        -- 2 bytes
                          , "\x00\x00"
                          , "blah"     -- the rest
                          ]
    expectException "too small" $ runInput input3 sa sb action
#endif
  where
    action _ !_ !_ = return ()


------------------------------------------------------------------------------
testNewHaProxyTooBig :: Test
testNewHaProxyTooBig = testCase "test/new_ha_proxy_too_big" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    let input = S.concat [ protocolHeader
                         , "\x21\x11"    -- TCP over v4
                         , "\x03\x0c"        -- 780: 12 bytes of address
                                             -- (network ordered) plus 768
                                             -- bytes of slop
                         , "\x7f\x00\x00\x01" -- localhost
                         , "\x7f\x00\x00\x01"
                         , S.replicate 768 '0'
                         , "\x27\x10" -- 10000 in network order
                         , "\x00\x50" -- 80 in network order
                         , "blah"     -- the rest
                         ]
    expectException "too big" $ runInput input sa sb action

    let input2 = S.concat [ protocolHeader
                          , "\x21\x00"    -- TCP over v4
                          , "\x03\x0c"        -- 780: 12 bytes of address
                                              -- (network ordered) plus 768
                                              -- bytes of slop
                         , "\x7f\x00\x00\x01" -- localhost
                         , "\x7f\x00\x00\x01"
                         , S.replicate 768 '0'
                         , "\x27\x10" -- 10000 in network order
                         , "\x00\x50" -- 80 in network order
                         , "blah"     -- the rest
                         ]
    expectException "too big" $ runInput input2 sa sb action

  where
    action _ !_ !_ = return ()


------------------------------------------------------------------------------
testNewHaProxyLocal :: Test
testNewHaProxyLocal = testCase "test/new_ha_proxy_local" $ do
    sa <- localhost 1111
    sb <- localhost 2222
    let input = S.concat [ protocolHeader
                         , "\x20\x00"    -- LOCAL UNSPEC
                         , "\x00\x00"        -- 0 bytes of address (network ordered)
                         , "blah"     -- the rest
                         ]
    runInput input sa sb action

#ifndef WINDOWS
    let ua = unixSock "/foo"
    let ub = unixSock "/bar"

    let input2 = S.concat [ protocolHeader
                          , "\x20\x00"    -- LOCAL UNSPEC
                          , "\x00\x00"        -- 0 bytes of address (network ordered)
                          , "blah"     -- the rest
                          ]
    runInput input2 ua ub action2
#endif
  where
    action proxyInfo !is !_ = do
        sa <- localhost 1111
        sb <- localhost 2222
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_INET $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]

#ifndef WINDOWS
    action2 proxyInfo !is !_ = do
        let sa = unixSock "/foo"
        let sb = unixSock "/bar"
        assertEqual "src addr" sa $ HA.getSourceAddr proxyInfo
        assertEqual "dest addr" sb $ HA.getDestAddr proxyInfo
        assertEqual "family" N.AF_UNIX $ HA.getFamily proxyInfo
        assertEqual "stype" N.Stream $ HA.getSocketType proxyInfo
        Streams.toList is >>= assertEqual "rest" ["blah"]
#endif


------------------------------------------------------------------------------
testGetSockAddr :: Test
testGetSockAddr = testCase "test/address/getSockAddr" $ do
    (f1, a1) <- getSockAddr 10 "127.0.0.1"
    x1 <- localhost 10
    assertEqual "f1" f1 N.AF_INET
    assertEqual "x1" x1 a1

    (f2, a2) <- getSockAddr 10 "::1"
    x2 <- localhost6 10
    assertEqual "f2" f2 N.AF_INET6
    assertEqual "x2" x2 a2

    expectException "empty result" $
      getSockAddrImpl (\_ _ _ -> return []) 10 "foo"


------------------------------------------------------------------------------
testTrivials :: Test
testTrivials = testCase "test/trivials" $ do
    coverShowInstance $ HA.makeProxyInfo undefined undefined undefined undefined
    coverTypeableInstance $ AddressNotSupportedException undefined
    coverShowInstance $ AddressNotSupportedException "ok"


------------------------------------------------------------------------------
localhost :: Int -> IO (N.SockAddr)
localhost p = snd <$> getSockAddr p "127.0.0.1"


------------------------------------------------------------------------------
localhost6 :: Int -> IO (N.SockAddr)
localhost6 p = snd <$> getSockAddr p "::1"


------------------------------------------------------------------------------
expectException :: String -> IO a -> IO ()
expectException name act = do
    e <- E.try act
    case e of
      Left (z::E.SomeException) -> (length $ show z) `seq` return ()
      Right _ -> fail $ name ++ ": expected exception, didn't get one"

------------------------------------------------------------------------------
coverTypeableInstance :: (Monad m, Typeable a) => a -> m ()
coverTypeableInstance a = typeOf a `seq` return ()


------------------------------------------------------------------------------
eatException :: IO a -> IO ()
eatException a = (a >> return ()) `E.catch` handler
  where
    handler :: E.SomeException -> IO ()
    handler _ = return ()


------------------------------------------------------------------------------
-- | Kill the false negative on derived show instances.
coverShowInstance :: (MonadIO m, Show a) => a -> m ()
coverShowInstance x = liftIO (a >> b >> c)
  where
    a = eatException $ E.evaluate $ length $ showsPrec 0 x ""
    b = eatException $ E.evaluate $ length $ show x
    c = eatException $ E.evaluate $ length $ showList [x] ""
