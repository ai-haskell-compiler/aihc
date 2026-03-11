{-# LANGUAGE TupleSections #-}
module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (try, AsyncException(..))
import Control.Monad (forM_, foldM, filterM, when)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (delete, deleteBy)
import Data.Maybe (fromJust)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdin, BufferMode(..))

import qualified Netcode.IO as Netcode

--------------------------------------------------------------------------------

data Activated a = Activated
                   { inactive :: [Int]
                   , active :: [(Int, a)]
                   } deriving (Eq, Show, Ord)

instance Functor Activated where
    fmap f (Activated i xs) = Activated i (fmap (second f) xs)

instance Foldable Activated where
    foldMap f (Activated _ xs) = foldMap (f . snd) xs

instance Traversable Activated where
    traverse f (Activated i' xs) =
        Activated i' <$> traverse (\(i, x) -> (i,) <$> f x) xs

mkActivated :: Int -> Activated a
mkActivated n = Activated (take n [0..]) []

activate :: Int -> a -> Activated a -> Maybe (Activated a)
activate _ _ (Activated [] _) = Nothing
activate i x (Activated is xs)
    | i `notElem` is = Nothing
    | otherwise      = Just (Activated (delete i is) ((i, x):xs))

deactivate :: Int -> Activated a -> Maybe (Activated a)
deactivate i a@(Activated is xs)
    | isActive i a =
        Just $ Activated (i:is) $ deleteBy ((==) `on` fst) (i, undefined) xs
    | otherwise = 
        Nothing

isActive :: Int -> Activated a -> Bool
isActive i (Activated _ xs) = i `elem` (map fst xs)

--------------------------------------------------------------------------------

newtype RandomInts = RandomInts (IORef [Int])

mkRandomInts :: IO RandomInts
mkRandomInts = do
    let randInts = 4 : map ((`mod` 134456) . (+ 28411) . (* 8121)) randInts
    RandomInts <$> newIORef randInts

getNextInt :: RandomInts -> IO Int
getNextInt (RandomInts ints) = do
    (i:is) <- readIORef ints
    writeIORef ints is
    return i

filterRand :: RandomInts -> Int -> [a] -> IO [a]
filterRand ints modVal = (map snd <$>)
                        . filterM (fmap ((== 0) . (`mod` modVal)) . fst)
                        . zip (repeat $ getNextInt ints)

whenRandMod :: RandomInts -> Int -> IO () -> IO ()
whenRandMod ints modVal = whenM ((== 0) . (`mod` modVal) <$> getNextInt ints)

--------------------------------------------------------------------------------

gPrivateKey :: [Word8]
gPrivateKey =
    [ 0x60, 0x6a, 0xbe, 0x6e, 0xc9, 0x19, 0x10, 0xea
    , 0x9a, 0x65, 0x62, 0xf6, 0x6f, 0x2b, 0x30, 0xe4
    , 0x43, 0x71, 0xd6, 0x2c, 0xd1, 0x99, 0x27, 0x26
    , 0x6b, 0x3c, 0x60, 0xf4, 0xb7, 0x15, 0xab, 0xa1
    ]

gProtocolID :: Word64
gProtocolID = 0x1122334455667788

gPacketData :: [Word8]
gPacketData = [0..(Netcode.maximumPacketSize - 1)]

gMaxNumClients :: Int
gMaxNumClients = 64

gMaxNumServers :: Int
gMaxNumServers = 2

gServerBasePort :: Int
gServerBasePort = 20000

gConnectTokenExpiry :: Int
gConnectTokenExpiry = 30

gConnectTokenTimeout :: Int
gConnectTokenTimeout = 5

-------------------------------------------------------------------------------
whenM :: Monad m => m Bool -> m () -> m ()
whenM c p = do { b <- c; when b p }

untilM :: Monad m => m Bool -> m ()
untilM p = p >>= bool (untilM p) (return ())
-------------------------------------------------------------------------------

data Soak = Soak (Activated Netcode.Client) (Activated Netcode.Server)

initializeSoak :: IO Soak
initializeSoak = do
    putStrLn "initializing"
    Netcode.initialize
    Netcode.logLevel Netcode.LogLevel'Info

    return $ Soak (mkActivated gMaxNumClients) (mkActivated gMaxNumServers)

shutdownSoak :: Soak -> IO ()
shutdownSoak (Soak clients servers) = do
    putStrLn "shutdown"

    forM_ (map snd $ active clients) Netcode.destroyClient
    forM_ (map snd $ active servers) Netcode.destroyServer

    Netcode.terminate

decodePacket :: Maybe Netcode.Packet -> IO Bool
decodePacket Nothing = return True
decodePacket (Just pkt) =
    withForeignPtr (Netcode.packetDataPtr pkt) $ \pktMem -> do
        pktData <- peekArray (Netcode.packetSize pkt) pktMem
        case (and $ zipWith (==) pktData gPacketData) of
            True -> return False
            False -> fail "Received garbled packet!"

iterateSoak :: RandomInts -> IORef Soak -> Double -> IO ()
iterateSoak ints sr t = do
    (Soak clients servers) <- readIORef sr
    let serverConfig =
            Netcode.setPrivateKey gPrivateKey $
            Netcode.setProtocolID gProtocolID $
            Netcode.defaultServerConfig

        mkServer ss serverID = do
            let addr = "127.0.0.1:" ++ show (gServerBasePort + serverID)
            s <- Netcode.createServer addr serverConfig t
            putStrLn $ "created server " ++ show s
            return $ fromJust $ activate serverID s ss

    servers' <- filterRand ints 10 (inactive servers)
            >>= foldM mkServer servers

    let killServer ss (serverID, s) = do
            putStrLn $ "destroy server " ++ show s
            Netcode.destroyServer s
            return $ fromJust $ deactivate serverID ss

    servers'' <- filterM (Netcode.isServerFull . snd) (active servers)
             >>= filterRand ints 10000
             >>= foldM killServer servers'

    let mkClient cs clientID = do
            c <- Netcode.createClient "0.0.0.0" Netcode.defaultClientConfig t
            putStrLn $ "created client " ++ show c
            return $ fromJust $ activate clientID c cs

    clients' <- filterRand ints 10 (inactive clients)
            >>= foldM mkClient clients

    let killClient cs (clientID, c) = do
            putStrLn $ "destroy client " ++ show c
            Netcode.destroyClient c
            return $ fromJust $ deactivate clientID cs

    clients'' <- filterRand ints 1000 (active clients)
             >>= foldM killClient clients'
{--
    forM_ servers'' $ \s -> do
        _ <- getNextInt ints
        whenRandMod ints 10 $
            whenM (not <$> Netcode.isServerRunning s) $ do
                numClients <- (`mod` Netcode.maxNumClients) <$> getNextInt ints
                Netcode.startServer s (1 + numClients)

        maxClients <- Netcode.maxClientsForServer s
        whenRandMod ints 1000 $
            whenM ((== maxClients) <$> Netcode.numConnectedClients s) $
            whenM (Netcode.isServerRunning s) $
            Netcode.stopServer s

        whenM (Netcode.isServerRunning s) $ do
            forM_ [0..(maxClients - 1)] $ \clientIndex ->
                whenM (Netcode.clientConnectedAtIndex s clientIndex) $ do
                    pktSz <-
                        ((+ 1) . (`mod` Netcode.maximumPacketSize)) <$>
                        getNextInt ints
                    withArrayLen (take pktSz gPacketData) $
                        Netcode.sendPacketFromServer s clientIndex

            forM_ [0..(maxClients - 1)] $ \clientIndex ->
                whenM (Netcode.clientConnectedAtIndex s clientIndex) $
                untilM $ Netcode.receivePacketFromClient s clientIndex
                     >>= decodePacket

        Netcode.updateServer s t
--}
    forM_ clients'' $ \c -> do
        _ <- getNextInt ints
        whenRandMod ints 10 $ whenM (Netcode.isClientDisconnected c) $ do
            clientID <- Netcode.generateClientID
            userData <-
                mapM (fmap fromIntegral)
                     (replicate Netcode.maximumUserDataSize $ getNextInt ints)

            connectServers <-
                filterM (Netcode.isServerRunning . snd) (active servers'')

            let maxServers = Netcode.maximumServersPerConnect
                mkAddrPair i = 
                    let addr = "127.0.0.1:" <> show (gServerBasePort + i)
                     in (addr, addr)
                serverAddrs = take maxServers $
                              map (mkAddrPair . fst) connectServers

            when (not $ null serverAddrs) $
                Netcode.connectClient c =<<
                Netcode.generateConnectToken serverAddrs
                                             gConnectTokenExpiry 
                                             gConnectTokenTimeout
                                             clientID
                                             gProtocolID
                                             gPrivateKey
                                             userData

        let isConnected = (== Netcode.ClientState'Connected)
        whenRandMod ints 100 $
            whenM (isConnected <$> Netcode.getClientState c) $
            Netcode.disconnectClient c

        whenM (isConnected <$> Netcode.getClientState c) $ do
            pktSz <-
                ((+ 1) . (`mod` Netcode.maximumPacketSize)) <$>
                getNextInt ints
            withArrayLen (take pktSz gPacketData)
                         (Netcode.sendPacketFromClient c)

            untilM $ Netcode.receivePacketFromServer c >>= decodePacket

        Netcode.updateClient c t

    writeIORef sr $ Soak clients'' servers''

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    args <- getArgs
    let numIters :: Maybe Int
        numIters = case length args of
            1 -> Just $ read (head args)
            _ -> Nothing

        times = case numIters of
            Just x -> take x [0.0,0.1..]
            Nothing -> [0.0,0.1..]

    putStrLn "[soak]"
    putStrLn $ "num_iterations = " <> show numIters

    soak <- initializeSoak
    soakRef <- newIORef soak

    ints <- mkRandomInts

    soakResult <- try $ forM_ times $ iterateSoak ints soakRef

    case soakResult of
        (Left UserInterrupt) -> return ()
        (Left e) -> fail (show e)
        (Right ()) -> return ()

    readIORef soakRef >>= shutdownSoak
