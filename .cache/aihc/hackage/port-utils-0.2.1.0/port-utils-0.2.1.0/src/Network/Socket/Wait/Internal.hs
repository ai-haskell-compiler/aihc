{-|
  This internal modules exposes additional functions for testing.
  Use 'Network.Socket.Wait' instead
|-}
module Network.Socket.Wait.Internal
  ( -- * Simple Wait Api
    wait
    -- * Advanced Wait Api
  , waitWith
  , EventHandlers (..)
  , defaultDelay
  , connectAction
  , waitM
  ) where
import qualified Network.Socket as N
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified System.IO.Error as IOE

-------------------------------------------------------------------------------
-- Simple Api
-------------------------------------------------------------------------------

-- | 'wait' will attempt to connect to the given host and port repeated every
-- 10 milliseconds until it is successful. It will throw an 'IOError' if the
-- host cannot be resolved.
--
-- A typical use case is to call 'wait' in test code to wait for a server to
-- start before trying to connect. For example:
--
-- @
--    void $ forkIO $ Warp.run 7000 app
--    -- Wait for the server to start listening on the socket
--    wait "127.0.0.1" 7000
--    -- Communicate with the server
-- @
--
-- If you would like to control the delay or understand how many connection
-- attempts were made use 'waitWith'.
--
-- Since 0.0.0.1
wait :: String
     -- ^ Host
     -> Int
     -- ^ Port
     -> IO ()
wait = waitWith mempty defaultDelay

-------------------------------------------------------------------------------
-- Advanced Api
-------------------------------------------------------------------------------
-- | The default delay between retries is 10000 microseconds (10 ms)
defaultDelay :: Int
defaultDelay = 10000

-- | The 'EventHandlers' is a record of 'IO' actions that are called when
--   interesting events occur in the lifecycle of the 'waitWith' loop.
--   One can pass in custom 'EventHandlers' values to implement logging
--   and other forms of instrumentation.
--
--   For example for debug one could print out each step using:
--
-- @
--    printHandlers = EventHandlers
--      { acting     = putStrLn "acting"
--      , delaying   = putStrLn "delaying"
--      , restarting = putStrLn "restarting"
--      }
-- @
--
data EventHandlers m = EventHandlers
  { connecting :: m ()
  -- ^ Called before the socket is created for connection
  , delaying :: m ()
  -- ^ Called after a failed attempt to connect before the thread
  -- is put to sleep.
  , restarting :: m ()
  -- ^ Called before a recursive call to restart the connection attempt
  }

instance Semigroup (m ()) => Semigroup (EventHandlers m) where
  x <> y = EventHandlers
    { connecting = connecting x <> connecting y
    , delaying   = delaying   x <> delaying   y
    , restarting = restarting x <> restarting y
    }

instance Monoid (m ()) => Monoid (EventHandlers m) where
  mempty  = EventHandlers mempty mempty mempty
  mappend = (<>)

-- | Advanced usage. In most situations calling 'wait' will suffice. This allows
-- one to customize the delay between retries and debug the behavior of the
-- function. 'wait' is defined as
--
-- @
--    wait = waitWith mempty defaultDelay
-- @
--
-- Since 0.2.0.0
waitWith :: EventHandlers IO
         -- ^ A record of IO actions that are called during sp
         -> Int
         -- ^ Microseconds to delay
         -> String
         -- ^ Host
         -> Int
         -- ^ Port
         -> IO ()
waitWith eh delay host port
  = if port < 1
      then E.throwIO $ IOE.mkIOError
            IOE.illegalOperationErrorType
            ("Invalid port! " <> show port <> " is less than 1")
            Nothing
            Nothing
      else waitM eh (C.threadDelay delay) (connectAction host port)

-- | This function loops if the second argument returns 'False'. Between the
--   recursive calls it will call it's first argument.
waitM :: Monad m => EventHandlers m -> m () -> m Bool -> m ()
waitM eh@EventHandlers {..} delayer action = do
  connecting
  action >>= \case
    True -> pure ()
    False -> do
      delaying
      delayer

      restarting
      waitM eh delayer action

-- | This function attempts to initiate a TCP connection to a given host and
--   port. If the host does not exist it throws an IOError. If the connection
--   fails it returns a 'False'. If it connects it returns 'True'.
connectAction :: String -> Int -> IO Bool
connectAction host port = do
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  -- getAddrInfo returns a non-empty array or throws per the doc
  addr:_ <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)
  e <- E.try $ E.bracket
        (N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr))
        N.close
        $ \sock -> N.connect sock $ N.addrAddress addr

  pure $ case e of
    Left (_ :: IOError) -> False
    Right _ -> True
