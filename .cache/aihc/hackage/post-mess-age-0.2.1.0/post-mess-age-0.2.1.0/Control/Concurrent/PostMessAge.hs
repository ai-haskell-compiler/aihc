
-- | Mechanism to get messages sent to a handle concurrently
--   without getting them mixed. They are sent to the handle in the same
--   order they are received by a /passer object/ (see 'Passer'), not
--   sending a message before the previous message is sent completely.
module Control.Concurrent.PostMessAge (
    -- * Passer type
    Passer
    -- * Open/Close a passer
  , createPasser
  , closePasser
    -- * Send messages to the passer
  , postMessage
    -- * Check passer status
  , isPasserClosed
  , isPasserOpen
    -- * Handle
  , passerHandle
  ) where

import Control.Monad (when)
import Control.Concurrent

data PasserStatus = Open | Closed

-- | The 'Passer' is the object that you send the messages to.
--   It will redirect this message to its attached handle,
--   making sure the messages are not intercalated.
--   Use 'postMessage' to send message to a passer object.
data Passer handle msg = Passer
  { passerStatus  :: MVar PasserStatus
  , passerHandle  :: handle -- ^ Return the handle used by a passer object.
  , passerChannel :: Chan (Maybe msg)
    }

-- | Passer object feeder loop.
feedHandle :: (handle -> msg -> IO ()) -> Passer handle msg -> IO ()
feedHandle f p = loop
  where
    ch = passerChannel p
    h = passerHandle p
    loop = do
      mx <- readChan ch
      case mx of
        Nothing -> return ()
        Just x -> f h x >> loop

-- | Check if a passer object is closed. When a passer object
--   is closed, it won't send any more messages to its attached
--   handle. This does not mean the handle itself is closed.
isPasserClosed :: Passer handle msg -> IO Bool
isPasserClosed p = do
  st <- readMVar $ passerStatus p
  return $ case st of
    Closed -> True
    _ -> False

-- | Check if a passer object is open. While a passer object
--   is open, all the messages received by the passer are
--   sent to its attached handle.
isPasserOpen :: Passer handle msg -> IO Bool
isPasserOpen = fmap not . isPasserClosed

-- | Send a message to a passer object. It returns a value
--   indicating if the message reached the passer object.
postMessage :: Passer handle msg -> msg -> IO Bool
postMessage p x = do
  b <- isPasserOpen p
  when b $ writeChan (passerChannel p) $ Just x
  return b

-- | Close a passer object, so it won't receive any more messages
--   in the future. Once a passer object is closed, it can't be
--   re-opened again. If you want to reuse a handle, create another
--   passer object with 'createPasser'.
closePasser :: Passer handle msg -> IO ()
closePasser p = do
  st <- swapMVar (passerStatus p) Closed
  case st of
    Closed -> return ()
    Open -> writeChan (passerChannel p) Nothing

-- | Create a passer object from a handle and a function to send
--   values to that handle.
--
--   Example:
--
-- > createPasser stderr hPutStrLn
--
createPasser :: handle -- ^ Handle to use
             -> (handle -> msg -> IO ()) -- ^ Function to send messages to the handle
             -> IO (Passer handle msg)
createPasser h f = do
  stv <- newMVar Open
  ch <- newChan
  let p = Passer stv h ch
  _ <- forkIO $ feedHandle f p
  return p
