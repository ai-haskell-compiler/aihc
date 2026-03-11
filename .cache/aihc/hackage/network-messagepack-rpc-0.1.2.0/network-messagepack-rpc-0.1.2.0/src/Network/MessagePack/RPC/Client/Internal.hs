module Network.MessagePack.RPC.Client.Internal where

import           Control.Concurrent      (ThreadId)
import           Control.Concurrent.MVar (MVar)
import qualified Data.ByteString         as B
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.IORef              (IORef)
import qualified Data.IORef              as IORef
import qualified Data.MessagePack        as MsgPack

import           Data.MessagePack.RPC

-- | A client data type for MessagePack RPC.
data Client = Client {
    clientSessionState :: !SessionState
  , clientBackend      :: !Backend
  , clientLog          :: Logger
  , clientFormat       :: Formatter
  , clientHandlerTid   :: IORef (Maybe ThreadId)
  }

data SessionState = SessionState {
    lastMessageId :: IORef MessageId
  , dispatchTable :: IORef (HashMap MessageId (MVar Result))
  }

-- | Backend IO functions.
--   Any receiving / sending actions are performed by calling these functions.
data Backend = Backend {
    backendSend  :: B.ByteString -> IO () -- ^ Sending
  , backendRecv  :: IO B.ByteString -- ^ Receiving
  , backendClose :: IO () -- ^ Closing
  }

-- | Logger type. Should print out the message passed as a first argument somewhere.
type Logger = String -> IO ()

-- | Convert 'Message' into a @String@ to print out by 'Logger'
type Formatter = Message -> String

-- | Result type of a RPC call.
--   Described as "error" and "result" of "Response Message"
--   in [the spec of MessagePack RPC](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md#response-message).
type Result = Either MsgPack.Object MsgPack.Object

initSessionState :: IO SessionState
initSessionState =
    SessionState <$> IORef.newIORef 0 <*> IORef.newIORef HM.empty
