import Control.Exception (assert)
import qualified Control.Exception as E
import Data.Either (isRight)
import Network.Socket hiding (KeepAlive)
import Network.Socket.KeepAlive

main :: IO ()
main = do
  addr <- resolve
  -- sock is a Socket type
  E.bracketOnError (openSocket addr) close $ \sock -> do
    withFdSocket sock $ \fd -> do
      -- set keep alive on, idle 60 seconds, interval 2 seconds
      _ <- assert . isRight <$> (setKeepAlive fd $ KeepAlive True 60 2)
      _ <- assert <$> getKeepAliveOnOff fd
      _ <- assert . isRight <$> (setKeepAlive fd $ KeepAlive False 60 2)
      _ <- assert . not <$> getKeepAliveOnOff fd
      return ()
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) (Just "localhost") Nothing
