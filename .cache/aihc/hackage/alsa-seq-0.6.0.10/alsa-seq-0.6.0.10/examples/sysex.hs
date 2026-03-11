import Common (handleExceptionCont, parseDestArgs, )

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )

import qualified Data.ByteString as B

import System.Environment (getArgs, )


main :: IO ()
main = handleExceptionCont $ do
  liftIO $ putStrLn "Starting."
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Haskell-SysEx"
  p <-
     ContT $
     Port.withSimple h "out"
        (Port.caps [Port.capRead, Port.capSubsRead,
                    Port.capWrite, Port.capSubsWrite])
        Port.typeMidiGeneric
  liftIO $ mainIO h p

mainIO :: SndSeq.T SndSeq.DuplexMode -> Port.T -> IO ()
mainIO h p = do
  c <- Client.getId h
  putStrLn ("Created sequencer with id: " ++ show c)
  let me = Addr.Cons c p
  conn <- parseDestArgs h me =<< getArgs

  putStrLn "enter a text line for submission as SysEx message"
  text <- getLine
  let dat =
         Event.ExtEv Event.SysEx $
         B.pack $ map (fromIntegral . fromEnum) text
  _ <- Event.outputDirect h $ Event.forConnection conn dat
  _ <- Event.outputDirect h $ Event.forConnection (Connect.Cons me me) dat
  let waitForSysEx = do
         ev <- Event.input h
         case Event.body ev of
            Event.ExtEv Event.SysEx str ->
               putStrLn $ "got: " ++ show str
            _ -> waitForSysEx
  waitForSysEx
  return ()
