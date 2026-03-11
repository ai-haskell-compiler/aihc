import Common (handleExceptionCont, parseDestArgs, )

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )

import System.Environment (getArgs, )


main :: IO ()
main = handleExceptionCont $ do
  liftIO $ putStrLn "Starting."
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Haskell-Send-Note"
  p <-
     ContT $
     Port.withSimple h "out"
        (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
  liftIO $ mainIO h p

mainIO :: SndSeq.T SndSeq.OutputMode -> Port.T -> IO ()
mainIO h p = do
  c <- Client.getId h
  putStrLn ("Created sequencer with id: " ++ show c)
  conn <- parseDestArgs h (Addr.Cons c p) =<< getArgs
  let ev n =
         Event.forConnection conn $ Event.NoteEv n $
            Event.simpleNote (Event.Channel 0) (Event.Pitch 60) Event.normalVelocity
  _ <- Event.outputDirect h $ ev Event.NoteOn
  putStrLn "press Enter for stopping the note"
  _ <- getChar
  _ <- Event.outputDirect h $ ev Event.NoteOff
  return ()
