import Common (handleException, runContUnit, )

import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (forever, )

main :: IO ()
main = handleException $ runContUnit $ do
   liftIO $ putStrLn "Starting."
   h <- ContT $ SndSeq.withDefault SndSeq.Block
   liftIO $ Client.setName (h :: SndSeq.T SndSeq.InputMode) "Haskell-Dump"
   liftIO $ putStrLn "Created sequencer."
   _p1 <-
      ContT $
      Port.withSimple h "primary"
         (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric
   _p2 <-
      ContT $
      Port.withSimple h "secondary"
         (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric
   liftIO $ putStrLn "Created ports."
   liftIO $ forever $ do
      putStrLn "waiting for an event:"
      print =<< Event.input h
