import Common (handleException, )

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

main :: IO ()
main = handleException $ do
   putStrLn "Starting."
   SndSeq.withDefault SndSeq.Block $ \h -> do
      Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Haskell-Broadcast"
      c <- Client.getId h
      putStrLn ("Created sequencer with id: " ++ show c)
      -- dst <- Addr.parse h "HS1:255"
      let dst = Addr.broadcast
      print dst
      let ev n =
             (Event.simple
                (Addr.Cons c Port.unknown)
                (Event.NoteEv n $
                 Event.simpleNote (Event.Channel 0) (Event.Pitch 60) Event.normalVelocity))
             {Event.dest = dst}
      _ <- Event.outputDirect h $ ev Event.NoteOn
      _ <- getChar
      _ <- Event.outputDirect h $ ev Event.NoteOff
      return ()
