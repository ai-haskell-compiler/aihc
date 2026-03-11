{- |
Play a melody.
Demonstrate how to wait on the end of a performance of events,
by sending an Echo message to ourselves.
-}
import Common (parseDestArgs, handleExceptionCont, )

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (zipWithM_, )

import System.Environment (getArgs, )


main :: IO ()
main = handleExceptionCont $ do
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Haskell-Melody"
  p <- ContT $
     Port.withSimple h "out"
        (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
        (Port.types [Port.typeMidiGeneric, Port.typeApplication])
  q <- ContT $ Queue.with h
  liftIO $ mainIO h p q

mainIO :: SndSeq.T SndSeq.DuplexMode -> Port.T -> Queue.T -> IO ()
mainIO h p q = do
  c <- Client.getId h
  let me = Addr.Cons c p
  conn <- parseDestArgs h me =<< getArgs

  let ev t e =
         (Event.forConnection conn e) {
             Event.queue = q,
             Event.time = Time.consAbs $ Time.Tick t
          }
      play t chan pitch =
         Event.output h (ev t $
            Event.NoteEv Event.NoteOn $ Event.simpleNote chan pitch Event.normalVelocity)
         >>
         Event.output h (ev (t+1) $
            Event.NoteEv Event.NoteOff $ Event.simpleNote chan pitch Event.normalVelocity)
         >>
         return ()

      echo t =
         fmap (const ()) $ Event.output h
            ((ev t $ Event.CustomEv Event.Echo $ Event.Custom 0 0 0){
               Event.dest = me
            })

  Queue.control h q Event.QueueStart Nothing
  Queue.control h q (Event.QueueTempo (Event.Tempo 10000000)) Nothing
  zipWithM_ (\t ->
        maybe (echo t) (play t (Event.Channel 0))) [0..] $
     (++[Nothing]) $
     concat $ concatMap (replicate 4 . map Just) $
     map (map Event.Pitch) $
        [57, 59, 60, 64] :
        [57, 59, 60, 65] :
        [57, 62, 64, 65] :
        [57, 59, 60, 64] :
        []
  _ <- Event.drainOutput h
  _ <- Event.outputPending h

--          threadDelay 10000000
  let waitForEcho = do
         event <- Event.input h
         print event
         case Event.body event of
            Event.CustomEv Event.Echo _d ->
               if Event.source event == me
                 then return ()
                 else waitForEcho
            _ -> waitForEcho
  waitForEcho
