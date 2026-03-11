{- |
Play two precisely timed beats simultaneously
where the speed can be controlled by MIDI controllers.

Whenever the speed is changed we have to cancel
the events that are already scheduled.
So we use this example to demonstrate removing output events.
-}
import Common (handleExceptionCont, )

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event.RemoveMonad as Remove
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified System.Exit as Exit
import qualified System.IO as IO
import System.Environment (getArgs, )

import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (mplus, )
import Data.Maybe.HT (toMaybe, )


data Message = Echo Event.Tag | Tempo Event.Tag Event.Value Double
  deriving (Show)


data Track =
  Track {
    tag :: Event.Tag,
    pitch :: Event.Pitch,
    defaultPeriod, range :: Double,
    cc :: Event.Parameter
  }


trackA, trackB :: Track

trackA =
  Track {
    tag = Event.Tag 0,
    pitch = Event.Pitch 60,
    defaultPeriod = 1,
    range = 2,
    cc = Event.Parameter 16
  }

trackB =
  Track {
    tag = Event.Tag 1,
    pitch = Event.Pitch 64,
    defaultPeriod = 1/7,
    range = 2,
    cc = Event.Parameter 17
  }


data State = State { _lastTime, _remainingPortion, _period :: Double }

initState :: Track -> State
initState track = State 0 0 (defaultPeriod track)


main :: IO ()
main = handleExceptionCont $ do
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Haskell-Beat"
  public <-
     ContT $ Port.withSimple h "inout"
        (Port.caps [Port.capRead, Port.capSubsRead,
                    Port.capWrite, Port.capSubsWrite])
        (Port.types [Port.typeMidiGeneric, Port.typeApplication])
  private <-
     ContT $ Port.withSimple h "private"
        (Port.caps [Port.capRead, Port.capWrite])
        (Port.types [Port.typeMidiGeneric])
  q <- ContT $ Queue.with h
  liftIO $ mainIO h public private q

mainIO :: SndSeq.T SndSeq.DuplexMode -> Port.T -> Port.T -> Queue.T -> IO ()
mainIO h public private q = do
  PortInfo.modify h public $ do
     PortInfo.setTimestamping True
     PortInfo.setTimestampReal True
     PortInfo.setTimestampQueue q

  c <- Client.getId h
  let publicAddr  = Addr.Cons c public
      privateAddr = Addr.Cons c private
  args <- getArgs
  case args of
     [input, output] ->
        (Connect.createFrom h public =<< Addr.parse h input)
        >>
        (Connect.createTo   h public =<< Addr.parse h output)
        >>
        return ()
     _ ->
       IO.hPutStrLn IO.stderr "need arguments: input-client output-client"
       >>
       Exit.exitFailure

  let mkEv tg t e =
         (Event.simple publicAddr e) {
             Event.tag = tg,
             Event.queue = q,
             Event.time = Time.consAbs $ Time.Real $ RealTime.fromDouble t
          }

      play tg t onoff p =
         (Event.output h $ mkEv tg t $ Event.NoteEv onoff $
          Event.simpleNote (Event.Channel 0) p Event.normalVelocity)
         >>
         return ()

      echo tg t =
         Event.output h
            ((mkEv tg t $ Event.CustomEv Event.Echo $ Event.Custom 0 0 0) {
               Event.dest = privateAddr
            })
         >>
         return ()

  Queue.control h q Event.QueueStart Nothing

  let start track = do
         play (tag track) 0 Event.NoteOn (pitch track)
         echo (tag track) 0

  start trackA
  start trackB
  _ <- Event.drainOutput h

  let checkCC ctrl track =
         toMaybe (Event.ctrlParam ctrl == cc track) (tag track)

      wait = do
         ev <- Event.input h
         case Event.body ev of
            Event.CustomEv Event.Echo _ ->
               if Event.dest ev == privateAddr
                 then return $ Echo $ Event.tag ev
                 else wait
            Event.CtrlEv Event.Controller ctrl ->
               case mplus (checkCC ctrl trackA) (checkCC ctrl trackB) of
                  Just tg ->
                     case Event.time ev of
                        Time.Cons Time.Absolute (Time.Real t) ->
                           return $ Tempo tg (Event.ctrlValue ctrl) $
                              RealTime.toDouble t
                        _ -> error "got time in a format that we did not request"
                  Nothing -> wait
            _ -> wait

  let schedule track t = do
         play (tag track) t Event.NoteOff (pitch track)
         play (tag track) t Event.NoteOn  (pitch track)
         echo (tag track) t
         _ <- Event.drainOutput h
         return ()

  {-
  Cancel the Echo and Notes we already sent,
  and replace them by ones with updated timestamp.
  -}
  let tempo track val t1 (State t0 r0 p0) = do
         Remove.run h $ do
            Remove.setOutput
            Remove.setTag $ tag track
         let r1 = r0 - (t1-t0) / p0
         let p1 = defaultPeriod track *
                     range track ** ((fromIntegral val - 64) / 64)
         schedule track $ t1 + r1 * p1
         return (State t1 r1 p1)

  let next track (State t0 r p) =
         let {-
             t1 should be the current time.
             In principle we could use the timestamp from the Echo message,
             but this will be slightly later than the reference time.
             -}
             t1 = t0 + r*p
         in  schedule track (t1+p) >> return (State t1 1 p)

  let go (sa, sb) = do
         msg <- wait
         case msg of
            Echo tg ->
               if tg == tag trackA
                 then next trackA sa >>= \s -> go (s, sb)
                 else next trackB sb >>= \s -> go (sa, s)
            Tempo tg (Event.Value val) t ->
               if tg == tag trackA
                 then tempo trackA val t sa >>= \s -> go (s, sb)
                 else tempo trackB val t sb >>= \s -> go (sa, s)

  go (initState trackA, initState trackB)
