module Reactive.Banana.ALSA.Common where

import qualified Reactive.Banana.ALSA.Private as Priv
import Reactive.Banana.ALSA.Private (Handle(..), )

import qualified Reactive.Banana.ALSA.Time as AlsaTime
import qualified Reactive.Banana.MIDI.Time as Time

import qualified Reactive.Banana.MIDI.Note as Note
import qualified Reactive.Banana.MIDI.Common as Common
import Reactive.Banana.MIDI.Common (VelocityField, singletonBundle, )

import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Time as ATime

import qualified Control.Exception.Extensible as Exc
import qualified Sound.ALSA.Exception as AExc
import qualified Foreign.C.Error as Err

import qualified Sound.MIDI.ALSA as MALSA
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import Sound.MIDI.ALSA.Construct ()
import Sound.MIDI.ALSA.Query ()
import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Velocity, Pitch, Controller, Program, )

import Data.Accessor.Basic ((^.), (^=), )

import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.Reader (ReaderT, )
import Control.Functor.HT (void, )

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Monoid as Mn
import Data.Foldable (Foldable, foldMap, )
import Data.Maybe (maybeToList, )
import Data.List (intercalate, )

import Prelude hiding (init, filter, reverse, )


-- * helper functions

init :: IO Handle
init = do
   h <- SndSeq.open SndSeq.defaultName SndSeq.Block
   Client.setName h "Haskell-Filter"
   c <- Client.getId h
   ppublic <-
      Port.createSimple h "inout"
         (Port.caps [Port.capRead, Port.capSubsRead,
                     Port.capWrite, Port.capSubsWrite])
         Port.typeMidiGeneric
   pprivate <-
      Port.createSimple h "private"
         (Port.caps [Port.capRead, Port.capWrite])
         Port.typeMidiGeneric
   q <- Queue.alloc h
   let hnd = Handle h c ppublic pprivate q
   Reader.runReaderT setTimeStamping hnd
   return hnd

exit :: Handle -> IO ()
exit h = do
   void $ Event.outputPending (sequ h)
   Queue.free (sequ h) (queue h)
   Port.delete (sequ h) (portPublic h)
   Port.delete (sequ h) (portPrivate h)
   SndSeq.close (sequ h)

with :: ReaderT Handle IO a -> IO a
with f =
   SndSeq.with SndSeq.defaultName SndSeq.Block $ \h -> do
   Client.setName h "Haskell-Filter"
   c <- Client.getId h
   Port.withSimple h "inout"
         (Port.caps [Port.capRead, Port.capSubsRead,
                     Port.capWrite, Port.capSubsWrite])
         Port.typeMidiGeneric $ \ppublic -> do
   Port.withSimple h "private"
         (Port.caps [Port.capRead, Port.capWrite])
         Port.typeMidiGeneric $ \pprivate -> do
   Queue.with h $ \q ->
      flip Reader.runReaderT (Handle h c ppublic pprivate q) $
      setTimeStamping >> f

-- | make ALSA set the time stamps in incoming events
setTimeStamping :: ReaderT Handle IO ()
setTimeStamping =
   Reader.ReaderT $ \h ->
   PortInfo.modify (sequ h) (portPublic h) $ do
      PortInfo.setTimestamping True
      PortInfo.setTimestampReal True
      PortInfo.setTimestampQueue (queue h)


startQueue :: ReaderT Handle IO ()
startQueue = Reader.ReaderT $ \h -> do
   Queue.control (sequ h) (queue h) Event.QueueStart Nothing
   void $ Event.drainOutput (sequ h)


{- |
Connect ourselve to an input client and an output client.
The function expects a list of alternative clients
that are checked successively.
-}
connect :: [String] -> [String] -> ReaderT Handle IO ()
connect fromNames toNames = do
   void $ connectFrom =<< parseAddresses fromNames
   void $ connectTo   =<< parseAddresses toNames

connectFrom, connectTo :: Addr.T -> ReaderT Handle IO Connect.T
connectFrom from = Reader.ReaderT $ \h ->
   Connect.createFrom (sequ h) (portPublic h) from
connectTo   to   = Reader.ReaderT $ \h ->
   Connect.createTo   (sequ h) (portPublic h) to

timidity, haskellSynth :: String
timidity = "TiMidity"
haskellSynth = "Haskell-LLVM-Synthesizer"

inputs, outputs :: [String]
inputs = ["ReMOTE SL", "E-MU Xboard61", "USB Midi Cable", "SAMSON Graphite 49"]
outputs = [timidity, haskellSynth, "Haskell-Synthesizer", "Haskell-Supercollider"]

connectTimidity :: ReaderT Handle IO ()
connectTimidity =
   connect inputs [timidity]

connectLLVM :: ReaderT Handle IO ()
connectLLVM =
   connect inputs [haskellSynth]

connectAny :: ReaderT Handle IO ()
connectAny =
   connect inputs outputs

parseAddresses :: [String] -> ReaderT Handle IO Addr.T
parseAddresses names = Reader.ReaderT $ \h ->
   let notFoundExc = Err.Errno 2
       go [] =
          Exc.throw $
          AExc.Cons
             "parseAdresses"
             ("could not find any of the clients: " ++ intercalate ", " names)
             notFoundExc
       go (x:xs) =
          AExc.catch (Addr.parse (sequ h) x) $
          \exc ->
             if AExc.code exc == notFoundExc
               then go xs
               else Exc.throw exc
   in  go names



-- * send single events

sendNote :: Channel -> AlsaTime.RelativeTicks -> Velocity -> Pitch -> ReaderT Handle IO ()
sendNote chan dur vel pit =
   let note = simpleNote chan pit vel
       z = Mn.mempty
       t = Time.inc dur z
   in  do outputEvent z (Event.NoteEv Event.NoteOn note)
          outputEvent t (Event.NoteEv Event.NoteOff note)

sendKey :: Channel -> Bool -> Velocity -> Pitch -> ReaderT Handle IO ()
sendKey chan noteOn vel pit =
   outputEvent Mn.mempty $
      Event.NoteEv
         (if noteOn then Event.NoteOn else Event.NoteOff)
         (simpleNote chan pit vel)

sendController :: Channel -> Controller -> Int -> ReaderT Handle IO ()
sendController chan ctrl val =
   outputEvent Mn.mempty $
      Event.CtrlEv Event.Controller $
      MALSA.controllerEvent chan ctrl (fromIntegral val)

sendProgram :: Channel -> Program -> ReaderT Handle IO ()
sendProgram chan pgm =
   outputEvent Mn.mempty $
      Event.CtrlEv Event.PgmChange $
      MALSA.programChangeEvent chan pgm

sendMode :: Channel -> Mode.T -> ReaderT Handle IO ()
sendMode chan mode =
   outputEvent Mn.mempty $
      Event.CtrlEv Event.Controller $
      MALSA.modeEvent chan mode


-- * events

class Reactor reactor where
   reactorTime :: Time.T reactor t a -> Time.T Priv.Reactor t a

instance Reactor Priv.Reactor where
   reactorTime = id

{- |
This class unifies several ways of handling multiple events at once.
-}
class Events ev where
   flattenEvents :: ev -> [Future Event.Data]

instance Events Event.Data where
   flattenEvents = singletonBundle

instance
   (Note.Make key, VelocityField value) =>
      Events (Note.Boundary key value) where
   flattenEvents = singletonBundle . Note.fromBnd

instance (Reactor m, Events ev) => Events (Common.Future m ev) where
   flattenEvents (Common.Future dt ev) =
      map
         (\(Common.Future t e) ->
            Common.Future (Mn.mappend t $ reactorTime dt) e) $
      flattenEvents ev

instance Events ev => Events (Maybe ev) where
   flattenEvents ev = maybe [] flattenEvents ev

instance Events ev => Events [ev] where
   flattenEvents = concatMap flattenEvents

instance (Foldable f, Events ev) => Events (NonEmpty.T f ev) where
   flattenEvents = foldMap flattenEvents

instance (Events ev0, Events ev1) => Events (ev0,ev1) where
   flattenEvents (ev0,ev1) = flattenEvents ev0 ++ flattenEvents ev1

instance (Events ev0, Events ev1, Events ev2) => Events (ev0,ev1,ev2) where
   flattenEvents (ev0,ev1,ev2) =
      flattenEvents ev0 ++ flattenEvents ev1 ++ flattenEvents ev2


makeEvent :: Handle -> AlsaTime.AbsoluteTicks -> Event.Data -> Event.T
makeEvent h t e =
   (Event.simple (Addr.Cons (client h) (portPublic h)) e)
      { Event.queue = queue h
      , Event.time = ATime.consAbs $ AlsaTime.toStamp t
      }

makeEcho :: Handle -> AlsaTime.AbsoluteTicks -> Event.T
makeEcho h t =
   let addr = Addr.Cons (client h) (portPrivate h)
   in  (Event.simple addr (Event.CustomEv Event.Echo (Event.Custom 0 0 0)))
          { Event.queue = queue h
          , Event.time = ATime.consAbs $ AlsaTime.toStamp t
          , Event.dest = addr
          }


outputEvent :: AlsaTime.AbsoluteTicks -> Event.Data -> ReaderT Handle IO ()
outputEvent t ev = Reader.ReaderT $ \h ->
   Event.output (sequ h) (makeEvent h t ev) >>
   void (Event.drainOutput (sequ h))


simpleNote :: Channel -> Pitch -> Velocity -> Event.Note
simpleNote c p v =
   Event.simpleNote
      (MALSA.fromChannel c)
      (MALSA.fromPitch p)
      (MALSA.fromVelocity v)


type Future = Common.Future Priv.Reactor
type Bundle a = Common.Bundle Priv.Reactor a
type EventBundle = Bundle Event.T
type EventDataBundle = Bundle Event.Data


-- * effects

setChannel ::
   Channel -> Event.Data -> Event.Data
setChannel chan e =
   case e of
      Event.NoteEv notePart note ->
         Event.NoteEv notePart $
         (MALSA.noteChannel ^= chan) note
      Event.CtrlEv ctrlPart ctrl ->
         Event.CtrlEv ctrlPart $
         (MALSA.ctrlChannel ^= chan) ctrl
      _ -> e


delayAdd ::
   Velocity -> AlsaTime.RelativeTicks -> Event.Data -> EventDataBundle
delayAdd decay d e =
   singletonBundle e ++
   (maybeToList $ fmap (Common.Future d) $
    Note.lift (Note.reduceVelocity decay) e)



-- * predicates - may be moved to midi-alsa package

controllerMatch ::
   Channel -> Controller -> Event.Ctrl -> Bool
controllerMatch chan ctrl param =
   Event.ctrlChannel param == MALSA.fromChannel chan &&
   Event.ctrlParam   param == MALSA.fromController ctrl

checkChannel ::
   (Channel -> Bool) ->
   (Event.Data -> Bool)
checkChannel p e =
   case e of
      Event.NoteEv _notePart note ->
         p (note ^. MALSA.noteChannel)
      Event.CtrlEv Event.Controller ctrl ->
         p (ctrl ^. MALSA.ctrlChannel)
      _ -> False

checkPitch ::
   (Pitch -> Bool) ->
   (Event.Data -> Bool)
checkPitch p e =
   case e of
      Event.NoteEv _notePart note ->
         p (note ^. MALSA.notePitch)
      _ -> False

checkController ::
   (Controller -> Bool) ->
   (Event.Data -> Bool)
checkController p e =
   case e of
      Event.CtrlEv Event.Controller ctrlMode ->
         case ctrlMode ^. MALSA.ctrlControllerMode of
            MALSA.Controller ctrl _ -> p ctrl
            _ -> False
      _ -> False

checkMode ::
   (Mode.T -> Bool) ->
   (Event.Data -> Bool)
checkMode p e =
   case e of
      Event.CtrlEv Event.Controller ctrlMode ->
         case ctrlMode ^. MALSA.ctrlControllerMode of
            MALSA.Mode mode -> p mode
            _ -> False
      _ -> False

checkProgram ::
   (Program -> Bool) ->
   (Event.Data -> Bool)
checkProgram p e =
   case e of
      Event.CtrlEv Event.PgmChange ctrl ->
         p (ctrl ^. MALSA.ctrlProgram)
      _ -> False


isAllNotesOff :: Event.Data -> Bool
isAllNotesOff =
   checkMode $ \mode ->
      mode == Mode.AllSoundOff ||
      mode == Mode.AllNotesOff
