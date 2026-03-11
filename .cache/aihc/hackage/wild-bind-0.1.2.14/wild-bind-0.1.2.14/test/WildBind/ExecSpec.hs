{-# LANGUAGE OverloadedStrings #-}
module WildBind.ExecSpec
    ( main
    , spec
    ) where

import           Control.Applicative       ((<$>))
import           Control.Concurrent        (forkIOWithUnmask, killThread, threadDelay)
import           Control.Concurrent.STM    (TChan, atomically, newTChanIO, readTChan, tryReadTChan,
                                            writeTChan)
import           Control.Exception         (bracket, fromException, throw)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Control.Monad.Trans.State as State
import           Data.Monoid               ((<>))
import           System.IO.Error           (userError)
import           Test.Hspec

import qualified WildBind.Binding          as WBB
import qualified WildBind.Exec             as WBE
import qualified WildBind.FrontEnd         as WBF

import           WildBind.ForTest          (SampleBackState (..), SampleInput (..),
                                            SampleState (..))

newtype EventChan s i
  = EventChan { unEventChan :: TChan (WBF.FrontEvent s i) }

data GrabHistory i
  = GSet i
  | GUnset i
  deriving (Eq, Ord, Show)

newtype GrabChan i
  = GrabChan { unGrabChan :: TChan (GrabHistory i) }

frontEnd :: EventChan s i -> GrabChan i -> WBF.FrontEnd s i
frontEnd echan gchan = WBF.FrontEnd
  { WBF.frontDefaultDescription = const "",
    WBF.frontSetGrab = \i -> atomically $ writeTChan (unGrabChan gchan) (GSet i),
    WBF.frontUnsetGrab = \i -> atomically $ writeTChan (unGrabChan gchan) (GUnset i),
    WBF.frontNextEvent = atomically $ readTChan $ unEventChan echan
  }

_write :: MonadIO m => TChan a -> a -> m ()
_write tc = liftIO . atomically . writeTChan tc

outChanOn :: MonadIO m => TChan a -> i -> a -> (i, WBB.Action m ())
outChanOn out_chan input out_elem = (input, WBB.Action "" (out_chan `_write` out_elem))

outChanOnS :: TChan a -> i -> a -> bs -> (i, WBB.Action (State.StateT bs IO) ())
outChanOnS out_chan input out_elem next_state = (,) input $ WBB.Action "" $ do
  liftIO $ atomically $ writeTChan out_chan out_elem
  State.put next_state

withWildBind' :: (WBF.FrontEnd s i -> IO ()) -> (EventChan s i -> GrabChan i -> IO ()) -> IO ()
withWildBind' exec action = do
  echan <- EventChan <$> newTChanIO
  gchan <- GrabChan <$> newTChanIO
  let spawnWildBind = forkIOWithUnmask $ \umask -> umask $ exec (frontEnd echan gchan)
  bracket spawnWildBind killThread (\_ -> action echan gchan)

withWildBind :: Ord i => WBB.Binding s i -> (EventChan s i -> GrabChan i -> IO ()) -> IO ()
withWildBind binding action = withWildBind' (WBE.wildBind binding) action

emitEvent :: TChan (WBF.FrontEvent s i) -> WBF.FrontEvent s i -> IO ()
emitEvent chan event = atomically $ writeTChan chan event

shouldProduce :: (Show a, Eq a) => TChan a -> a -> IO ()
shouldProduce chan expectation = (atomically $ readTChan chan) `shouldReturn` expectation

readAll :: TChan a -> IO [a]
readAll chan = atomically $ readAll' [] where
  readAll' acc = do
    mret <- tryReadTChan chan
    case mret of
      Nothing  -> return (reverse acc)
      Just ret -> readAll' (ret : acc)

shouldNowMatch :: (Show a, Eq a) => TChan a -> [a] -> IO ()
shouldNowMatch chan expectation = readAll chan >>= (`shouldMatchList` expectation)

changeAndInput :: s -> i -> [WBF.FrontEvent s i]
changeAndInput s i = [WBF.FEChange s, WBF.FEInput i]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  wildBindSpec
  optionSpec

wildBindSpec :: Spec
wildBindSpec = do
  describe "wildBind" $ do
    it "should enable input grabs" $ do
      ochan <- newTChanIO
      let b = WBB.binding [outChanOn ochan SIa 'A',
                           outChanOn ochan SIb 'B']
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange $ SS ""
        emitEvent echan $ WBF.FEInput SIa
        ochan `shouldProduce` 'A'
        ghist <- readAll gchan
        ghist `shouldMatchList` [GSet SIa, GSet SIb]
    it "should enable/disable grabs when the front-end state changes" $ do
      ochan <- newTChanIO
      let b = (WBB.whenFront (\(SS s) -> s == "A") $ WBB.binding [outChanOn ochan SIa 'A'])
              <>
              (WBB.whenFront (\(SS s) -> s == "B") $ WBB.binding [outChanOn ochan SIb 'B'])
              <>
              (WBB.whenFront (\(SS s) -> s == "C") $ WBB.binding [outChanOn ochan SIc 'C'])
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        mapM_ (emitEvent echan) $ changeAndInput (SS "A") SIa
        ochan `shouldProduce` 'A'
        gchan `shouldNowMatch` [GSet SIa]
        mapM_ (emitEvent echan) $ changeAndInput (SS "B") SIb
        ochan `shouldProduce` 'B'
        gchan `shouldNowMatch` [GUnset SIa, GSet SIb]
        mapM_ (emitEvent echan) $ changeAndInput (SS "C") SIc
        ochan `shouldProduce` 'C'
        gchan `shouldNowMatch` [GUnset SIb, GSet SIc]
        emitEvent echan $ WBF.FEChange (SS "")
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIc]
    it "should enable/disable grabs when the back-end state changes" $ do
      ochan <- newTChanIO
      let b' = WBB.ifBack (== (SB 0)) (WBB.binding' [outChanOnS ochan SIa 'A' (SB 1)])
               $ WBB.whenBack (== (SB 1)) (WBB.binding' [outChanOnS ochan SIb 'B' (SB 0)])
          b = WBB.startFrom (SB 0) b'
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "")
        threadDelay 10000
        gchan `shouldNowMatch` [GSet SIa]
        emitEvent echan $ WBF.FEInput SIa
        ochan `shouldProduce` 'A'
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIa, GSet SIb]
        emitEvent echan $ WBF.FEInput SIb
        ochan `shouldProduce` 'B'
        threadDelay 10000
        gchan `shouldNowMatch` [GUnset SIb, GSet SIa]
    it "should crush exceptions from bound actions" $ do
      ochan <- newTChanIO
      let b = WBB.binds $ do
            WBB.on SIa `WBB.run` (fail "ERROR!!")
            WBB.on SIb `WBB.run` (atomically $ writeTChan ochan 'b')
      withWildBind b $ \(EventChan echan) _ -> do
        emitEvent echan $ WBF.FEChange (SS "")
        emitEvent echan $ WBF.FEInput SIa
        emitEvent echan $ WBF.FEInput SIb
        ochan `shouldProduce` 'b'
    it "should keep the current back-state when exception is thrown from bound actions" $ do
      ochan <- newTChanIO
      let killing_b = WBB.binds' $ WBB.on SIa `WBB.run` do
            State.put (SB 0)
            liftIO $ fail "ERROR!"
          b = WBB.startFrom (SB 0) $ (killing_b <>)
              $ WBB.ifBack (== SB 0)
              ( WBB.binds' $ WBB.on SIb `WBB.run` do
                   liftIO $ atomically $ writeTChan ochan 'b'
                   State.put (SB 1)
              )
              ( WBB.binds' $ WBB.on SIc `WBB.run` do
                   liftIO $ atomically $ writeTChan ochan 'c'
                   State.put (SB 0)
              )
      withWildBind b $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "")
        emitEvent echan $ WBF.FEInput SIa
        emitEvent echan $ WBF.FEInput SIb
        ochan `shouldProduce` 'b'
        gchan `shouldNowMatch` [GSet SIa, GSet SIb,  GUnset SIb, GSet SIc]
        emitEvent echan $ WBF.FEInput SIa
        emitEvent echan $ WBF.FEInput SIc
        ochan `shouldProduce` 'c'
        gchan `shouldNowMatch` [GUnset SIc, GSet SIb]


shouldNextMatch :: (Show a, Eq a) => TChan [a] -> [a] -> IO ()
shouldNextMatch tc expected = do
  got <- atomically $ readTChan tc
  got `shouldMatchList` expected

optionSpec :: Spec
optionSpec = do
  describe "optBindingHook" $ do
    it "hooks change of binding because front-end state changes" $ do
      hook_chan <- newTChanIO
      out_chan <- newTChanIO
      let opt = WBE.defOption { WBE.optBindingHook = _write hook_chan }
          b = WBB.whenFront (== SS "hoge")
              $ WBB.binding [ (SIa, WBB.Action "a button" (out_chan `_write` 'a')),
                              (SIb, WBB.Action "b button" (out_chan `_write` 'b'))
                            ]
      withWildBind' (WBE.wildBind' opt b) $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "hoge")
        hook_chan `shouldNextMatch` [(SIa, "a button"), (SIb, "b button")]
        emitEvent echan $ WBF.FEInput SIa
        hook_chan `shouldNextMatch` [(SIa, "a button"), (SIb, "b button")]
        out_chan `shouldProduce` 'a'
        gchan `shouldNowMatch` [GSet SIa, GSet SIb]
        emitEvent echan $ WBF.FEChange (SS "")
        hook_chan `shouldNextMatch` []
        gchan `shouldNowMatch` [GUnset SIa, GUnset SIb]
    it "hooks change of binding because back-end state changes" $ do
      hook_chan <- newTChanIO
      out_chan <- newTChanIO
      let opt = WBE.defOption { WBE.optBindingHook = _write hook_chan  }
          b = WBB.startFrom (SB 0)
              $ WBB.ifBack (== (SB 0))
              (WBB.binding' [(SIa, WBB.Action "a button" (out_chan `_write` 'a' >> State.put (SB 1)))])
              $ WBB.whenBack (== (SB 1)) ( WBB.binding' [(SIa, WBB.Action "A BUTTON" (out_chan `_write` 'A' >> State.put (SB 0))),
                                                         (SIc, WBB.Action "c button" (out_chan `_write` 'c'))]
                                         )
      withWildBind' (WBE.wildBind' opt b) $ \(EventChan echan) (GrabChan gchan) -> do
        emitEvent echan $ WBF.FEChange (SS "")
        hook_chan `shouldNextMatch` [(SIa, "a button")]
        gchan `shouldNowMatch` [GSet SIa]
        emitEvent echan $ WBF.FEInput SIa
        out_chan `shouldProduce` 'a'
        hook_chan `shouldNextMatch` [(SIa, "A BUTTON"), (SIc, "c button")]
        gchan `shouldNowMatch` [GSet SIc]
        emitEvent echan $ WBF.FEInput SIc
        out_chan `shouldProduce` 'c'
        hook_chan `shouldNextMatch` [(SIa, "A BUTTON"), (SIc, "c button")]
        gchan `shouldNowMatch` []
        emitEvent echan $ WBF.FEInput SIa
        out_chan `shouldProduce` 'A'
        hook_chan `shouldNextMatch` [(SIa, "a button")]
        gchan `shouldNowMatch` [GUnset SIc]
  describe "optCatch" $ do
    it "receives front-state, input and exception" $ do
      hook_chan <- newTChanIO
      let catcher fs input err = atomically $ writeTChan hook_chan (fs, input, err)
          opt = WBE.defOption { WBE.optCatch = catcher }
          b = WBB.binds $ WBB.on SIa `WBB.run` (throw $ userError "BOOM!")
      withWildBind' (WBE.wildBind' opt b) $ \(EventChan echan) _ -> do
        emitEvent echan $ WBF.FEChange (SS "front state")
        emitEvent echan $ WBF.FEInput SIa
        (got_state, got_input, got_exception) <- atomically $ readTChan hook_chan
        got_state `shouldBe` SS "front state"
        got_input `shouldBe` SIa
        fromException got_exception `shouldBe` Just (userError "BOOM!")

