{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module WildBind.BindingSpec
    ( main
    , spec
    ) where

import           Control.Applicative        (pure, (<$>), (<*>))
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State  as State
import           Data.IORef                 (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Monoid                (mconcat, mempty, (<>))
import qualified Lens.Micro                 as Lens
import           Test.Hspec
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, listOf, property, sample')

import qualified WildBind.Binding           as WB
import           WildBind.ForTest           (SampleBackState (..), SampleInput (..),
                                             SampleState (..), boundDescs, boundDescs',
                                             checkBoundDescs, evalStateEmpty, execAll, inputAll,
                                             withRefChecker)

main :: IO ()
main = hspec spec

data BiggerSampleBackState
  = BSB
      { _lSB :: SampleBackState
      , _rSB :: SampleBackState
      }
  deriving (Eq, Ord, Show)

lSB :: Lens.Lens' BiggerSampleBackState SampleBackState
lSB = Lens.lens _lSB (\bsb sb -> bsb { _lSB = sb })

rSB :: Lens.Lens' BiggerSampleBackState SampleBackState
rSB = Lens.lens _rSB (\bsb sb -> bsb { _rSB = sb })

-- 'view' is since microlens-0.3.5.0
view :: Lens.Lens' s a -> s -> a
view l s = s Lens.^. l

newStrRef :: MonadIO m => m (IORef String)
newStrRef = liftIO $ newIORef []

withStrRef :: MonadIO m => (IORef String -> (String -> m ()) -> m ()) -> m ()
withStrRef = withRefChecker []

outOn :: MonadIO m => IORef [a] -> i -> a -> (i, WB.Action m ())
outOn out_ref input out_elem = (input, WB.Action "" $ liftIO $ modifyIORef out_ref (++ [out_elem]))

outOnS :: MonadIO m => IORef [a] -> i -> a -> (s -> s) -> (i, WB.Action (State.StateT s m) ())
outOnS out_ref input out_elem modifier = (,) input $ WB.Action "" $ do
  State.modify modifier
  liftIO $ modifyIORef out_ref (++ [out_elem])

genStatelessBinding :: Arbitrary a => IORef [a] -> Gen (WB.Binding s SampleInput)
genStatelessBinding out_list =
  WB.binding <$> (listOf $ (,) <$> arbitrary <*> (WB.Action "" <$> outputRandomElem))
  where
    outputRandomElem = do
      out_elem <- arbitrary
      return $ modifyIORef out_list (out_elem :)

generate :: Gen a -> IO a
generate = fmap head . sample'

execAll' :: Ord i => [i] -> State.StateT (WB.Binding SampleState i) IO ()
execAll' = execAll (SS "")

mempty_stateless :: WB.Binding SampleState SampleInput
mempty_stateless = mempty

checkMappend :: (WB.Binding SampleState SampleInput -> WB.Binding SampleState SampleInput) -> IO ()
checkMappend append_op = do
  out_ref <- newStrRef
  rand_binding <- generate $ genStatelessBinding out_ref
  let execute b = void $ inputAll b (SS "") =<< generate (listOf arbitrary)
  execute rand_binding
  out_orig <- readIORef out_ref
  writeIORef out_ref []
  execute (append_op rand_binding)
  readIORef out_ref `shouldReturn` out_orig

actRun :: Maybe (WB.Action IO a) -> IO ()
actRun = void . WB.actDo . fromJust

checkInputsS :: (Show i, Eq i) => s -> [i] -> State.StateT (WB.Binding s i) IO ()
checkInputsS state exp_in = State.get >>= \b -> lift $ WB.boundInputs b state `shouldMatchList` exp_in

checkInputsS' :: (Show i, Eq i) => [i] -> State.StateT (WB.Binding SampleState i) IO ()
checkInputsS' = checkInputsS (SS "")

spec :: Spec
spec = do
  spec_stateless
  spec_conversions
  spec_convBack
  spec_stateful
  spec_extend
  spec_conditionBoth
  spec_monadic
  spec_reader
  spec_revise

spec_stateless :: Spec
spec_stateless = do
  describe "Binding (Monoid instances)" $ do
    it "mempty returns empty binding" $ property
      ( isNothing <$> (WB.boundAction mempty_stateless <$> arbitrary <*> arbitrary) )
    it "mempty `mappend` random == mempty" $ do
      checkMappend (mempty <>)
    it "random `mappend` mempty == mempty" $ do
      checkMappend (<> mempty)
  describe "binding" $ do
    it "returns a stateless Binding" $ withStrRef $ \out checkOut -> do
      let b = WB.binding [outOn out SIa 'A', outOn out SIb 'B']
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      WB.boundAction b (SS "") SIc `shouldSatisfy` isNothing
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "A"
      actRun $ WB.boundAction b (SS "") SIb
      checkOut "AB"
    it "prefers the latter action if multiple actions are bound to the same input" $ withStrRef $ \out checkOut -> do
      let b = WB.binding [ outOn out SIa '1',
                           outOn out SIa '2',
                           outOn out SIa '3'
                         ]
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "3"
  describe "whenFront" $ do
    it "adds a condition on the front-end state" $ withStrRef $ \out checkOut -> do
      let b = WB.whenFront (\(SS s) -> s == "hoge") $ WB.binding [outOn out SIa 'A']
      WB.boundInputs b (SS "") `shouldMatchList` []
      WB.boundAction b (SS "") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "foobar") `shouldMatchList` []
      WB.boundAction b (SS "foobar") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      actRun $ WB.boundAction b (SS "hoge") SIa
      checkOut "A"
    it "is AND condition" $ withStrRef $ \out checkOut -> do
      let raw_b = WB.binding [outOn out SIa 'A']
          b = WB.whenFront ((<= 5) . length . unSS) $ WB.whenFront ((3 <=) . length . unSS) $ raw_b
      WB.boundInputs b (SS "ho") `shouldMatchList` []
      WB.boundAction b (SS "ho") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hogehoge") `shouldMatchList` []
      WB.boundAction b (SS "hogehoge") SIa `shouldSatisfy` isNothing
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      actRun $ WB.boundAction b (SS "hoge") SIa
      checkOut "A"
    it "should be effective for derived Bindings" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let raw_b = WB.binding [outOn out SIa 'A']
      State.put $ WB.whenFront (\(SS s) -> s == "foobar") $ raw_b
      checkInputsS (SS "hoge") []
      checkInputsS (SS "foobar") [SIa]
      execAll (SS "foobar") [SIa]
      checkOut "A"
      checkInputsS (SS "hoge") []
      checkInputsS (SS "foobar") [SIa]
  describe "ifFront" $ do
    it "chooses from independent Bindings" $ withStrRef $ \out checkOut -> do
      let b = WB.ifFront (\(SS s) -> length s <= 5)
              (WB.binding [outOn out SIa 'A']) (WB.binding [outOn out SIb 'B'])
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      WB.boundInputs b (SS "foobar") `shouldMatchList` [SIb]
      actRun $ WB.boundAction b (SS "foobar") SIb
      checkOut "B"
    it "adds AND conditions when nested" $ withStrRef $ \out checkOut -> do
      let b1 = WB.ifFront (\(SS s) -> length s <= 5)
               (WB.binding [outOn out SIa 'A']) (WB.binding [outOn out SIb 'B'])
          b = WB.ifFront (\(SS s) -> length s >= 3) b1 $ WB.binding [outOn out SIc 'C']
      WB.boundInputs b (SS "") `shouldMatchList` [SIc]
      WB.boundInputs b (SS "foo") `shouldMatchList` [SIa]
      WB.boundInputs b (SS "hoge") `shouldMatchList` [SIa]
      WB.boundInputs b (SS "foobar") `shouldMatchList` [SIb]
      actRun $ WB.boundAction b (SS "ho") SIc
      checkOut "C"
  describe "Binding (mappend)" $ do
    it "combines two stateless Bindings" $ withStrRef $ \out checkOut -> do
      let b1 = WB.binding [outOn out SIa 'A']
          b2 = WB.binding [outOn out SIb 'B']
          b = b1 <> b2
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      void $ inputAll b (SS "") [SIa, SIb]
      checkOut "AB"
    it "front-end conditions are preserved" $ withStrRef $ \out _ -> do
      let b1 = WB.whenFront ((3 <=) . length . unSS) $ WB.binding [outOn out SIa 'A']
          b2 = WB.whenFront ((<= 5) . length . unSS) $ WB.binding [outOn out SIb 'B']
          b = b1 <> b2
      WB.boundInputs b (SS "aa") `shouldMatchList` [SIb]
      WB.boundInputs b (SS "aabb") `shouldMatchList` [SIa, SIb]
      WB.boundInputs b (SS "aabbcc") `shouldMatchList` [SIa]
    it "prefers the latter Binding" $ withStrRef $ \out checkOut -> do
      let b1 = WB.binding [outOn out SIa '1', outOn out SIb 'B']
          b2 = WB.binding [outOn out SIa '2']
          b = b1 <> b2
      WB.boundInputs b (SS "") `shouldMatchList` [SIa, SIb]
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "2"
    it "preserves implicit back-end states" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b1 = WB.startFrom (SB 0)
               $ WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa '0' (\_ -> SB 1)])
               $ WB.ifBack (== (SB 1)) (WB.binding' [outOnS out SIa '1' (\_ -> SB 0)])
               $ mempty
          b2 = WB.startFrom (SB 0)
               $ WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIb '2' (\_ -> SB 1)])
               $ WB.ifBack (== (SB 1)) (WB.binding' [outOnS out SIb '3' (\_ -> SB 0)])
               $ mempty
      State.put (b1 <> b2)
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIa]
      checkOut "0"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIb]
      checkOut "02"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIa]
      checkOut "021"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIb]
      checkOut "0213"
      checkInputsS (SS "") [SIa, SIb]
      execAll (SS "") [SIa]
      checkOut "02130"

spec_conversions :: Spec
spec_conversions = do
  describe "convFront" $ do
    it "converts front-end state" $ withStrRef $ \out checkOut -> do
      let orig_b = WB.whenFront (("hoge" ==) . unSS) $ WB.binding [outOn out SIa 'A']
          b = WB.convFront SS orig_b
      WB.boundInputs b "" `shouldMatchList` []
      WB.boundInputs b "hoge" `shouldMatchList` [SIa]
      actRun $ WB.boundAction b "hoge" SIa
      checkOut "A"
  describe "convInput" $ do
    it "converts input symbols" $ withStrRef $ \out checkOut -> do
      let orig_b = WB.binding [outOn out SIa 'A']
          b = WB.convInput show orig_b
      WB.boundInputs b (SS "") `shouldMatchList` ["SIa"]
      actRun $ WB.boundAction b (SS "") "SIa"
      checkOut "A"
  describe "advice" $ do
    it "converts all actions in Binder" $ withStrRef $ \out checkOut -> do
      let convert_action a = a { WB.actDescription = WB.actDescription a <> "!!",
                                 WB.actDo = WB.actDo a >> (modifyIORef out (++ "!"))
                               }
          b = WB.binds $ WB.advice convert_action $ do
            WB.on SIa `WB.as` "action a" `WB.run` modifyIORef out (++ "A")
            WB.on SIb `WB.as` "action b" `WB.run` modifyIORef out (++ "B")
      (WB.actDescription <$> WB.boundAction b () SIa) `shouldBe` Just "action a!!"
      (WB.actDescription <$> WB.boundAction b () SIb) `shouldBe` Just "action b!!"
      void $ inputAll b () [SIa]
      checkOut "A!"
      void $ inputAll b () [SIb]
      checkOut "A!B!"
    it "preserves the order of binding." $ withStrRef $ \out checkOut -> do
      let b = WB.binds $ WB.advice (WB.before $ modifyIORef out (++ "A")) $ do
            WB.on SIa `WB.run` modifyIORef out (++ "1")
            WB.on SIa `WB.run` modifyIORef out (++ "2")
            WB.on SIa `WB.run` modifyIORef out (++ "3")
      void $ inputAll b () [SIa]
      checkOut "A3"
    it "can nest" $ withStrRef $ \out checkOut -> do
      let b = WB.binds $ do
            WB.on SIa `WB.run` modifyIORef out (++ "1")
            WB.advice (WB.before $ modifyIORef out (++ "*")) $ do
              WB.on SIb `WB.run` modifyIORef out (++ "3")
              WB.advice (WB.after $ modifyIORef out (++ "@")) $ do
                WB.on SIa `WB.run` modifyIORef out (++ "4")
                WB.on SIc `WB.run` modifyIORef out (++ "5")
              WB.advice (WB.after $ modifyIORef out (++ "#")) $ do
                WB.on SIb `WB.run` modifyIORef out (++ "6")
                WB.on SIc `WB.run` modifyIORef out (++ "7")
              WB.on SIa `WB.run` modifyIORef out (++ "8")
      void $ inputAll b () [SIa]
      checkOut "*8"
      void $ inputAll b () [SIb]
      checkOut "*8*6#"
      void $ inputAll b () [SIc]
      checkOut "*8*6#*7#"
  describe "before" $ do
    it "prepends a monadic action" $ withStrRef $ \out checkOut -> do
      let act = WB.Action { WB.actDescription = "desc",
                            WB.actDo = modifyIORef out (++ "ORIG")
                          }
          got = WB.before (modifyIORef out (++ "before")) act
      WB.actDescription got `shouldBe` "desc"
      WB.actDo got
      checkOut "beforeORIG"
  describe "after" $ do
    it "appends a monadic action" $ withStrRef $ \out checkOut -> do
      let act = WB.Action { WB.actDescription = "desc",
                            WB.actDo = modifyIORef out (++ "ORIG")
                          }
          got = WB.after (modifyIORef out (++ "after")) act
      WB.actDescription got `shouldBe` "desc"
      WB.actDo got
      checkOut "ORIGafter"

spec_convBack :: Spec
spec_convBack = do
  describe "convBack" $ do
    it "can convert the back-end state by isomorphism" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let act = do
            out_elem <- head <$> show <$> unSB <$> State.get
            liftIO $ modifyIORef out (++ [out_elem])
            State.modify succ
          orig_b = WB.binding' [(SIa, WB.Action "" act)]
          b = WB.convBack (\s _-> unSB s) SB orig_b
      State.put $ WB.startFrom 0 b
      checkInputsS' [SIa]
      execAll' [SIa]
      checkOut "0"
      execAll' [SIa]
      checkOut "01"
      execAll' [SIa]
      checkOut "012"
    it "can convert the back-end state by a lens" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let bl = WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa '0' (\_ -> SB 1)])
               $ WB.whenBack (== (SB 1)) (WB.binding' [outOnS out SIa '1' (\_ -> SB 0)])
          br = WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIb '2' (\_ -> SB 1)])
               $ WB.whenBack(== (SB 1)) (WB.binding' [outOnS out SIb '3' (\_ -> SB 0)])
          bg = WB.whenBack (== (BSB (SB 0) (SB 0))) $ WB.binding' [outOnS out SIc '4' (\_ -> BSB (SB 1) (SB 1))]
          convBackByLens :: Lens.Lens' s a -> WB.Binding' a f i -> WB.Binding' s f i
          convBackByLens l = WB.convBack (Lens.set l) (view l)
          b = (convBackByLens lSB bl) <> (convBackByLens rSB br) <> bg
      State.put $ WB.startFrom (BSB (SB 0) (SB 0)) b
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIa]
      checkOut "0"
      checkInputsS' [SIa, SIb]
      execAll' [SIb]
      checkOut "02"
      checkInputsS' [SIa, SIb]
      execAll' [SIb]
      checkOut "023"
      checkInputsS' [SIa, SIb]
      execAll' [SIa]
      checkOut "0231"
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIc]
      checkOut "02314"
      checkInputsS' [SIa, SIb]
      execAll' [SIa, SIb]
      checkOut "0231413"


spec_stateful :: Spec
spec_stateful = do
  describe "binding'" $ do
    it "returns a stateful Binding" $ withStrRef $ \out checkOut -> do
      let act = do
            out_elem <- head <$> show <$> unSB <$> State.get
            liftIO $ modifyIORef out (++ [out_elem])
            State.modify succ
          b = WB.binding' [(SIa, WB.Action "" act)]
      WB.boundInputs' b (SB 0)  (SS "") `shouldBe` [SIa]
      WB.boundInputs' b (SB 10) (SS "hoge") `shouldBe` [SIa]
      void $ inputAll (WB.startFrom (SB 0) b) (SS "") $ replicate 12 SIa
      checkOut "012345678911"
    it "prefers the latter action if multiple actions are bound to the same input" $ withStrRef $ \out checkOut -> do
      let b = WB.startFrom (SB 0) $ WB.binding' [ outOn out SIa '1',
                                                  outOn out SIa '2',
                                                  outOn out SIa '3'
                                                ]
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "3"
    it "can create a stateful Binding with different bound inputs for different back-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      State.put $ WB.startFrom (SB 0)
        $ WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa 'A' (\_ -> SB 1)])
        $ WB.ifBack (== (SB 1)) (WB.binding' [outOnS out SIb 'B' (\_ -> SB 2)])
        $ WB.ifBack (== (SB 2)) (WB.binding' [outOnS out SIc 'C' (\_ -> SB 0)])
        $ mempty
      checkOut ""
      checkInputsS (SS "") [SIa]
      execAll (SS "") [SIa]
      checkOut "A"
      checkInputsS (SS "") [SIb]
      execAll (SS "") [SIb]
      checkOut "AB"
      checkInputsS (SS "") [SIc]
      execAll (SS "") [SIc]
      checkOut "ABC"
      checkInputsS (SS "") [SIa]
  describe "Binding (mappend, stateful)" $ do
    it "shares the explicit back-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b1 = WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa 'A' (\_ -> SB 1)])
               $ WB.ifBack (== (SB 1)) ( WB.binding' [outOnS out SIb 'B' (\_ -> SB 2),
                                                    outOnS out SIc 'b' (\_ -> SB 2)]
                                       )
               $ WB.ifBack (== (SB 2)) (WB.binding' [outOnS out SIc 'C' (\_ -> SB 0)])
               $ mempty
          b2 = WB.whenBack (== (SB 1)) $ WB.binding' [outOnS out SIb 'D' (\_ -> SB 0)]
          b = b1 <> b2
      State.put $ WB.startFrom (SB 0) b
      checkInputsS (SS "") [SIa]
      execAll (SS "") [SIa]
      checkOut "A"
      checkInputsS (SS "") [SIb, SIc]
      execAll (SS "") [SIb]
      checkOut "AD"
      checkInputsS (SS "") [SIa]
      execAll (SS "") [SIa, SIc]
      checkOut "ADAb"
      checkInputsS (SS "") [SIc]
      execAll (SS "") [SIc]
      checkOut "ADAbC"
      checkInputsS (SS "") [SIa]

  describe "ifBack" $ do
    it "chooses from unconditional bindings" $ withStrRef $ \out checkOut -> do
      let b = WB.ifBack (\(SB sb) -> sb < 5)
              (WB.binding [outOn out SIa 'A']) (WB.binding [outOn out SIb 'B'])
          ba = WB.startFrom (SB 4) b
          bb = WB.startFrom (SB 5) b
      WB.boundInputs ba (SS "") `shouldMatchList` [SIa]
      actRun $ WB.boundAction ba (SS "") SIa
      checkOut "A"
      WB.boundInputs bb (SS "") `shouldMatchList` [SIb]
      actRun $ WB.boundAction bb (SS "") SIb
      checkOut "AB"
    it "combines an extended stateless binding with a stateful binding" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b_stateless = WB.binding [outOn out SIa 'A']
          b = WB.ifBack (\(SB sb) -> sb < 5)
              (b_stateless <> WB.binding' [outOnS out SIb 'B' $ const (SB 10)])
              $ WB.binding' [outOnS out SIc 'C' $ const (SB 3)]
      State.put $ WB.startFrom (SB 0) b
      checkInputsS' [SIa, SIb]
      execAll' [SIa]
      checkOut "A"
      checkInputsS' [SIa, SIb]
      execAll' [SIb]
      checkOut "AB"
      checkInputsS' [SIc]
      execAll' [SIc]
      checkOut "ABC"
      checkInputsS' [SIa, SIb]
    it "combines implicit stateful binding with a binding with newly introduced states" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b1 = WB.startFrom (SB 0)
               $ WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa 'A' $ const (SB 1)])
               $ WB.binding' [outOnS out SIb 'B' $ const (SB 0)]
          b = WB.startFrom (SB 0)
              $ WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa 'a' $ const (SB 1)])
              $ WB.extend b1 <> WB.binding' [outOnS out SIc 'c' $ const (SB 0)]
      State.put b
      checkInputsS' [SIa]
      execAll' [SIa]
      checkOut "a"
      checkInputsS' [SIa, SIc]
      execAll' [SIa]
      checkOut "aA"
      checkInputsS' [SIb, SIc]
      execAll' [SIb]
      checkOut "aAB"
      checkInputsS' [SIa, SIc]
      execAll' [SIc]
      checkOut "aABc"
      checkInputsS' [SIa]

  describe "whenBack" $ do
    it "adds a condition to the back-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let raw_b = WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa '0' (\_ -> SB 1)])
                  $ WB.ifBack (== (SB 1)) (WB.binding' [outOnS out SIb '1' (\_ -> SB 0)])
                  $ mempty
          b = WB.whenBack (== SB 0) $ raw_b
      State.put $ WB.startFrom (SB 0) b
      checkInputsS' [SIa]
      execAll' [SIa]
      checkOut "0"
      checkInputsS' []

spec_extend :: Spec
spec_extend = do
  describe "extend" $ do
    it "extends a stateless Binding" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let bl :: WB.Binding SampleState SampleInput
          bl = WB.binding [
            outOn out SIa 'a',
            outOn out SIb 'b',
            outOn out SIc 'c']
          bs = WB.ifBack (== (SB 0)) (WB.binding' [outOnS out SIa 'A' (\_ -> SB 1)])
               $ WB.ifBack (== (SB 1)) (WB.binding' [outOnS out SIb 'B' (\_ -> SB 2)])
               $ WB.ifBack (== (SB 2)) (WB.binding' [outOnS out SIc 'C' (\_ -> SB 0)])
               $ mempty
      State.put $ WB.startFrom (SB 0) $ (WB.extend bl <> bs)
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIb, SIc, SIa]
      checkOut "bcA"
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIa, SIc, SIb]
      checkOut "bcAacB"
      checkInputsS' [SIa, SIb, SIc]
      execAll' [SIa, SIb, SIc]
      checkOut "bcAacBabC"

spec_conditionBoth :: Spec
spec_conditionBoth = do
  describe "ifBoth" $ do
    it "chooses bindings according to front-end and back-end states" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b = WB.ifBoth (\ _ (SS fs) -> fs == "hoge") ( WB.binding' [ outOnS out SIa 'a' (SB . succ . unSB),
                                                                      outOnS out SIb 'b' (const $ SB 0)
                                                                    ]
                                                    )
              $ WB.ifBoth (\ (SB bs) (SS fs) -> length fs < bs)
                (WB.binding' [ outOnS out SIc 'c' (SB . pred . unSB) ])
                (WB.binding' [ outOnS out SIb 'B' (SB . succ . unSB) ])
      State.put $ WB.startFrom (SB 10) $ b
      checkInputsS (SS "hoge") [SIa, SIb]
      checkInputsS (SS "") [SIc]
      checkInputsS (SS "foooooobaaaaaa") [SIb]
      execAll (SS "hoge") [SIb]
      checkOut "b"
      checkInputsS (SS "") [SIb]
      execAll (SS "") [SIb]
      checkOut "bB"
      checkInputsS (SS "hoge") [SIa, SIb]
      checkInputsS (SS "") [SIc]
      checkInputsS (SS "a") [SIb]
      execAll (SS "") [SIc]
      checkOut "bBc"
      checkInputsS (SS "hoge") [SIa, SIb]
      checkInputsS (SS "") [SIb]
      checkInputsS (SS "a") [SIb]
      execAll (SS "hoge") $ replicate 5 SIa
      checkOut "bBcaaaaa"
      checkInputsS (SS "hoge") [SIa, SIb]
      checkInputsS (SS "fooo") [SIc]
      checkInputsS (SS "foooo") [SIb]

  describe "whenBoth" $ do
    let incr' out ret = outOnS out SIa ret (\(SB num) -> SB (num + 1))
        decr' out ret = outOnS out SIb ret (\(SB num) -> SB (num - 1))
    it "adds a condition to both front-end and back-end states" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let incr = incr' out
          decr = decr' out
          raw_b = WB.ifBack (== (SB 0)) (WB.binding' [incr '+']) (WB.binding' [incr '+', decr '-'])
          b = WB.whenBoth (\(SB num) (SS str) -> length str == num) $ raw_b
      State.put $ WB.startFrom (SB 0) $ b
      checkInputsS (SS "hoge") []
      checkInputsS (SS "") [SIa]
      execAll (SS "") [SIa]
      checkOut "+"
      checkInputsS (SS "") []
      checkInputsS (SS "e") [SIa, SIb]
      execAll (SS "e") [SIa]
      checkOut "++"
      checkInputsS (SS "e") []
      checkInputsS (SS "eg") [SIa, SIb]
      execAll (SS "eg") [SIb]
      checkOut "++-"
      checkInputsS (SS "e") [SIa, SIb]
    it "creates independent conditions when combined with <>" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let incr = incr' out
          decr = decr' out
          bn = WB.ifBack (== (SB 0)) (WB.binding' [incr '+']) (WB.binding' [incr '+', decr '-'])
          bn' = WB.whenBoth (\(SB num) (SS str) -> length str == num) bn
          ba = WB.ifBack (== (SB 0)) (WB.binding' [incr 'p']) (WB.binding' [incr 'p', decr 'm'])
          ba' = WB.whenBoth (\(SB num) (SS str) -> read str == num) ba
      State.put $ WB.startFrom (SB 1) (bn' <> ba')
      checkInputsS (SS "10") []
      checkInputsS (SS "4") [SIa, SIb]
      execAll (SS "4") [SIa]
      checkOut "+"
      checkInputsS (SS "2") [SIa, SIb]
      execAll (SS "2") [SIa]
      checkOut "+p"
      checkInputsS (SS "342") [SIa, SIb]
      execAll (SS "342") [SIb]
      checkOut "+p-"
      execAll (SS "2") [SIb]
      checkOut "+p-m"
      checkInputsS (SS "1") [SIa, SIb]
      execAll (SS "1") [SIb]
      checkOut "+p-mm"

spec_monadic :: Spec
spec_monadic = describe "Monadic construction of Binding" $ do
  describe "binds" $ do
    it "constructs stateless Binding" $ withStrRef $ \out checkOut -> do
      let putOut c = modifyIORef out (++ [c])
          b = WB.binds $ do
            WB.on SIa `WB.run` putOut 'a'
            WB.on SIb `WB.run` do
              putOut 'b'
              putOut 'B'
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "a"
      actRun $ WB.boundAction b (SS "") SIb
      checkOut "abB"
    it "prefers the latter action if multiple actions are bound to the same input" $ withStrRef $ \out checkOut -> do
      let b = WB.binds $ do
            WB.on SIa `WB.run` modifyIORef out (++ "1")
            WB.on SIa `WB.run` modifyIORef out (++ "2")
            WB.on SIa `WB.run` modifyIORef out (++ "3")
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "3"
  describe "Binder" $ do
    it "can bind actions with different result types" $ withStrRef $ \out checkOut -> do
      let ret_b :: String
          ret_b = "return by b"
          b = WB.binds $ do  -- it's ok if it compiles..
            WB.on SIa `WB.run` do
              modifyIORef out (++ "a")
              return ()
            WB.on SIb `WB.run` do
              modifyIORef out (++ "b")
              return ret_b
      actRun $ WB.boundAction b (SS "") SIb
      checkOut "b"
  describe "binds'" $ do
    it "constructs stateful Binding" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      State.put $ WB.startFrom (SB 0) $ WB.binds' $ do
        WB.on SIa `WB.run` (State.modify $ \(SB v) -> SB (v + 1))
        WB.on SIb `WB.run` (State.modify $ \(SB v) -> SB (v - 1))
        WB.on SIc `WB.run` do
          (SB cur) <- State.get
          liftIO $ modifyIORef out (++ show cur)
      execAll' [SIa, SIa, SIa]
      checkOut ""
      execAll' [SIc]
      checkOut "3"
      execAll' [SIb, SIb]
      checkOut "3"
      execAll' [SIc]
      checkOut "31"
    it "prefers the latter action if multiple actions are bound to the same input" $ withStrRef $ \out checkOut -> do
      let b = WB.startFrom (SB 0) $ WB.binds' $ do
            WB.on SIa `WB.run` (liftIO $ modifyIORef out (++ "1"))
            WB.on SIa `WB.run` (liftIO $ modifyIORef out (++ "2"))
            WB.on SIa `WB.run` (liftIO $ modifyIORef out (++ "3"))
      actRun $ WB.boundAction b (SS "") SIa
      checkOut "3"
  describe "as" $ do
    it "sets ActionDescription" $ do
      let b = WB.binds $ do
            WB.on SIa `WB.as` "action for a" `WB.run` return ()
            WB.on SIb `WB.as` "action for b" `WB.run` return ()
      (WB.actDescription <$> WB.boundAction b (SS "") SIa) `shouldBe` Just "action for a"
      (WB.actDescription <$> WB.boundAction b (SS "") SIb) `shouldBe` Just "action for b"
      (WB.actDescription <$> WB.boundAction b (SS "") SIc) `shouldBe` Nothing

spec_reader :: Spec
spec_reader = describe "binding with ReaderT action" $ do
  describe "bindsF" $ do
    it "allows actions to access front-end state" $ withStrRef $ \out checkOut -> do
      let b = WB.bindsF $ do
            WB.on SIa `WB.run` do
              fs <- Reader.ask
              liftIO $ modifyIORef out (++ unSS fs)
      actRun $ WB.boundAction b (SS "hoge") SIa
      checkOut "hoge"
      actRun $ WB.boundAction b (SS "_foobar") SIa
      checkOut "hoge_foobar"
  describe "bindsF'" $ do
    it "allows stateful actions to access front-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let ba = WB.bindsF' $ do
            WB.on SIa `WB.run` do
              (SB bs) <- State.get
              (SS fs) <- lift $ Reader.ask
              if bs >= 0
                then liftIO $ modifyIORef out (++ fs)
                else liftIO $ modifyIORef out (++ reverse fs)
          bb = WB.binds' $ do
            WB.on SIb `WB.run` State.modify (\(SB bs) -> SB (bs + 1))
          bc = WB.binds' $ do
            WB.on SIc `WB.run` State.modify (\(SB bs) -> SB (bs - 1))
          b = WB.startFrom (SB 0) (bb <> ba <> bc)
      State.put b
      execAll (SS "abc") [SIa, SIb, SIb]
      checkOut "abc"
      execAll (SS "123") [SIc, SIc, SIc, SIc, SIa]
      checkOut "abc321"
      execAll (SS "xyz") [SIa, SIb, SIb, SIa]
      checkOut "abc321zyxxyz"

spec_revise :: Spec
spec_revise = do
  describe "revise" $ do
    it "should allow unbinding" $ do
      let b = WB.binds $ do
            WB.on SIa `WB.as` "a" `WB.run` return ()
            WB.on SIb `WB.as` "b" `WB.run` return ()
          rev () _ i act = if i == SIa then Nothing else Just act
          got = WB.revise rev b
      boundDescs got (SS "")  `shouldMatchList` [(SIb, "b")]
    it "should allow revising description" $ do
      let b = WB.binds $ do
            WB.on SIa `WB.as` "a" `WB.run` return ()
            WB.on SIb `WB.as` "b" `WB.run` return ()
          rev () _ _ act = Just $ act { WB.actDescription = mconcat $ replicate 3 $ WB.actDescription act }
          got = WB.revise rev b
      boundDescs got (SS "") `shouldMatchList` [(SIa, "aaa"), (SIb, "bbb")]
    it "should revise conditionally on front-end state" $ do
      let b = WB.binds $ do
            WB.on SIa `WB.as` "a" `WB.run` return ()
            WB.on SIb `WB.as` "b" `WB.run` return ()
          rev () (SS fs) i act = if i == SIa && length fs >= 3
                                 then Nothing else Just act
          got = WB.revise rev b
      boundDescs got (SS "")  `shouldMatchList` [(SIa, "a"), (SIb, "b")]
      boundDescs got (SS "xx") `shouldMatchList` [(SIa, "a"), (SIb, "b")]
      boundDescs got (SS "xxx") `shouldMatchList` [(SIb, "b")]
      boundDescs got (SS "xxxx") `shouldMatchList` [(SIb, "b")]
    it "should revise conditionally on back-end state" $ do
      let b = WB.binds $ do
            WB.on SIa `WB.as` "a" `WB.run` return ()
            WB.on SIb `WB.as` "b" `WB.run` return ()
          rev (SB bs) _ i act = if bs >= 5 && i == SIb
                                then Nothing
                                else Just act
          got = WB.revise rev $ WB.extend b
      boundDescs' got (SB 3) (SS "") `shouldMatchList` [(SIa, "a"), (SIb, "b")]
      boundDescs' got (SB 5) (SS "") `shouldMatchList` [(SIa, "a")]
      boundDescs' got (SB 7) (SS "") `shouldMatchList` [(SIa, "a")]
    it "should allow modifying the action" $ withStrRef $ \out checkOut -> do
      let b = WB.binds' $ do
            WB.on SIa `WB.as` "a" `WB.run` do
              (SB bs) <- State.get
              liftIO $ modifyIORef out (++ show bs)
          rev (SB bs) (SS fs) _ = WB.justBefore bf . WB.after af
            where
              bf = modifyIORef out (++ replicate bs 'X')
              af = modifyIORef out (++ fs)
          got = WB.revise rev b
      actRun $ WB.boundAction' got (SB 4) (SS "FF") SIa
      checkOut "XXXX4FF"
    it "should be effective after change of back-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b = WB.startFrom (SB 0) $ WB.binds' $ do
            WB.on SIa `WB.run` do
              (SB bs) <- State.get
              liftIO $ modifyIORef out (++ show bs)
              State.put (SB $ bs + 1)
          rev _ _ _ = WB.justAfter $ modifyIORef out (++ "X")
          got = WB.revise rev b
      State.put got
      execAll (SS "") [SIa]
      checkOut "0X"
      execAll (SS "") [SIa]
      checkOut "0X1X"
      execAll (SS "") [SIa]
      checkOut "0X1X2X"
  describe "revise'" $ do
    it "should allow modifying the back-end state" $ evalStateEmpty $ withStrRef $ \out checkOut -> do
      let b = WB.binds' $ do
            WB.on SIa `WB.as` "a" `WB.run` do
              (SB bs) <- State.get
              liftIO $ modifyIORef out (++ show bs)
          rev _ (SS fs) _ = WB.justAfter af . WB.before bf
            where
              bf = State.modify (\(SB s) -> SB (s + 1))
              af = State.put $ SB $ length fs
          got = WB.startFrom (SB 0) $ WB.revise' rev b
      State.put got
      execAll (SS "abc") [SIa]
      checkOut "1"
      execAll (SS "a") [SIa]
      checkOut "14"
      execAll (SS "") [SIa]
      checkOut "142"
    it "should allow unbind conditionally" $ evalStateEmpty $ do
      let b = WB.binds' $ do
            WB.on SIa `WB.as` "a" `WB.run` State.modify (\(SB bs) -> SB $ bs + 1)
            WB.on SIb `WB.as` "b" `WB.run` return ()
          rev (SB bs) (SS fs) i orig = if i == SIb && bs >= 3 && bs <= 6 && fs /= "XXX"
                                       then Nothing
                                       else Just orig
          got = WB.startFrom (SB 0) $ WB.revise' rev b
      State.put got
      checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
      execAll (SS "") [SIa, SIa]
      checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
      execAll (SS "") [SIa]
      checkBoundDescs (SS "") [(SIa, "a")]
      checkBoundDescs (SS "XXX") [(SIa, "a"), (SIb, "b")]
      execAll (SS "") [SIa, SIa, SIa]
      checkBoundDescs (SS "") [(SIa, "a")]
      execAll (SS "") [SIa]
      checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]

