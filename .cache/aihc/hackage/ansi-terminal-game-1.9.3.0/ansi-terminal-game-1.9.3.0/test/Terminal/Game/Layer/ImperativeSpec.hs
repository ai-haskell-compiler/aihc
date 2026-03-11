module Terminal.Game.Layer.ImperativeSpec where

import Terminal.Game.Layer.Imperative
import Terminal.Game.Layer.Object
import Terminal.Game.Random
import Alone
import Balls

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Control.Exception as E
import qualified Test.QuickCheck as Q
import qualified GHC.Exts as X

-- Test for state.
stateTest :: Show r => Game s r -> GRec -> s
stateTest g r = either em id (testGame g r)
    where
          em wr = error $ "stateTest: " ++ show wr

spec :: Spec
spec = do

  describe "runGame" $ do
    let nd = error "<not-defined>"
        s :: (Integer, Bool, Integer)
        s = (0, False, 0)
        lf (t, True, i) Tick         = Right (t+1, True, i+1)
        lf (t, b,    i) Tick         = Right (t+1, b,    i  )
        lf (t, _,    i) (KeyPress _) = Right (t,   True, i  )
        es = [Tick, KeyPress 'c', KeyPress 'c', Tick, Tick]
        g :: Game (Integer, Bool, Integer) ()
        g = Game nd s (const lf) nd
    it "does not confuse input and logic" $
      stateTest g (createGRec (80, 24) es) `shouldBe` (3, True, 2)

  describe "testGame" $ do
    it "tests a game" $ do
        r <- readRecord "test/records/alone-record-test.gr"
        stateTest aloneInARoom r `shouldBe` MyState (20, 66) Stop
    it "tests a game exiting correctly" $ do
        r <- readRecord "test/records/alone-record-left.gr"
        testGame aloneInARoom r `shouldBe` Left ()
    it "picks up screen resize events" $ do
        r <- readRecord "test/records/balls-dims.gr"
        let g = fireworks (mkStdGen 1)
            t = stateTest g r
        length (balls t) `shouldBe` 1
    it "picks FPS too" $ do
        r <- readRecord "test/records/balls-slow.gr"
        let g = fireworks (mkStdGen 1)
            t = stateTest g r
        bslow t `shouldBe` True
    it "does not hang on empty/unclosed input" $
        let w = createGRec (80, 24) [Tick] in
        stateTest aloneInARoom w `shouldBe` MyState (10, 10) Stop
    modifyMaxSize (const 1000) $
      it "does not crash/hang on random input" $ Q.property $
        let genEvs = Q.listOf1 Q.arbitrary
        in Q.forAll genEvs $
             \es -> let w = createGRec (80, 24) es
                        a = testGame aloneInARoom w
                    in a == a
    it "fails with an informative message" $ do
        r <- readRecord "test/records/alone-record-test.gr"
        let r' = r { aTermSize = X.fromList (replicate 1000 Nothing) }
            t = testGame aloneInARoom r'
            e = "testGame, exception called: [TSetupDisplay,TStartEvents,\
                \TException CannotGetDisplaySize]"
        E.evaluate t `shouldThrow` errorCall e

