module Control.Timer.TickSpec where

import Control.Timer.Tick

import Test.Hspec

import qualified Test.QuickCheck as Q
import qualified Control.Exception as E


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "creaTimer" $ do
    it "should get off with the right resource" $
      let st = creaTimer 'a' 'b' 1 in
      fetchFrame (tick st) `shouldBe` 'b'

  describe "creaTimedRes" $ do
    it "does not allow creation of empty timed resources" $
      E.evaluate (creaTimedRes AlwaysLoop [])
        `shouldThrow` anyException
    it "creates an already elapsed timer (Reach)" $
      let t = creaTimedRes (Times 1 Reach) [(10, 'a')]
      in isExpired t `shouldBe` True
    it "does not allow negative durations" $
      let t = creaTimedRes (Times 1 Elapse) [(0, 'a')]
      in E.evaluate t `shouldThrow` anyException
    it "always creates a lapseable timer" $ Q.property $
      let genPos :: Q.Gen Integer
          genPos = Q.suchThat Q.arbitrary (>0)

          genLoop :: Q.Gen Loop
          genLoop = Q.oneof [Times <$> genPos
                                   <*> Q.oneof [pure Reach, pure Elapse]]

          genTup :: Q.Gen (Integer, Char)
          genTup = (,) <$> genPos <*> pure 'a'

          genTest = (,) <$> genLoop <*> Q.listOf1 genTup

      in Q.forAll genTest
          (\(l, as) ->
            let t = creaTimedRes l as
            in isExpired (lapse t) == True)

  describe "tick" $ do
    it "ticks a simple timer" $
      let st = creaBoolTimer 1 in
      isExpired (tick st) `shouldBe` True

  describe "ticks" $ do
    let t = creaBoolTimer 10
    it "fails on negative integer" $
      E.evaluate (ticks (-3) t) `shouldThrow` anyException
    it "performs a number of ticks" $
      isExpired (ticks 10 t) `shouldBe` True
    it "does not overtick" $
      isExpired (ticks  9 t) `shouldBe` False
    it "does not choke on extra tick" $
      isExpired (ticks 80 t) `shouldBe` True

  describe "loops" $ do
    let t  = creaTimedRes (Times 2 Elapse)  [(1,())]
    let ta = creaTimedRes AlwaysLoop [(1,())]
    it "loops appropriately" $
      isExpired (ticks 2 t) `shouldBe` True
    it "loops appropriately (forever)" $
      isExpired (ticks 20 ta) `shouldBe` False
    it "expires appropriately" $
      isExpired (ticks 1 t) `shouldBe` False
    it "does not error on cycling" $
      fetchFrame (ticks 20 ta) `shouldBe` ()

  describe "isExpired" $ do
    let st = creaBoolTimer 10
    it "checks if a timer is expired" $
      isExpired (ticks 100 st) `shouldBe` True
    it "complements isLive" $
      isExpired (ticks 100 st) `shouldBe` not (isLive (ticks 100 st))
    it "expires on last frame with Reach" $
      let st1 = creaTimedRes (Times 1 Reach) [(1, 'a'), (1, 'b')] in
      isExpired (ticks 1 st1) `shouldBe` True

  describe "fetchFrame" $ do
    let ta = creaTimedRes (Times 1 Elapse) [(1,'a'), (2, 'b'), (1, 'c')]
    it "gets the underlying resoure" $
      fetchFrame (ticks 3 ta) `shouldBe` 'c'
    it "does not choke on last tick" $
      fetchFrame (ticks 10 ta) `shouldBe` 'c'

  describe "reset" $ do
    let t = creaBoolTimer 10
    it "resets the timer" $
      reset (ticks 30 t) `shouldBe` t

  describe "lapse" $ do
    it "lapses the timer" $
      let t = creaBoolTimer 10
      in lapse t `shouldBe` ticks 10 t
    it "lapse Reach without hanging" $
      let t = creaTimedRes (Times 1 Reach) [(10, 'a')]
      in isExpired (lapse t) `shouldBe` True

  describe "creaBoolTimerLoop" $ do
    let t  = creaBoolTimerLoop 10
    it "Expires after the right amount of clicks" $
      fetchFrame (ticks 10 t) `shouldBe` True
    let t' = creaTimedRes (Times 10 Reach) [(10, False), (1, True)]
    it "behaves similarly to a multi-loop timer" $
      fetchFrame (ticks 10 t) `shouldBe` fetchFrame (ticks 10 t')

