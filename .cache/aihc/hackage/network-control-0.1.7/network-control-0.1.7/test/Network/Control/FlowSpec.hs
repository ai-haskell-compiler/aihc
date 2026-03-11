{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Network.Control.FlowSpec (spec) where

import Data.List
import Data.Text.Lazy (unpack)
import Network.Control
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Pretty.Simple

-- types

data Op = Consume | Receive
    deriving (Eq, Show, Bounded, Enum)

data OpWithResult = ConsumeWithResult (Maybe Int) | ReceiveWithResult Bool
    deriving (Eq, Show)

data Step op = Step {stepOp :: op, stepArg :: Int}
    deriving (Eq, Show)

data Trace = Trace
    { traceStart :: RxFlow
    , traceSteps :: [(Int, Step OpWithResult, RxFlow)]
    }
    deriving (Eq, Show)

-- arbitrary instances

maxWindowSize :: Int
maxWindowSize = 200 -- (more realistic: 2_000_000)

instance Arbitrary RxFlow where
    -- Prefer to generate a simple window size
    arbitrary =
        newRxFlow
            <$> oneof
                [ elements [1, 10, 50, 100]
                , chooseInt (1, maxWindowSize)
                ]

instance Arbitrary Op where
    arbitrary = elements [minBound ..]

instance Arbitrary Trace where
    arbitrary = do
        initialFlow <- arbitrary
        len <- chooseInt (0, 500)
        Trace initialFlow <$> runManySteps len 0 initialFlow
      where
        runManySteps :: Int -> Int -> RxFlow -> Gen [(Int, Step OpWithResult, RxFlow)]
        runManySteps 0 _ _ = pure []
        runManySteps len ix oldFlow = do
            (newStep, newFlow) <- runStep oldFlow <$> genStep oldFlow
            ((ix, newStep, newFlow) :) <$> runManySteps (len - 1) (ix + 1) newFlow

        genStep :: RxFlow -> Gen (Step Op)
        genStep oldFlow = oneof [mkConsume, mkReceive]
          where
            -- Negative frames are non-sensical; frames larger than the window
            -- size are theoretically possible (but will trivially be rejected
            -- as exceeding the window).
            mkReceive =
                Step Receive <$> chooseInt (0, rxfBufSize oldFlow * 2)

            -- We can only consume as much as we have received
            -- (but it is in principle not a problem to consume 0 bytes)
            mkConsume =
                Step Consume <$> chooseInt (0, rxfReceived oldFlow - rxfConsumed oldFlow)

        runStep :: RxFlow -> Step Op -> (Step OpWithResult, RxFlow)
        runStep oldFlow = \case
            Step Consume arg ->
                let (newFlow, limitDelta) = maybeOpenRxWindow arg FCTWindowUpdate oldFlow
                 in (Step (ConsumeWithResult limitDelta) arg, newFlow)
            Step Receive arg ->
                let (newFlow, isAcceptable) = checkRxLimit arg oldFlow
                 in (Step (ReceiveWithResult isAcceptable) arg, newFlow)

    shrink (Trace initialFlow steps) =
        concat
            [ -- Take a prefix (starting with the same initialFlow)
              Trace initialFlow <$> init (inits steps)
            , -- Take a suffix (starting with a later initialFlow)
              map shiftInitialFlow $ drop 1 (tails steps)
            ]
      where
        shiftInitialFlow :: [(Int, Step OpWithResult, RxFlow)] -> Trace
        shiftInitialFlow [] = Trace initialFlow []
        shiftInitialFlow ((_, _, initialFlow') : rest) = Trace initialFlow' rest

-- invariants

assertTrace :: Trace -> Property
assertTrace (Trace initialFlow steps) = assertStep initialFlow steps

assertStep :: RxFlow -> [(Int, Step OpWithResult, RxFlow)] -> Property
assertStep _ [] = property True
assertStep oldFlow ((ix, step, newFlow) : steps) =
    counterexample ("step #" <> show ix) check .&. assertStep newFlow steps
  where
    check :: Expectation
    check = case step of
        Step (ConsumeWithResult limitDelta) arg -> do
            -- There is no point duplicating precisely the same logic here as in
            -- 'maybeOpenRxWindow': that would result in circular reasoning.
            -- Instead, we leave 'maybeOpenRxWindow' some implementation
            -- freedom, and only verify that the window update makes sense:
            --
            -- (a) It can't be too large: the new window after the update should
            --     never exceed the specified buffer size.
            -- (b) It can't be too late: if we consume /all/ received data, and
            --     do not allow the peer to send any further data, then the
            --     system deadlocks.
            -- (c) It shouldn't be too small: very small window updates are
            --     wasteful.
            --
            -- Within these parameters 'maybeOpenRxWindow' can decide when to
            -- send window updates and how large they should be. We also don't
            -- set the bound on (c) too strict.
            newFlow
                `shouldBe` RxFlow
                    { rxfBufSize = rxfBufSize oldFlow
                    , rxfConsumed = rxfConsumed oldFlow + arg
                    , rxfReceived = rxfReceived oldFlow
                    , rxfLimit = case limitDelta of
                        Nothing -> rxfLimit oldFlow
                        Just upd -> rxfLimit oldFlow + upd
                    }
            -- Condition (a)
            newFlow `shouldSatisfy` \flow ->
                rxfLimit flow - rxfConsumed flow <= rxfBufSize flow
            -- Condition (b)
            newFlow `shouldSatisfy` \flow ->
                rxfLimit flow > rxfConsumed flow
            -- Condition (c)
            limitDelta `shouldSatisfy` \case
                Nothing -> True
                Just upd -> upd >= rxfBufSize newFlow `div` 8
        Step (ReceiveWithResult isAcceptable) arg -> do
            newFlow
                `shouldBe` if isAcceptable
                    then
                        RxFlow
                            { rxfBufSize = rxfBufSize newFlow
                            , rxfConsumed = rxfConsumed oldFlow
                            , rxfReceived = rxfReceived oldFlow + arg
                            , rxfLimit = rxfLimit oldFlow
                            }
                    else oldFlow

spec :: Spec
spec = do
    describe "Flow" $ do
        prop "state transition graph checks out" $ \trace ->
            counterexample (unpack $ pShowNoColor trace) (assertTrace trace)
