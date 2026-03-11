-------------------------------------------------------------------------------
-- Layer 2 (mockable IO), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Terminal.Game.Layer.Object.Test where

-- Test (pure) MonadGame* typeclass implementation for testing purposes.

import Terminal.Game.Layer.Object.Interface
import Terminal.Game.Layer.Object.Primitive

import qualified Control.Monad.Except as E
import qualified Control.Monad.RWS as S
import qualified Data.Bifunctor as B


-----------
-- TYPES --
-----------

data TestEvent = TCleanUpError
               | TException ATGException
               | TQuitGame
               | TSetupDisplay
               | TShutdownDisplay
               | TStartGame
               | TStartEvents
               | TStopEvents
        deriving (Eq, Show)

-- e: ()
-- r: ()
-- w: [TestEvents]
-- s: [GTest]
newtype Test a = Test (E.ExceptT () (S.RWS () [TestEvent] GRec) a)
               deriving (Functor, Applicative, Monad,
                         E.MonadError (), S.MonadState GRec,
                         S.MonadWriter [TestEvent])

runTest :: Test a -> GRec -> (Maybe a, [TestEvent])
runTest (Test em) es = let m = E.runExceptT em
                           t = S.evalRWS m () es in
                       B.first (either (const Nothing) Just) t

-------------------------------------------------------------------------------
-- Class

mockHandle :: InputHandle
mockHandle = InputHandle (error "mock handle keyMvar")
                         (error "mock handle threads")

instance MonadInput Test where
    startEvents _ = S.tell [TStartEvents] >>
                    return mockHandle
    pollEvents _ = S.state getPolled
    stopEvents _ = S.tell [TStopEvents]
    areEventsOver = S.gets isOver

instance MonadTimer Test where
    getTime = return 1
    sleepABit _ = return ()

instance MonadException Test where
    cleanUpErr a _ = S.tell [TCleanUpError] >> a
    throwExc e = S.tell [TException e] >>
                 E.throwError ()

instance MonadDisplay Test where
    setupDisplay = () <$ S.tell [TSetupDisplay]
    clearDisplay = return ()
    displaySize = Test $ S.state getDims
    blitPlane _ _ = return ()
    shutdownDisplay = () <$ S.tell [TShutdownDisplay]
