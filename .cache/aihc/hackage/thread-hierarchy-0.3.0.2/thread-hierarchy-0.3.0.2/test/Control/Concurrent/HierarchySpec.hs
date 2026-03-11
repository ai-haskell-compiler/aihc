module Control.Concurrent.HierarchySpec where

import           Control.Concurrent                   (killThread, threadDelay)
import           Control.Concurrent.MVar              (MVar, isEmptyMVar,
                                                       newEmptyMVar, putMVar,
                                                       readMVar, takeMVar)
import           Control.Concurrent.STM.TVar          (readTVarIO)
import           Control.Exception                    (AsyncException (ThreadKilled),
                                                       catch)
import           Control.Monad                        (unless, void)
import           Data.Foldable                        (for_)
import           Data.Map.Strict                      (toList)
import           Data.Traversable                     (for)
import           Data.Typeable                        (Typeable, typeOf)

import           Test.Hspec

import           Control.Concurrent.HierarchyInternal

instance Typeable a => Show (MVar a) where
    show mVar = show (typeOf mVar)

instance Show FinishMarker where
    show (FinishMarker mVar) = "FinishMarker" ++ show mVar

spec :: Spec
spec = do
    describe "newThreadMap" $
        it "returns a new empty ThreadMap" $ do
            (ThreadMap threadMap) <- newThreadMap
            currentChildren <- readTVarIO threadMap
            toList currentChildren `shouldBe` []

    describe "newChild" $ do
        it "forks a new thread and register it to given ThreadMap" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            oneRootChildren <- readTVarIO rtMapMVar
            (length . toList) oneRootChildren `shouldBe` 1
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            threeRootChildren <- readTVarIO rtMapMVar
            (length . toList) threeRootChildren `shouldBe` 3

        it "forks a thread which automatically cleanup registration of itself on exit" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            trigger <- newEmptyMVar
            void . newChild rootThreadMap $ \_ -> takeMVar trigger
            putMVar trigger ()
            threadDelay (10 * 1000)
            oneRootChildren <- readTVarIO rtMapMVar
            toList oneRootChildren `shouldBe` []
            trigger1 <- newEmptyMVar
            trigger2 <- newEmptyMVar
            trigger3 <- newEmptyMVar
            void . newChild rootThreadMap $ \_ -> takeMVar trigger1
            void . newChild rootThreadMap $ \_ -> takeMVar trigger2
            void . newChild rootThreadMap $ \_ -> takeMVar trigger3
            putMVar trigger1 ()
            threadDelay 1000
            putMVar trigger2 ()
            threadDelay 1000
            putMVar trigger3 ()
            threadDelay (100 * 1000)
            threeRootChildren <- readTVarIO rtMapMVar
            toList threeRootChildren `shouldBe` []

        it "forks a thread which automatically kills its child on normal exit" $ do
            parentExceptionMarker <- newEmptyMVar
            parentFinishMarker <- newEmptyMVar
            childStartMarker <- newEmptyMVar
            blocker <- newEmptyMVar
            childExceptionMarker <- newEmptyMVar
            trigger <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \children -> do
                void . newChild children $ \_ -> (putMVar childStartMarker () *> takeMVar blocker)
                    `catch` \e -> putMVar childExceptionMarker (e :: AsyncException)
                takeMVar trigger
                    `catch` \e -> putMVar parentExceptionMarker (e :: AsyncException)
                putMVar parentFinishMarker ()
            takeMVar childStartMarker
            runningRootChildren <- readTVarIO rtMapMVar
            (length . toList) runningRootChildren `shouldBe` 1
            putMVar trigger ()
            takeMVar parentFinishMarker
            threadDelay (10 * 1000)
            finishedRootChildren <- readTVarIO rtMapMVar
            toList finishedRootChildren `shouldBe` []
            caughtExceptionByChild <- readMVar childExceptionMarker
            caughtExceptionByChild `shouldBe` ThreadKilled
            isParentNotKilled <- isEmptyMVar parentExceptionMarker
            isParentNotKilled `shouldBe` True

    describe "killThreadHierarchy" $ do
        it "kills running thread in given ThreadMap" $ do
            startMarker <- newEmptyMVar
            blocker <- newEmptyMVar
            exceptionMarker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> (putMVar startMarker () *> takeMVar blocker)
                `catch` \e -> putMVar exceptionMarker (e :: AsyncException)
            takeMVar startMarker
            currentRootChildren <- readTVarIO rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            killThreadHierarchy rootThreadMap
            caughtException <- readMVar exceptionMarker
            caughtException `shouldBe` ThreadKilled
            remainingRootChildren <- readTVarIO rtMapMVar
            toList remainingRootChildren `shouldBe` []

        it "kills all running thread in given ThreadMap" $ do
            startMarker1 <- newEmptyMVar
            exceptionMarker1 <- newEmptyMVar
            startMarker2 <- newEmptyMVar
            exceptionMarker2 <- newEmptyMVar
            startMarker3 <- newEmptyMVar
            exceptionMarker3 <- newEmptyMVar
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> (putMVar startMarker1 () *> takeMVar blocker)
                `catch` \e -> putMVar exceptionMarker1 (e :: AsyncException)
            void . newChild rootThreadMap $ \_ -> (putMVar startMarker2 () *> takeMVar blocker)
                `catch` \e -> putMVar exceptionMarker2 (e :: AsyncException)
            void . newChild rootThreadMap $ \_ -> (putMVar startMarker3 () *> takeMVar blocker)
                `catch` \e -> putMVar exceptionMarker3 (e :: AsyncException)
            takeMVar startMarker1
            takeMVar startMarker2
            takeMVar startMarker3
            currentRootChildren1 <- readTVarIO rtMapMVar
            (length . toList) currentRootChildren1 `shouldBe` 3
            killThreadHierarchy rootThreadMap
            caughtException1 <- readMVar exceptionMarker1
            caughtException2 <- readMVar exceptionMarker2
            caughtException3 <- readMVar exceptionMarker3
            caughtException1 `shouldBe` ThreadKilled
            caughtException2 `shouldBe` ThreadKilled
            caughtException3 `shouldBe` ThreadKilled
            currentRootChildren2 <- readTVarIO rtMapMVar
            toList currentRootChildren2 `shouldBe` []

        it "kills entire hierarchy of threads" $ do
            childStartMarker <- newEmptyMVar
            blocker <- newEmptyMVar
            childExceptionMarker <- newEmptyMVar
            parentExceptionMarker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \children -> do
                void . newChild children $ \_ -> (putMVar childStartMarker () *> takeMVar blocker)
                    `catch` \e -> putMVar childExceptionMarker (e :: AsyncException)
                takeMVar blocker
                    `catch` \e -> putMVar parentExceptionMarker (e :: AsyncException)
            takeMVar childStartMarker
            currentRootChildren1 <- readTVarIO rtMapMVar
            (length . toList) currentRootChildren1 `shouldBe` 1
            killThreadHierarchy rootThreadMap
            caughtExceptionByChild <- readMVar childExceptionMarker
            caughtExceptionByChild `shouldBe` ThreadKilled
            caughtExceptionByParent <- readMVar parentExceptionMarker
            caughtExceptionByParent `shouldBe` ThreadKilled
            threadDelay 1000
            currentRootChildren2 <- readTVarIO rtMapMVar
            toList currentRootChildren2 `shouldBe` []

    describe "Raced termination" $ do
        it "gracefully terminates threads under race condition" $ do
            let volume = 10000
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            triggers <- for [1..volume] $ \_ -> do
                trigger <- newEmptyMVar
                void . newChild rootThreadMap $ \_ -> takeMVar trigger
                return trigger
            currentRootChildren <- readTVarIO rtMapMVar
            let childrenList = toList currentRootChildren
            length childrenList `shouldBe` volume

            for_ triggers $ \trigger -> putMVar trigger ()
            killThreadHierarchy rootThreadMap

            remainingRootChildren <- readTVarIO rtMapMVar
            toList remainingRootChildren `shouldBe` []

        it "gracefully terminates multi-layered threads under race condition" $ do
            let volume = 10000
            blocker <- newEmptyMVar
            trigger <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \childThreadMap@(ThreadMap chMapMVar) -> do
                for_ [1..volume] . const . newChild childThreadMap $ \_ -> takeMVar blocker
                currentGrandChildren <- readTVarIO chMapMVar
                let grandChildrenList = toList currentGrandChildren
                length grandChildrenList `shouldBe` volume
                takeMVar trigger

            putMVar trigger ()
            -- You may need to adjust this delay to reproduce race condition.
            threadDelay (100 * 10^3)
            killThreadHierarchy rootThreadMap

            remainingRootChildren <- readTVarIO rtMapMVar
            toList remainingRootChildren `shouldBe` []

    describe "Finish marker MVar () inside of ThreadMap" $ do
        it "is filled by () on thread normal exit" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> threadDelay (10 * 1000)
            currentRootChildren <- readTVarIO rtMapMVar
            let (_, FinishMarker markerMVar) = head $ toList currentRootChildren
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "is filled by () on thread exit by kill" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren <- readTVarIO rtMapMVar
            let (threadID, FinishMarker markerMVar) = head $ toList currentRootChildren
            killThread threadID
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "is available after thread normal exit" $ do
            trigger <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar trigger
            currentRootChildren1 <- readTVarIO rtMapMVar
            let (_, FinishMarker markerMVar) = head $ toList currentRootChildren1
            putMVar trigger ()
            threadDelay (10 * 1000)
            currentRootChildren2 <- readTVarIO rtMapMVar
            toList currentRootChildren2 `shouldBe` []
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "is available after the thread was killed" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren1 <- readTVarIO rtMapMVar
            let (threadID, FinishMarker markerMVar) = head $ toList currentRootChildren1
            killThread threadID
            threadDelay (10 * 1000)
            currentRootChildren2 <- readTVarIO rtMapMVar
            toList currentRootChildren2 `shouldBe` []
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "works with many threads in normal exit scenario" $ do
            let volume = 10000
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            triggers <- for [1..volume] $ \_ -> do
                trigger <- newEmptyMVar
                void . newChild rootThreadMap $ \_ -> takeMVar trigger
                return trigger
            currentRootChildren <- readTVarIO rtMapMVar
            let childrenList = toList currentRootChildren
            length childrenList `shouldBe` volume

            for_ triggers $ \trigger -> putMVar trigger ()
            let waitForCleanup = do
                    remaining <- readTVarIO rtMapMVar
                    unless (toList remaining == []) $ threadDelay (10 * 1000) >> waitForCleanup
            waitForCleanup
            remainingRootChildren <- readTVarIO rtMapMVar
            toList remainingRootChildren `shouldBe` []
            for_ childrenList $ \(_, FinishMarker markerMVar) -> do
                mark <- takeMVar markerMVar
                mark `shouldBe` ()

        it "works with many threads in killed exit scenario" $ do
            let volume = 10000
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            for_ [1..volume] . const . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren <- readTVarIO rtMapMVar
            let childrenList = toList currentRootChildren
            length childrenList `shouldBe` volume
            for_ childrenList $ \(threadID, _) -> killThread threadID
            let waitForCleanup = do
                    remaining <- readTVarIO rtMapMVar
                    unless (toList remaining == []) $ threadDelay (10 * 1000) >> waitForCleanup
            waitForCleanup
            remainingRootChildren <- readTVarIO rtMapMVar
            toList remainingRootChildren `shouldBe` []
            for_ childrenList $ \(_, FinishMarker markerMVar) -> do
                mark <- takeMVar markerMVar
                mark `shouldBe` ()

    describe "Overkill" $ do
        it "Killing normally exited thread doesn't take any effect" $ do
            trigger <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar trigger
            currentRootChildren <- readTVarIO rtMapMVar
            let (threadID, _) = head $ toList currentRootChildren
            threadDelay (10 * 1000)
            putMVar trigger ()
            threadDelay (10 * 1000)
            killThread threadID
            remainingRootChildren <- readTVarIO rtMapMVar
            toList remainingRootChildren `shouldBe` []

        it "Killing already killed thread doesn't take any effect" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren <- readTVarIO rtMapMVar
            let (threadID, _) = head $ toList currentRootChildren
            threadDelay (10 * 1000)
            killThread threadID
            threadDelay (10 * 1000)
            remainingRootChildren1 <- readTVarIO rtMapMVar
            toList remainingRootChildren1 `shouldBe` []
            killThread threadID
            threadDelay (10 * 1000)
            remainingRootChildren2 <- readTVarIO rtMapMVar
            toList remainingRootChildren2 `shouldBe` []
