{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
import           Control.Exception
import           Database.PostgreSQL.LibPQ.Notify
import           Test.Hspec
import qualified Database.Postgres.Temp as Temp
import           Control.Concurrent.Async
import           Data.IORef
import           Control.Concurrent
import           Data.Foldable
import           Control.Monad ((<=<))
import           Database.PostgreSQL.Simple.Options as Options
import qualified Database.PostgreSQL.LibPQ as PQ

main :: IO ()
main = hspec spec

aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  (var, stopper, asyncer) <- runIO $
    (,,) <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Nothing
  let theStart :: IO a
      theStart = do

        thread <- async $ do
          withFunc $ \x -> do
            putMVar var x
            takeMVar stopper
          pure $ error "Don't evaluate this"

        writeIORef asyncer $ Just thread

        either pure pure =<< (wait thread `race` takeMVar var)

      theStop :: a -> IO ()
      theStop _ = do
        putMVar stopper ()
        traverse_ cancel =<< readIORef asyncer

  beforeAll theStart $ afterAll theStop $ specWith

withSetup :: ((PQ.Connection, PQ.Connection) -> IO ()) -> IO ()
withSetup f = either throwIO pure <=< Temp.withDbCache $ \dbCache ->
  Temp.withConfig (Temp.defaultConfig <> Temp.cacheConfig dbCache) $ \db -> do
    let localhostOpts = Temp.toConnectionOptions db
    bracket (PQ.connectdb (Options.toConnectionString localhostOpts))
      PQ.finish
      $ \c -> bracket (PQ.connectdb (Options.toConnectionString localhostOpts))
          PQ.finish
          $ \c1 -> f (c, c1)

spec :: Spec
spec = aroundAll withSetup $ do
  describe "getNotificationWithConfig" $ describe "returns a notification" $ do
    it "before" $ \(conn1, conn2) -> do
      let initialChannel = "channel"
          initialData = "hi!"

      _ <- PQ.exec conn2 $ "LISTEN " <> initialChannel
      _ <- PQ.exec conn1 $ "NOTIFY " <> initialChannel <> ", '" <> initialData <>"';"

      Right PQ.Notify {..} <- getNotification conn2
      notifyRelname `shouldBe` initialChannel
      notifyExtra `shouldBe` initialData

    it "after file notifications are registered" $ \(conn1, conn2) -> do
      ender <- newEmptyMVar
      beforeWaiter <- newEmptyMVar
      let initialChannel = "channel"
          initialData = "hi!"
      _ <- PQ.exec conn2 $ "LISTEN " <> initialChannel
      _ <- forkIO $ do
             takeMVar beforeWaiter
             _ <- PQ.exec conn1 ("NOTIFY " <> initialChannel <> ", '" <> initialData <>"';")
             putMVar ender ()

      let config = defaultConfig
            { beforeWait = putMVar beforeWaiter ()
            }

      Right PQ.Notify {..} <- getNotificationWithConfig config conn2
      notifyRelname `shouldBe` initialChannel
      notifyExtra `shouldBe` initialData
