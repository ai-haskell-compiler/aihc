module Database.PostgreSQL.TransactSpec where

import           Control.Monad              (void)
import           Control.Monad.Catch
import           Data.Typeable
import qualified Database.PostgreSQL.Simple as PS
import           Database.PostgreSQL.Simple ( Connection
                                            , Only (..)
                                            , SqlError (..)
                                            )
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Transact
import qualified Database.Postgres.Temp as Temp
import           Test.Hspec (Spec, SpecWith, describe, beforeAll, afterAll, it, runIO, shouldThrow)
import           Test.Hspec.Expectations.Lifted (shouldReturn)
import           Data.IORef
import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Foldable
import qualified Control.Exception as E
import           Control.Monad ((<=<))

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

-------------------------       Test DB Creation       -------------------------
withConn :: Temp.DB -> (Connection -> IO a) -> IO a
withConn db f = do
  let connStr = Temp.toConnectionString db
  bracket (PS.connectPostgreSQL connStr) PS.close f

withSetup :: (Connection -> IO ()) -> IO ()
withSetup f = either E.throwIO pure <=< Temp.withDbCache $ \dbCache ->
  Temp.withConfig (Temp.defaultConfig <> Temp.cacheConfig dbCache) $ \db ->
    withConn db $ \conn -> do
      void $ PS.execute_ conn $
          [sql| CREATE TABLE fruit (name VARCHAR(100) PRIMARY KEY ) |]
      f conn



withDb :: DB a -> Connection -> IO a
withDb action conn = runDB conn action

runDB :: Connection -> DB a -> IO a
runDB = flip runDBTSerializable

-------------------------        Test Utilities        -------------------------
insertFruit :: String -> DB ()
insertFruit fruit
  = void $ execute [sql| INSERT INTO fruit (name) VALUES (?) |] (Only fruit)

getFruits :: DB [String]
getFruits
  = fmap (map fromOnly)
  $ query_ [sql|SELECT name FROM fruit ORDER BY name|]

fruits :: Connection -> IO [String]
fruits conn
  = fmap (map fromOnly)
  $ PS.query_ conn [sql|SELECT name FROM fruit ORDER BY name|]

-- Simple exception type for testing
data Forbidden = Forbidden
    deriving (Show, Eq, Typeable)

instance Exception Forbidden

-------------------------         Tests Start          -------------------------
spec :: Spec
spec = describe "TransactionSpec" $ do
  aroundAll withSetup $ do
    it "execute_ happen path succeeds" $ \conn -> do
        let apple = "apple"
        runDB conn $ insertFruit apple

        fruits conn `shouldReturn` [apple]

    it "execute_ rollbacks on exception" $ \conn -> do
        flip shouldThrow (\(SqlError {}) -> True) $
            runDB conn $ do
                insertFruit "orange"
                -- This should cause an exception because of the UNIQUE
                -- constraint on 'name'
                insertFruit "apple"

        fruits conn `shouldReturn` ["apple"]

  aroundAll withSetup $ do
    it "multiple execute_'s succeed" $ \conn -> do
        runDB conn $ do
            insertFruit "grapes"
            insertFruit "orange"

        fruits conn `shouldReturn` ["grapes", "orange"]

  aroundAll withSetup $ do
    it "throwM causes a rollback" $ \conn -> do
        flip shouldThrow (\Forbidden -> True) $
            runDB conn $ do
                insertFruit "salak"
                () <- throwM Forbidden
                insertFruit "banana"

        fruits conn `shouldReturn` []

    it "query recovers when exception is caught" $ \conn -> do
        runDB conn $ do
            -- This should always happen because of the handle below
            insertFruit "banana"
            handle (\Forbidden -> insertFruit "tomato") $ do
                insertFruit "salak"
                throwM Forbidden

        fruits conn `shouldReturn` ["banana", "tomato"]

  aroundAll withSetup $ do
    it "multiple catch statements work correctly" $ \conn -> do
        runDB conn $ do
            insertFruit "banana"
            handle (\Forbidden -> insertFruit "tomato") $ do
                -- This will happen ... even if there is an exception below
                -- if we catch it
                insertFruit "blueberry"
                handle (\Forbidden -> insertFruit "frankenberry") $ do
                    insertFruit "salak"
                    throwM Forbidden

        fruits conn `shouldReturn` ["banana", "blueberry", "frankenberry"]

  aroundAll withSetup $ do
    it "alternate branches can also have savepoints" $ \conn -> do
        runDB conn $ do
            insertFruit "banana"
            catch (insertFruit "tomato" >> throwM Forbidden) $
                \Forbidden -> do
                    insertFruit "blueberry"
                    handle (\Forbidden -> insertFruit "frankenberry") $ do
                        insertFruit "salak"
                        throwM Forbidden

        fruits conn `shouldReturn` ["banana", "blueberry", "frankenberry"]

  aroundAll withSetup $ do
    it "releasing silently fails if the transaction errors" $ \conn -> do
        runDB conn $ do
            insertFruit "banana"
            catchAll (void $ execute_ [sql| ABORT |]) $
                \_ -> insertFruit "tomato"

        fruits conn `shouldReturn` []

    it "rollback ... rollbacks effects on expected finish" $ withDb $ do
      insertFruit "grapes"
      rollback $ do
        insertFruit "oranges"
        getFruits `shouldReturn` ["grapes", "oranges"]

      getFruits `shouldReturn` ["grapes"]

  aroundAll withSetup $ do
    it "rollback ... rollbacks effects on exception" $ withDb $ do
      insertFruit "grapes"
      _ :: Either Forbidden () <- try $ rollback $ do
          insertFruit "oranges"
          getFruits `shouldReturn` ["grapes", "oranges"]
          throwM Forbidden

      getFruits `shouldReturn` ["grapes"]

  aroundAll withSetup $ do
    it "abort ... abort effects on expected finish" $ \conn -> runDB conn (do
      insertFruit "grapes"
      abort $ do
        insertFruit "oranges"
        getFruits `shouldReturn` ["grapes", "oranges"]

      getFruits `shouldReturn` []
      ) `shouldThrow` (\Abort -> True)

  aroundAll withSetup $ do
    it "abort ... abort effects on exception" $ \conn -> runDB conn ( do
      insertFruit "grapes"
      _ :: Either Forbidden () <- try $ abort $ do
          insertFruit "oranges"
          getFruits `shouldReturn` ["grapes", "oranges"]
          throwM Forbidden

      getFruits `shouldReturn` []
      ) `shouldThrow` (\Abort -> True)

  aroundAll withSetup $ do
    it "abort ... abort throws a warning only when nested" $ \conn -> do
      runDB conn (abort (abort (pure ()))) `shouldThrow` (\Abort -> True)

  aroundAll withSetup $ do
    it "abort ... can't be caught" $ \conn -> do
      runDB conn (abort (pure ()) `catch` (\Abort -> pure ())) `shouldThrow` (\Abort -> True)
