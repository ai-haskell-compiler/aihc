import Control.DeepSeq
import Control.Exception
import Criterion.Main hiding (defaultConfig)
import qualified Database.Postgres.Temp as Temp
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Transaction as PS
import Database.PostgreSQL.Transact



data Once a = Once { unOnce :: a }

instance NFData (Once a) where
  rnf x = seq x ()

setup :: IO (Temp.Cache, Once Temp.DB, Once PS.Connection)
setup = do
  cacheInfo <- Temp.setupInitDbCache Temp.defaultCacheConfig
  let theConfig = Temp.defaultConfig <> Temp.cacheConfig cacheInfo
  (db, conn) <- bracketOnError (Temp.startConfig theConfig) (either mempty Temp.stop) $ \case
    Left e -> throwIO e
    Right db -> bracketOnError (PS.connectPostgreSQL $ Temp.toConnectionString db) PS.close $ \conn ->
      pure (db, conn)

  pure (cacheInfo, Once db, Once conn)

cleanup :: (Temp.Cache, Once Temp.DB, Once PS.Connection) -> IO ()
cleanup (x, Once y, Once z) = do
  Temp.cleanupInitDbCache x
  Temp.stop y
  PS.close z

setupWith :: (PS.Connection -> Benchmark) -> Benchmark
setupWith f = envWithCleanup setup cleanup $ \ ~(_, _, Once x) -> f x

main :: IO ()
main = defaultMain
  [ setupWith $ \conn -> bench "rollback" $ whnfIO $
      runDBT (rollback $ pure ()) PS.ReadCommitted conn
  , setupWith $ \conn -> bench "abort" $ whnfIO $
      handle (\Abort -> pure ()) $ runDBT (abort $ pure ()) PS.ReadCommitted conn
  ]
