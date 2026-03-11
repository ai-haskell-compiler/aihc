# aws-xray-client-persistent

A Haskell client for integrating AWS X-Ray with [Persistent](https://hackage.haskell.org/package/persistent).

To use this, you may want choose to define a helper to annotate traces

```hs

-- ...

import Data.Pool
import Database.Persist.Sql
import Network.AWS.XRayClient.Persistent

-- ...

runSqlPoolXRay
  :: (backend ~ SqlBackend, MonadUnliftIO m)
  => Text
  -- ^ Subsegment name
  --
  -- The top-level subsegment will be named @\"<this> runSqlPool\"@ and the,
  -- with a lower-level subsegment named @\"<this> query\"@.
  --
  -> XRayVaultData -- ^ Vault data to trace with
  -> ReaderT backend m a
  -> Pool backend
  -> m a
runSqlPoolXRay name vaultData action pool =
  traceXRaySubsegment' vaultData (name <> " runSqlPool") id
    $ withRunInIO
    $ \run -> withResource pool $ \backend -> do
        let
          sendTrace = atomicallyAddVaultDataSubsegment vaultData
          stdGenIORef = xrayVaultDataStdGen vaultData
          subsegmentName = name <> " query"
        run . runSqlConn action =<< liftIO
          (xraySqlBackend sendTrace stdGenIORef subsegmentName backend)
```

Then you can use this in your `runDB` definition to trace DB calls

```hs
-- ...
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    pool <- getsYesod appPool
    mVaultData <- vaultDataFromRequest <$> waiRequest
      
    -- ...

    maybe runSqlPool (runSqlPoolXRay "runDB") mVaultData action' pool
```
