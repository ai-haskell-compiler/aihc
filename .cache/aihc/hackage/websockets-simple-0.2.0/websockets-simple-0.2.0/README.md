# websockets-simple

Provides for a slightly more composable structure for websocket apps:

```haskell
data Input = Increment | Decrement
  deriving (FromJSON)

data Output = Value Int
  deriving (ToJSON)

myApp :: MonadBaseControl IO m => m (WebSocketsApp m Input Output)
myApp = do
  countRef <- liftIO $ newIORef 0
  emitter <- liftIO $ newIORef (Nothing :: Async ())

  let killEmitter = do
        mThread <- liftIO $ readIORef emitter
        case mThread of
          Nothing -> pure ()
          Just thread -> cancel thread

  pure WebSocketsApp
    { onOpen = \WebSocketsAppParams{send} ->
        liftBaseWith $ \runInBase -> do
          thread <- async $ forever $ do
            count <- readIORef countRef
            runInBase $ send $ Value count
            threadDelay 1000000 -- every second, emit the current value
          writeIORef emitter (Just thread)
    , onReceive = \WebSocketsAppParams{send,close} x -> do
        count <- liftIO $
          ( modifyIORef countRef $ case x of
              Increment -> (+ 1)
              Decrement -> (-) 1
          ) *> readIORef countRef
        if count >= 10 || count <= -10
          then close
          else send (Value count)
    , onClose = \mReason -> do
        killEmitter
        case mReason of
          Nothing -> liftIO $ writeIORef countRef 0
          Just _ -> pure ()
    }
```
