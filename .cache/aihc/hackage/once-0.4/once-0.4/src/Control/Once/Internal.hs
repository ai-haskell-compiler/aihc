{-# LANGUAGE LambdaCase #-}
module Control.Once.Internal where
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Monad
import Control.Concurrent.MVar

once0 :: IO a -> IO (IO a)
once0 act = do
    ref <- newMVar Nothing
    pure $ takeMVar ref >>= \case
        Nothing -> do
          value <- act
          putMVar ref (Just value)
          pure value
        Just value -> do
          putMVar ref (Just value)
          pure value

once1 :: (Eq a, Hashable a) => (a -> IO b) -> IO (a -> IO b)
once1 fn = do
    ref <- newMVar HM.empty
    pure $ \arg -> do
        m <- takeMVar ref
        (new, action) <- case HM.lookup arg m of
          Just action -> pure (m, action)
          Nothing     -> do
            action <- once0 $ fn arg
            pure (HM.insert arg action m, action)
        putMVar ref new
        action
