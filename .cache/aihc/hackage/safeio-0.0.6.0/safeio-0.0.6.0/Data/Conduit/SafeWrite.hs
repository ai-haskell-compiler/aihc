module Data.Conduit.SafeWrite
    ( safeSinkFile
    , atomicConduitUseFile
    ) where

import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Data.IORef (newIORef, writeIORef, readIORef, IORef)
import           System.IO (Handle)
import           Control.Monad.Trans.Resource
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO, MonadIO(..))

import           System.IO.SafeWrite (allocateTempFile, finalizeTempFile)

-- | Write to file 'finalname' using a temporary file and atomic move.
--
-- The file is only written if the sink runs to completion without errors. Any
-- form of early termination will cause the output to be removed.
--
-- This function is deprecated in favor of 'Data.Conduit.Binary.SinkFileCautious'
safeSinkFile :: (MonadResource m) =>
                    FilePath -- ^ Final filename
                    -> C.ConduitT B.ByteString C.Void m ()
safeSinkFile finalname = atomicConduitUseFile finalname CC.sinkHandle

-- | Conduit using a Handle in an atomic way
atomicConduitUseFile :: (MonadResource m) =>
                    FilePath -- ^ Final filename
                    -> (Handle -> C.ConduitM i o m a) -- ^ Conduit which uses a Handle
                    -> C.ConduitM i o m a
atomicConduitUseFile finalname cond = C.bracketP
                            acquire 
                            deleteTempOnError
                            action
    where
        acquire :: IO ((FilePath, Handle), IORef Bool)
        acquire = ((,) <$> allocateTempFile finalname <*> newIORef False)
        action (tdata@(_, th), completed) = do
            r <- cond th
            liftIO $ do
                finalizeTempFile finalname True tdata
                writeIORef completed True
            return r
        deleteTempOnError :: ((FilePath, Handle), IORef Bool) -> IO ()
        deleteTempOnError (tdata, completed) = do
            unlessM (readIORef completed) $
                liftIO $ finalizeTempFile finalname False tdata

        unlessM c act = c >>= flip unless act
