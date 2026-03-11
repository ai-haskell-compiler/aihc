{-|
Module      : Data.Conduit.Algorithms.Storable
Copyright   : 2018 Luis Pedro Coelho
License     : MIT
Maintainer  : luis@luispedro.org

Read/write Storable vectors
-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Data.Conduit.Algorithms.Storable
    ( writeStorableV
    , readStorableV
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import Control.Monad (when)

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit as C
import           Data.Conduit ((.|))

-- | write a Storable vector
--
-- This uses the same format as in-memory
--
-- See 'readStorableV'
writeStorableV :: forall m a. (MonadIO m, Monad m, Storable a) => C.ConduitT (VS.Vector a) B.ByteString m ()
writeStorableV = CL.mapM (liftIO. encodeStorable')
    where
        encodeStorable' :: Storable a => VS.Vector a -> IO B.ByteString
        encodeStorable' v' = VS.unsafeWith v' $ \p ->
                                    B.packCStringLen (castPtr p, VS.length v' * (sizeOf (undefined :: a)))


-- | read a Storable vector
--
-- This expects the same format as the in-memory vector.
--
-- This will break up the incoming data into vectors of the given size. The
-- last vector may be smaller if there is not enough data. Any unconsumed Bytes
-- will be leftover for the next conduit in the pipeline.
--
-- See 'writeStorableV'
readStorableV :: forall m a. (MonadIO m, Storable a) => Int -> C.ConduitM B.ByteString (VS.Vector a) m ()
readStorableV nelems = CC.chunksOfE blockBytes .| parseBlocks
    where
        blockBytes = nelems * (sizeOf a')
        a' :: a
        a' = undefined


        parseBlocks :: MonadIO m => C.ConduitT B.ByteString (VS.Vector a) m ()
        parseBlocks = C.awaitForever $ \bs -> do
            let (n,rest) = B.length bs `divMod` sizeOf a'
            r <- liftIO $ do
                v <- VSM.new n
                BU.unsafeUseAsCStringLen bs $ \(p, _) ->
                    VSM.unsafeWith v $ \vp ->
                        moveBytes (castPtr vp) p (n * sizeOf a')
                VS.unsafeFreeze v
            C.yield r
            when (rest > 0) $ do
                C.leftover (B.drop (n * sizeOf a') bs)
