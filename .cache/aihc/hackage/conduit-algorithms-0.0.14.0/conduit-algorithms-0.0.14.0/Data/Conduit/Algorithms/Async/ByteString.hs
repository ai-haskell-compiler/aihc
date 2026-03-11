{-|
Module      : Data.Conduit.Algorithms.Async.ByteString
Copyright   : 2018 Luis Pedro Coelho
License     : MIT
Maintainer  : luis@luispedro.org

Higher level async processing interfaces for handling 'ByteString' objects.
-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TupleSections #-}

module Data.Conduit.Algorithms.Async.ByteString
    ( asyncMapLineGroupsC
    , asyncFilterLinesC
    ) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Algorithms.Async as CAlg
import qualified Data.Conduit.List as CL
import qualified Data.Conduit as C
import           Data.Conduit ((.|))

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Control.DeepSeq


-- | Apply a function to groups of lines
--
-- Note that this is much more efficient than the (more or less equivalent,
-- except that the intermediate lists can be of varying sizes):
--
-- @
--      CB.lines .| CC.conduitVector N .| CAlg.asyncMapC nthreads (f . V.toList)
-- @
--
-- The reason being that splitting into lines then becomes the bottleneck and
-- processing a single line is typically a tiny chunk of work so that the
-- threading overhead overwhelms the advantage of using multiple cores.
-- Instead, 'asyncMapLineGroupsC' will pass big chunks to the worker thread and
-- perform most of the line splitting _in the worker thread_.
--
-- Only Unix-style ASCII lines are supported (splitting at Bytes with value
-- 10, i.e., \\n). When Windows lines (\\r\\n) are passed to this function, this
-- results in each element having an extra \\r at the end.
asyncMapLineGroupsC :: (MonadIO m, NFData a) => Int -> ([B.ByteString] -> a) -> C.ConduitT B.ByteString a m ()
asyncMapLineGroupsC nthreads f = breakAtLineBoundary .| CAlg.asyncMapC nthreads (f . asLines)
    where
        asLines :: BL.ByteString -> [B.ByteString]
        asLines = fmap BL.toStrict . BL.split 10

        -- The purpose is to break input blocks at a line boundary
        breakAtLineBoundary :: Monad m => C.ConduitT B.ByteString BL.ByteString m ()
        breakAtLineBoundary = continue BL.empty
        continue prev = C.await >>= \case
                    Nothing -> unless (BL.null prev) $
                                C.yield prev
                    Just n -> case B.elemIndexEnd 10 n of
                        Nothing -> continue (BL.append prev (BL.fromStrict n))
                        Just p -> do
                            let (first, rest) = B.splitAt p n
                            C.yield (BL.append prev (BL.fromStrict first))
                            continue (BL.fromStrict $ B.drop 1 rest) -- skip \n char

-- | Filter lines using multiple threads
--
-- It is not clear from the types but the input is taken to unbroken lines,
-- while the output will be yielded line by line. This conduit is equivalent to
--
-- @
--      CB.lines .| CL.filer f
-- @
--
asyncFilterLinesC :: MonadIO m => Int -> (B.ByteString -> Bool) -> C.ConduitT B.ByteString B.ByteString m ()
asyncFilterLinesC n f = asyncMapLineGroupsC n (filter f) .| CL.concat
{-# INLINE asyncFilterLinesC #-}

