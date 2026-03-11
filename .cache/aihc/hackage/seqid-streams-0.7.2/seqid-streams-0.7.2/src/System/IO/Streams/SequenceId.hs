{-# LANGUAGE BangPatterns #-}

module System.IO.Streams.SequenceId
       ( sequenceIdInputStream
       , sequenceIdOutputStream
       ) where

import           Control.Applicative ((<$>))
import           Data.IORef          (newIORef, readIORef, writeIORef)
import           Data.SequenceId     (SequenceIdError, checkSeqId,
                                      incrementSeqId)
import           System.IO.Streams   (InputStream, OutputStream)
import qualified System.IO.Streams   as Streams


------------------------------------------------------------------------------
-- | Wrap an 'System.IO.Streams.InputStream' and check for dropped or duplicated sequence IDs.
--
-- Example:
--
-- @
-- ghci> 'System.IO.Streams.fromList' [1..10 :: 'Data.SequenceId.SequenceId'] >>= 'sequenceIdInputStream' 0 'id' ('fail' . 'show') >>= 'System.IO.Streams.toList'
-- [1,2,3,4,5,6,7,8,9,10]
--
-- ghci> 'System.IO.Streams.fromList' [5..10 :: 'Data.SequenceId.SequenceId'] >>= 'sequenceIdInputStream' 0 'id' ('fail' . 'show') >>= 'System.IO.Streams.toList'
-- *** Exception: user error ('Data.SequenceId.SequenceIdError' {errType = 'Data.SequenceId.SequenceIdDropped', lastSeqId = 0, currSeqId = 5})
--
-- ghci> 'System.IO.Streams.fromList' [1..10 :: 'Data.SequenceId.SequenceId'] >>= 'sequenceIdInputStream' 5 'id' ('fail' . 'show') >>= 'System.IO.Streams.toList'
-- *** Exception: user error ('Data.SequenceId.SequenceIdError' {errType = 'Data.SequenceId.SequenceIdDuplicated', lastSeqId = 5, currSeqId = 1})
-- @
sequenceIdInputStream :: Integral s
                      => s                            -- ^ Initial sequence ID
                      -> (a -> s)                     -- ^ Function applied to each element of the stream to get the sequence ID
                      -> (SequenceIdError s -> IO ()) -- ^ Error handler
                      -> InputStream a                -- ^ 'System.IO.Streams.InputStream' to check the sequence of
                      -> IO (InputStream a)           -- ^ Pass-through of the given stream
sequenceIdInputStream initSeqId getSeqId seqIdFaultHandler inStream =
    fst <$> Streams.inputFoldM f initSeqId inStream
  where
    f lastSeqId x = do
        let currSeqId = getSeqId x
        maybe (return ()) seqIdFaultHandler $ checkSeqId lastSeqId currSeqId
        return $ max currSeqId lastSeqId


------------------------------------------------------------------------------
-- | Wrap an 'System.IO.Streams.OutputStream' to give a sequence ID for each element written.
--
-- Example:
--
-- @
-- (outStream', getSeqId) <- 'sequenceIdOutputStream' 1 outStream
-- return $ 'System.IO.Streams.Combinators.contramapM' (addSeqId getSeqId) outStream'
-- @
sequenceIdOutputStream :: Integral s
                       => s                   -- ^ Initial sequence ID
                       -> (s -> a -> b)       -- ^ Transformation function
                       -> OutputStream b      -- ^ 'System.IO.Streams.OutputStream' to count the elements of
                       -> IO (OutputStream a) -- ^ ('IO' 'SequenceId') is the action to run to get the current sequence ID
sequenceIdOutputStream i f = outputFoldM f' i
  where f' seqId bdy = (nextSeqId, f nextSeqId bdy)
          where nextSeqId = incrementSeqId seqId


outputFoldM :: (a -> b -> (a, c))  -- ^ fold function
            -> a                   -- ^ initial seed
            -> OutputStream c      -- ^ output stream
            -> IO (OutputStream b) -- ^ returns a new stream as well as an IO action to fetch the updated seed value.
outputFoldM step initSeqId outStream = do
    ref <- newIORef initSeqId
    Streams.makeOutputStream (wr ref)
  where
    wr _ Nothing    = Streams.write Nothing outStream
    wr ref (Just x) = do
        !accum <- readIORef ref
        let (!accum', !x') = step accum x
        writeIORef ref accum'
        Streams.write (Just x') outStream
