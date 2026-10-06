-- | Unbounded channels. A channel is a linked list of 'MVar' cells. The
-- read end and the write end each point to a cell of the list.
module Control.Concurrent.Chan
  ( Chan,
    newChan,
    writeChan,
    readChan,
    dupChan,
    getChanContents,
    writeList2Chan,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Exception.Base (mask_)
import GHC.IO.Unsafe (unsafeInterleaveIO)
import Prelude

-- | An unbounded channel.
data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

instance Eq (Chan a) where
  Chan readLeft _ == Chan readRight _ = readLeft == readRight

-- | The cells of a channel. An empty cell is the end of the list.
type Stream a = MVar (Item a)

-- | A value and the cell that comes after it.
data Item a = Item a (Stream a)

-- | Make an empty channel.
newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)

-- | Write a value to a channel.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) value = do
  newHole <- newEmptyMVar
  mask_ $ modifyMVar_ writeVar $ \oldHole -> do
    putMVar oldHole (Item value newHole)
    return newHole

-- | Read the next value from a channel. Wait when the channel is empty.
readChan :: Chan a -> IO a
readChan (Chan readVar _) =
  modifyMVar readVar $ \readEnd -> do
    Item value newReadEnd <- readMVar readEnd
    return (newReadEnd, value)

-- | Make a copy of a channel. The copy starts empty. Each value that is
-- written to one of the two channels after this call is also in the other.
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- readMVar writeVar
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)

-- | Read all the values of a channel as a lazy list.
getChanContents :: Chan a -> IO [a]
getChanContents chan = unsafeInterleaveIO $ do
  value <- readChan chan
  rest <- getChanContents chan
  return (value : rest)

-- | Write all the values of a list to a channel.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan chan = mapM_ (writeChan chan)
