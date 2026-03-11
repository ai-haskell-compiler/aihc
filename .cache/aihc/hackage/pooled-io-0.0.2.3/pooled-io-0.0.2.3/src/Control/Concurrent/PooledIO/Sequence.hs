{- |
Functions for sequencing actions requested from concurrent threads.

Here is an example usage:

> import qualified Control.Concurrent.PooledIO.Independent as Parallel
> import qualified Control.Concurrent.PooledIO.Sequence as Sequence
>
> thread :: Sequence.In -> FilePath -> IO ()
> thread seqIn name = do
>    txt <- Sequence.sync seqIn $ readFile (name ++ ".in")
>    -- evaluate result with ($!!) before sending it to the sequencing thread
>    Sequence.async seqIn . writeFile (name ++ ".out") $!! processMyText txt
>    doFurtherStuff
>    -- block main program until completion
>    Sequence.sync seqIn $ return ()
>
> main :: IO ()
> main = do
>    (seqIn, seqOut) <- Sequence.new
>    void $ forkIO $ Sequence.run seqOut
>    Parallel.run $ map (thread seqIn) ["a", "b", "c"]
-}
module Control.Concurrent.PooledIO.Sequence (In, Out, new, run, async, sync) where

import qualified Control.Concurrent.Split.Chan as Chan
import qualified Control.Concurrent.Split.MVar as MVar

import Control.Monad (join, forever)

import Data.Tuple.HT (mapPair)


type Action = IO ()

newtype In = In (Chan.In Action)
newtype Out = Out (Chan.Out Action)


new :: IO (In, Out)
new = fmap (mapPair (In,Out)) Chan.new

{- |
Run the sequencing thread.
You will usually fork it.
-}
run :: Out -> IO ()
run (Out chan) =
   forever $ join $ Chan.read chan

{- |
This is primarily intended for output functions.
You should make sure that the emitted data is evaluated before calling 'async'.
Otherwise the sequencing thread will evaluate it
and thus not much parallelization will happen.

Example:

> async seqIn . writeFile "foobar.txt" $!! show result
-}
async :: In -> IO () -> IO ()
async (In chan) act =
   Chan.write chan act

{- |
This is primarily intended for input functions.
You should also call it at the end of a thread in order to make sure
that all your asynchronous actions are completed.
It will actually also wait for the actions that were requested by other threads.
However, I think this should not hurt
since after completion of the current thread
another one will be started and it will certainly begin with an input action,
which has to be queued anyway.
-}
sync :: In -> IO a -> IO a
sync chan act = do
   (resultIn, resultOut) <- MVar.newEmpty
   async chan $ act >>= MVar.put resultIn
   MVar.take resultOut
