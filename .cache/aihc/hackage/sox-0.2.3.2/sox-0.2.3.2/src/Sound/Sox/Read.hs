module Sound.Sox.Read (
   Handle,
   open, close,
   -- withFile,
   withHandle1,
   withHandle2,
   ) where

import qualified Sound.Sox.Frame as Frame
-- import Sound.Sox.System (catchCtrlC, )

import qualified Sound.Sox.Option.Format as Option
import qualified Sound.Sox.Private.Option as OptPriv
import qualified Sound.Sox.Private.Arguments as Args
import Data.Monoid (mconcat, )

import qualified System.Process as Proc
import qualified System.IO as IO
-- import Control.Exception (bracket, )
import System.Exit (ExitCode, )


data Handle signal =
   Handle {
      pipeInput, pipeOutput, pipeError :: IO.Handle,
      processId :: Proc.ProcessHandle
   }

{-
withFile ::
   (Frame.C y) =>
   (IO.Handle -> IO (sig y))
      {- ^ Reader routine -
           e.g. 'Sound.Sox.Signal.List.put'
           or 'Data.StorableVector.hPut' -} ->
   Option.T ->
   FilePath ->
   IO ExitCode
withFile = undefined
-}

{- |
Unfortunately we cannot retrieve the sample rate using @sox@.
However there is @soxi@ for this purpose, which we may support in future.

> :load Sound.Sox.Read Sound.Sox.Signal.List
> :module + Control.Exception
> bracket (open Option.none "test.aiff") close $ \h -> withHandle2 Sound.Sox.Signal.List.getContents h >>= \x -> print (Control.Monad.Exception.Asynchronous.result x :: [Data.Int.Int16])
-}
open ::
   (Frame.C y) =>
   Option.T ->
   FilePath ->
   IO (Handle (sig y))
open opts =
   openAux undefined opts Option.none

openAux ::
   (Frame.C y) =>
   y ->
   Option.T ->
   Option.T ->
   FilePath ->
   IO (Handle (sig y))
openAux frame srcOpts dstOpts fileName =
   fmap
      (\(input,output,err,proc) ->
           Handle input output err proc)
      (Proc.runInteractiveProcess "sox"
          (Args.decons $ mconcat $
           OptPriv.toArguments srcOpts :
           Args.fileName fileName :
           OptPriv.toArguments
             (mconcat $
              dstOpts :
              Option.numberOfChannels
                 (Frame.numberOfChannels frame) :
              Option.format (Frame.format frame) :
              []) :
           Args.pipe :
           [])
          Nothing Nothing)

close :: Handle signal -> IO ExitCode
close h =
   mapM_ IO.hClose [pipeInput h, pipeOutput h, pipeError h] >>
   Proc.waitForProcess (processId h)

withHandle1 ::
   (IO.Handle -> m signal) ->
   (Handle signal -> m signal)
withHandle1 act h =
   act (pipeOutput h)

withHandle2 ::
   (IO.Handle -> m (f signal)) ->
   (Handle signal -> m (f signal))
withHandle2 act h =
   act (pipeOutput h)
