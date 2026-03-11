module Sound.Sox.Write (simple, extended, manyExtended, ) where

import qualified Sound.Sox.Frame as Frame
import Sound.Sox.System (catchCtrlC, )

import qualified Sound.Sox.Option.Format as Option
import qualified Sound.Sox.Private.Option as OptPriv
import qualified Sound.Sox.Private.Arguments as Args
import Data.Monoid (mconcat, )

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import qualified System.Process as Proc
import qualified System.IO as IO
import Control.Exception (bracket, )
import System.Exit (ExitCode, )


{- |
Sox determines the output format
from the filename extension or from 'Option.format'.
Make sure that you provide one of them.

> :load Sound.Sox.Write Sound.Sox.Signal.List
>
> simple Sound.Sox.Signal.List.put Option.none "test.aiff" 11025 (take 100 $ iterate (1000+) (0::Data.Int.Int16))
-}
simple ::
   (Frame.C y) =>
   (IO.Handle -> sig y -> IO ())
      {- ^ Writer routine -
           e.g. 'Sound.Sox.Signal.List.put'
           or 'Data.StorableVector.hPut' -} ->
   Option.T ->
   FilePath ->
   Int
      {- ^ sample rate -} ->
   sig y ->
   IO ExitCode
simple write opts =
   extended write Option.none opts

extended ::
   (Frame.C y) =>
   (IO.Handle -> sig y -> IO ())
      {- ^ Writer routine -
           e.g. 'Sound.Sox.Signal.List.put'
           or 'Data.StorableVector.hPut' -} ->
   Option.T
      {- ^ source options, usually none -} ->
   Option.T
      {- ^ target options -} ->
   FilePath ->
   Int
      {- ^ sample rate -} ->
   sig y ->
   IO ExitCode
extended write srcOpts dstOpts fileName sampleRate signal =
   bracket
      (open srcOpts dstOpts fileName sampleRate signal)
      close
      (\(input,_,_,proc) ->
         catchCtrlC >>
         write input signal >>
         return proc)
   -- get exit code, e.g. when options were wrong
   >>= Proc.waitForProcess

{- |
The traversable functor @f@ might be 'Maybe' or '[]'.
It allows you to write to many files simultaneously
and returns the exit codes of all writing processes.
-}
manyExtended ::
   (Frame.C y, Trav.Traversable f) =>
   (f IO.Handle -> sig y -> IO ())
      {- ^ Writer routine -
           e.g. 'Sound.Sox.Signal.List.put'
           or 'Data.StorableVector.hPut' -} ->
   Option.T
      {- ^ source options, usually none -} ->
   Option.T
      {- ^ target options -} ->
   f FilePath ->
   Int
      {- ^ sample rate -} ->
   sig y ->
   IO (f ExitCode)
manyExtended write srcOpts dstOpts fileNames sampleRate signal =
   bracket
      (Trav.traverse
         (\fileName -> open srcOpts dstOpts fileName sampleRate signal)
         fileNames)
      (Fold.traverse_ close)
      (\handles ->
         catchCtrlC >>
         write (fmap (\(input,_,_,_) -> input) handles) signal >>
         return (fmap (\(_,_,_,proc) -> proc) handles))
   -- get exit code, e.g. when options were wrong
   >>= Trav.traverse Proc.waitForProcess


type Handle = (IO.Handle, IO.Handle, IO.Handle, Proc.ProcessHandle)

open ::
   (Frame.C y) =>
   Option.T
      {- ^ source options, usually none -} ->
   Option.T
      {- ^ target options -} ->
   FilePath ->
   Int
      {- ^ sample rate -} ->
   sig y ->
   IO Handle
open srcOpts dstOpts fileName sampleRate signal =
   Proc.runInteractiveProcess "sox"
      (Args.decons $ mconcat $
       OptPriv.toArguments
         (mconcat $
          srcOpts :
          Option.numberOfChannels
             (Frame.withSignal Frame.numberOfChannels signal) :
          Option.sampleRate sampleRate :
          Option.format (Frame.withSignal Frame.format signal) :
          []) :
       Args.pipe :
       OptPriv.toArguments dstOpts :
       Args.fileName fileName :
       [])
      Nothing Nothing

close :: Handle -> IO ()
close (input,output,err,_proc) =
   mapM_ IO.hClose [input, output, err]
