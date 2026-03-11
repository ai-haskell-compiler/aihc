module Sound.Sox.Play where

import qualified Sound.Sox.Frame as Frame
import Sound.Sox.System (catchCtrlC, )

import qualified Sound.Sox.Option.Format as Option
import qualified Sound.Sox.Private.Option as OptPriv
import qualified Sound.Sox.Private.Arguments as Args
import Data.Monoid (mconcat, )

import qualified System.Process as Proc
import qualified System.IO as IO
import Control.Exception (bracket, )
import System.Exit (ExitCode, )



{- |
> :load Sound.Sox.Play Sound.Sox.Signal.List
>
> simple Sound.Sox.Signal.List.put Option.none 11025 (iterate (1000+) (0::Data.Int.Int16))
-}
simple ::
   (Frame.C y) =>
   (IO.Handle -> sig y -> IO ())
      {- ^ Writer routine -
           e.g. 'Sound.Sox.Signal.List.put'
           or 'Data.StorableVector.hPut' -} ->
   Option.T ->
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
   Int
      {- ^ sample rate -} ->
   sig y ->
   IO ExitCode
extended write srcOpts dstOpts sampleRate stream =
   bracket
      {-
      Formerly we called 'play' here.
      On Windows the SoX package does not install a 'play' command.
      However using the '-d' argument for the destination always works.
      -}
      (Proc.runInteractiveProcess "sox"
          (Args.decons $ mconcat $
           OptPriv.toArguments
             (mconcat $
              srcOpts :
              Option.numberOfChannels
                 (Frame.withSignal Frame.numberOfChannels stream) :
              Option.sampleRate sampleRate :
              Option.format (Frame.withSignal Frame.format stream) :
              []) :
           Args.pipe :
           OptPriv.toArguments dstOpts :
           Args.single "-d" :
           [])
          Nothing Nothing)
      (\(input,output,err,_proc) ->
          mapM_ IO.hClose [input, output, err])
      (\(input,_,_,proc) ->
         catchCtrlC >>
         write input stream >>
         return proc)
   -- wait for end of replay
   >>= Proc.waitForProcess
