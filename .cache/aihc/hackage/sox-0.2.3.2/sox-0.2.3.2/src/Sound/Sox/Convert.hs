module Sound.Sox.Convert where

import qualified Sound.Sox.Private.Option as Option
import qualified Sound.Sox.Private.Arguments as Args
import Data.Monoid (mconcat, )

import qualified System.Process as Cmd
-- import qualified System.IO as IO
import System.Exit (ExitCode, )



{- |
> :load Sound.Sox.Convert
>
> simple Option.none "test.aiff" Option.none "test.wav"
-}
simple ::
   Option.T {- ^ source options -} ->
   FilePath {- ^ source file name -} ->
   Option.T {- ^ target options -} ->
   FilePath {- ^ target file name -} ->
   IO ExitCode
simple srcOpts srcFile dstOpts dstFile =
   Cmd.rawSystem "sox"
      (Args.decons $ mconcat $
       Option.toArguments srcOpts :
       Args.fileName srcFile :
       Option.toArguments dstOpts :
       Args.fileName dstFile :
       [])
