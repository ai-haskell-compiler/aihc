module Sound.Sox.System where

import qualified System.Posix.Signals as Signals
import Control.Exception (bracket, )

{- |
Disable sigPIPE for a local action.
If we don't call this, GHCi quits,
when the playing command is aborted with CTRL-C.
Unfortunately there doesn't seem to be another way of doing this.
-}
catchCtrlC :: IO ()
catchCtrlC =
   Signals.installHandler Signals.sigPIPE
      Signals.Ignore Nothing
    >> return ()


{-
This won't help in GHCi,
since the old handler will terminate GHCi
as soon as it is installed, again.
-}
ignoreCtrlC :: IO a -> IO a
ignoreCtrlC act =
   bracket
      (Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing)
      (\handler ->
          Signals.installHandler Signals.sigPIPE handler Nothing)
      (const act)
