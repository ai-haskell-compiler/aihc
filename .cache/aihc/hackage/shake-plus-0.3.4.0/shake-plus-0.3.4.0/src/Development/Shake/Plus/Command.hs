{- |
   Module     : Development.Shake.Plus.Command
   License    : MIT
   Stability  : experimental

Utilities in "Development.Shake.Command" lifted to `MonadAction`.
-}
module Development.Shake.Plus.Command (
  command
, command_
, Development.Shake.cmd
, Development.Shake.cmd_
, Development.Shake.Command.unit
, Development.Shake.Command.CmdArgument(..)
, Development.Shake.Command.CmdArguments(..)
, Development.Shake.Command.IsCmdArgument(..)
, (:->)
, Development.Shake.Command.Stdout(..)
, Development.Shake.Command.StdoutTrim(..)
, Development.Shake.Command.Stderr(..)
, Development.Shake.Command.Stdouterr(..)
, Development.Shake.Command.Exit(..)
, Development.Shake.Command.Process(..)
, Development.Shake.Command.CmdTime(..)
, Development.Shake.Command.CmdLine(..)
, Development.Shake.Command.FSATrace(..)
, Development.Shake.Command.CmdResult
, Development.Shake.Command.CmdString
, Development.Shake.Command.CmdOption(..)
) where

import           Control.Exception.Extra
import           Development.Shake           (CmdOption, CmdResult)
import qualified Development.Shake
import           Development.Shake.Command   ((:->), CmdArguments)
import qualified Development.Shake.Command
import           Development.Shake.Plus.Core
import           RIO

-- | Lifted version of `Development.Shake.command`.
command :: (Partial, CmdResult r, MonadAction m) => [CmdOption] -> String -> [String] -> m r
command x y z = liftAction $ Development.Shake.command x y z

-- | Lifted version of `Development.Shake.command_`.
command_ :: (Partial, MonadAction m) => [CmdOption] -> String -> [String] -> m ()
command_ x y z = liftAction $ Development.Shake.command_ x y z
