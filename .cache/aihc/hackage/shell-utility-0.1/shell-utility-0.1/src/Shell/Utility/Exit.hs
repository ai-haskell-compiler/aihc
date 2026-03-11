{- |
The Exit mechanism is not compositional
and thus should not be used in larger long-running programs.
However, in small command-line utilities
and especially for signaling errors in command-line arguments
it is acceptable.

The 'IO' instance is useful for 'GetOpt' and immediate exit.
The 'Either' instance is useful for 'Optparse.Applicative.eitherReader'.
-}
module Shell.Utility.Exit where

import qualified System.Exit as Exit
import qualified System.IO as IO

import Control.Applicative (Applicative)

import Data.String (IsString, fromString)


class Applicative f => Exit f where
   {- |
   Also known as 'System.Exit.die' in newer versions of 'base'.
   -}
   exitFailureMsg :: String -> f a

instance Exit IO where
   exitFailureMsg msg = do
      IO.hPutStrLn IO.stderr msg
      Exit.exitFailure

instance (IsString str) => Exit (Either str) where
   exitFailureMsg = Left . fromString

{- ToDo:
Optparse.Applicative.ReadM could also be an instance
but this would add a new dependency.
-}
