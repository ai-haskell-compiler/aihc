{-|
Parse Jira wiki markup from stdin.
-}
module Main (main) where

import Prelude hiding (interact)

import Control.Exception (throw)
import Data.Text (pack)
import Data.Text.IO (interact)
import System.Exit (ExitCode (ExitFailure))
import Text.Jira.Parser (parse)


main :: IO ()
main = interact parse'
  where
    parse' t = case parse t of
      Left _ -> throw (ExitFailure 1)
      Right r -> pack (show r)
