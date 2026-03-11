{- |
Both "System.Console.GetOpt" and "Optparse.Applicative"
do not have built-in support for number or enumeration arguments.
But there is usually a lot to check,
e.g. whether numbers are positive, not too big, etc.
We provide argument parsers here in a way that can be used
in all command-line parsing libraries.
-}
module Shell.Utility.ParseArgument where

import qualified Shell.Utility.Exit as Exit

import Control.Applicative (pure)
import Text.Printf (printf)
import Data.Maybe (listToMaybe)


parseNumber ::
   (Exit.Exit m, Read a) => String -> (a -> Bool) -> String -> String -> m a
parseNumber name constraint constraintName str =
   case reads str of
      [(n, "")] ->
         if constraint n
            then pure n
            else Exit.exitFailureMsg $
                 name ++ " must be a " ++ constraintName ++ " number"
      _ ->
         Exit.exitFailureMsg $
         name ++ " must be a number, but is '" ++ str ++ "'"


enumMaybe :: (Bounded a, Enum a, Eq str) => (a -> str) -> str -> Maybe a
enumMaybe fmt str =
   listToMaybe $ dropWhile ((str/=) . fmt) [minBound .. maxBound]

enumeration ::
   (Bounded a, Enum a) => String -> (a -> String) -> String -> Either String a
enumeration name fmt str =
   maybe (Left $ printf "unknown %s: %s" name str) Right $ enumMaybe fmt str
