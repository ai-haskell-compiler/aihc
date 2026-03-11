module Server.Option (
   T(..),
   get,
   printVerbose,
   ) where

import qualified Shell.Utility.Verbosity as Verbosity
import Shell.Utility.Verbosity (Verbosity)
import Shell.Utility.ParseArgument (parseNumber)
import Shell.Utility.Exit (exitFailureMsg)

import System.Console.GetOpt
          (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Environment (getArgs, getProgName, )
import System.Exit (exitSuccess, )

import Control.Monad (when, )


data T =
   Cons {
      verbosity :: Verbosity,
      port :: Int
   }
   deriving (Show)


deflt :: T
deflt =
   Cons {
      verbosity = Verbosity.normal,
      port = 8080
      -- other options might control maximum values for some dimensions in the games
   }


printVerbose :: (Show a) => T -> Verbosity -> a -> IO ()
printVerbose opt verb a =
   when (verb <= verbosity opt) $ print a


{-
Guide for common Linux/Unix command-line options:
  http://www.faqs.org/docs/artu/ch10s05.html
-}
description :: [OptDescr (T -> IO T)]
description =
   Option ['h'] ["help"]
      (NoArg $ \ _flags -> do
         programName <- getProgName
         putStrLn
            (usageInfo ("Usage: " ++ programName ++ " [OPTIONS]") description)
         exitSuccess)
      "show options" :
   Option ['v'] ["verbose"]
      (flip ReqArg "LEVEL" $ \str flags ->
         fmap (\verb -> flags{verbosity = verb}) $ Verbosity.parse str)
      "level of verbosity" :
   Option ['p'] ["port"]
      (flip ReqArg "NUMBER" $ \str flags ->
         fmap (\p -> flags{port = fromInteger p}) $
         parseNumber "port" (\n -> 0<n && n<0x10000) "a positive 16-bit number" str)
      "port number" :
   []


get :: IO T
get = do
   argv <- getArgs
   let (opts, files, errors) = getOpt RequireOrder description argv
   when (not $ null errors) $
      exitFailureMsg (init (concat errors))
   when (not $ null files) $
      exitFailureMsg $
         "Do not know what to do with arguments " ++ show files
   foldl (>>=)
      (return deflt)
      opts
