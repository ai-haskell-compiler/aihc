module Main where

import Language.Haskell.Colorize
import System.Console.GetOpt
import System.Environment(getArgs)
import System.IO
import System.Exit(exitFailure)
import Control.Monad(when)

main :: IO ()
main =
  do opts <- getOptions
     when (optHelp opts) showUsage
     let style = case optStyle opts of
                   ANSILight  -> ansiLight
                   ANSIDark   -> ansiDark
     interact (\xs -> render style xs "")


getOptions :: IO Options
getOptions =
  do (fs,non_opt,errs) <- getOpt Permute flags `fmap` getArgs
     case (non_opt,errs) of
       ([],[]) -> return (foldr ($) defaultOptions fs)
       _       -> mapM_ (hPutStrLn stderr) errs >> showUsage

-- | Print usage info and quit
showUsage :: IO a
showUsage = do hPutStrLn stderr (usageInfo "Available options:" flags)
               exitFailure

flags :: [ OptDescr (Options -> Options) ]
flags = [ Option []       ["ansi-dark"]
          (NoArg $ \o -> o { optStyle = ANSIDark })
          "Generate ANSI output for a dark terminal (default)"
        , Option []       ["ansi-light"]
          (NoArg $ \o -> o { optStyle = ANSILight })
          "Generate ANSI output for a light terminal"
        , Option ['h']    ["help"]
          (NoArg $ \o -> o { optHelp = True })
          "Display this help."
        ]

data OptStyle = ANSILight | ANSIDark

data Options = Options
  { optStyle :: OptStyle
  , optHelp  :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optStyle = ANSIDark
  , optHelp  = False
  }


