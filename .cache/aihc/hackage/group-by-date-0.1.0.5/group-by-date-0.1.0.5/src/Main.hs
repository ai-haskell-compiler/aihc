module Main where

import qualified GroupByDate

import qualified System.FilePath.Find as FindFile
import System.FilePath.Find ((==?), )

import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Environment (getArgs, getProgName, )
import System.Exit (exitWith, ExitCode(..), )

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.Class (lift, )
import Control.Monad.HT ((<=<), )
import Control.Monad (liftM, when, )

import Data.Foldable (forM_, )
import Data.List (intercalate, )

import Text.Printf (printf, )


{-
parseCard :: String -> Exc.Exceptional String Int
parseCard str =
   case reads str of
      [(n,"")] ->
         if n>=0
           then return n
           else Exc.throw $ "negative number: " ++ str
      _ -> Exc.throw $ "could not parse cardinal " ++ show str
-}


data Flags =
   Flags {
      optHelp      :: Bool,
      optRecursive :: Bool,
      optFullDst   :: Bool,
      optFormat    :: String,
      optCommand   :: String,
      optMode      :: Mode
   }

defaultFlags :: Flags
defaultFlags =
   Flags {
      optHelp = False,
      optRecursive = False,
      optFullDst = False,
      optFormat = "%_Y/%_Y-%m/%_Y-%m-%d",
      optCommand = "mv",
      optMode = Script
   }


data Mode = Script | Move | Copy | SymLink
   deriving (Eq, Ord, Bounded, Show, Enum)

parseMode :: String -> Exc.Exceptional String Mode
parseMode str =
   case filter ((str==) . formatMode) [minBound..maxBound] of
      mode:_ -> return mode
      [] -> Exc.throw $ "unknown mode " ++ show str

formatMode :: Mode -> String
formatMode op =
   case op of
      Script -> "script"
      Move -> "move"
      Copy -> "copy"
      SymLink -> "symlink"


options :: [OptDescr (Flags -> Exc.Exceptional String Flags)]
options =
   Option ['h'] ["help"]
      (NoArg (\flags -> return $ flags{optHelp = True}))
      "show options" :
   Option ['r'] ["recursive"]
      (NoArg (\flags -> return $ flags{optRecursive = True}))
      "descent recursively into directories" :
   Option ['f'] ["format"]
      (ReqArg (\str flags -> return $ flags{optFormat = str}) "STRING")
      (printf "format string for path, default %s" $
         show $ optFormat defaultFlags) :
   Option [] ["mode"]
      (ReqArg
         (\str flags -> fmap (\mode -> flags{optMode = mode}) $ parseMode str)
         "STRING")
      (printf "mode (%s), default %s"
         (intercalate ", " $ map formatMode [minBound..maxBound])
         (show $ formatMode $ optMode defaultFlags)) :
   Option [] ["command"]
      (ReqArg (\str flags -> return $ flags{optCommand = str}) "STRING")
      (printf "command for file transfer in script, default %s" $
         show $ optCommand defaultFlags) :
   []


main :: IO ()
main =
   Exc.resolveT (\e -> putStr $ "Aborted: " ++ e ++ "\n") $ do
      argv <- lift getArgs
      let (opts, filesAndDirs, errors) = getOpt RequireOrder options argv
      when (not (null errors)) $ Exc.throwT $ concat $ errors
      flags <-
         Exc.ExceptionalT $ return $
            foldr (flip (>>=)) (return defaultFlags) opts
      when (optHelp flags) $ lift $ do
         programName <- getProgName
         putStrLn
            (usageInfo
               ("Usage: " ++ programName ++
                " [OPTIONS] FILES-AND-DIRECTORIES ...")
               options)
         exitWith ExitSuccess

      files <-
         if optRecursive flags
           then lift $ liftM concat $
                mapM
                   (FindFile.find FindFile.always
                      (FindFile.fileType ==? FindFile.RegularFile))
                   filesAndDirs
           else return filesAndDirs

      let process =
            case optMode flags of
               Script ->
                  GroupByDate.commandFromPath
                     (optFullDst flags) (optCommand flags) (optFormat flags)
               Move -> GroupByDate.movePath (optFormat flags)
               Copy -> GroupByDate.copyPath (optFormat flags)
               SymLink -> GroupByDate.symLinkPath (optFormat flags)

      forM_ files $ lift . (process <=< GroupByDate.parsePathIO)
