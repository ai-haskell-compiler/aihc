module Main (main) where

import qualified YouTube

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent as Conc
-- import qualified System.Posix.Env as PosixEnv
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Process as Proc
import qualified System.IO.Error as Err
import qualified System.IO as IO
import Control.Exception (bracket_, )
import Utility (exitFailureMsg, )

import Control.Monad (when, )
import Control.Functor.HT (void, )
import Data.List (isPrefixOf, )
import Data.List.HT (maybePrefixOf, )
import Data.Maybe (mapMaybe, )


runCurl :: String -> String -> String -> IO String
runCurl user passwd source = do
   (inp,out,err,pid) <-
      Proc.runInteractiveProcess "curl" [
        "--location", "https://www.google.com/accounts/ClientLogin",
        "--data",
           "Email=" ++ user ++ 
           "&Passwd=" ++ passwd ++
           "&service=youtube&source=" ++ source,
        "--header", "Content-Type:application/x-www-form-urlencoded"]
         Nothing Nothing
   errTerm <- MVar.newEmptyMVar
   authVar <- MVar.newEmptyMVar
   void $ Conc.forkIO $ IO.hGetContents out >>= MVar.putMVar authVar
   void $ Conc.forkIO $ IO.hGetContents err >>= IO.hPutStr IO.stderr >> MVar.putMVar errTerm ()
   IO.hClose inp
   exit <- Proc.waitForProcess pid
   case exit of
      Exit.ExitFailure _ -> Exit.exitWith exit
      _ -> return ()
   MVar.takeMVar errTerm
   MVar.takeMVar authVar


run :: Maybe String -> Maybe String -> IO ()
run mUser mSource = do
   user <-
      case mUser of
         Nothing -> Env.getEnv YouTube.userVar
         Just x -> return x
   source <-
      case mSource of
         Nothing ->
            Err.catchIOError
               (Env.getEnv YouTube.sourceVar)
               (\err ->
                  if Err.isDoesNotExistError err
                    then return "Haskell-YouTube"
                    else Err.ioError err)
         -- Nothing -> PosixEnv.getEnvDefault YouTube.sourceVar "Haskell-YouTube"
         Just x -> return x

   putStr "Password: "
   IO.hFlush IO.stdout
   echoMode <- IO.hGetEcho IO.stdin
   passwd <-
      bracket_
         (IO.hSetEcho IO.stdin False)
         (IO.hSetEcho IO.stdin echoMode)
         getLine
   putStrLn ""

   response <- runCurl user passwd source
   when (any (isPrefixOf "Error=") $ lines response) $
      exitFailureMsg response
   case mapMaybe (maybePrefixOf "Auth=") $ lines response of
      [] ->
         exitFailureMsg $
            "no Auth assignment found in server response:\n" ++ response
      auth : _ -> do
         putStrLn "please do the following before running youtube-upload:"
         putStrLn $ "export " ++ YouTube.authVar ++ "=" ++ auth
         {-
         We cannot change environment variables of the calling Shell this way.
         PosixEnv.setEnv YouTube.authVar auth True
         -}

exitUsage :: IO ()
exitUsage = do
   prog <- Env.getProgName
   exitFailureMsg $
      "Usage: " ++ prog ++ " [youtubeEMail [developerSource]]"

main :: IO ()
main = do
   args0 <- Env.getArgs
   case args0 of
      "--help" : _ -> exitUsage
      [] -> run Nothing Nothing
      [user] -> run (Just user) Nothing
      [user, source] -> run (Just user) (Just source)
      _ -> exitUsage
