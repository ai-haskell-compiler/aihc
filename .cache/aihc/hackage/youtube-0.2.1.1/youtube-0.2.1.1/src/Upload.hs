module Main (main) where

import qualified YouTube

import qualified Data.ByteString.Lazy as BL
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent as Conc
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Process as Proc
import qualified System.IO as IO
import Utility (exitFailureMsg, )


boundary :: String
boundary = "20ura9wrejfoegsnvgengnesg893ut9834"

writeBody :: FilePath -> FilePath -> IO.Handle -> IO ()
writeBody xml video h = do
   IO.hPutStrLn h $ "--" ++ boundary
   IO.hPutStrLn h "Content-Type: application/atom+xml; charset=UTF-8"
   IO.hPutStrLn h ""
   BL.readFile xml >>= BL.hPut h
   IO.hPutStrLn h $ "--" ++ boundary
   IO.hPutStrLn h "Content-Type: video/avi"
   IO.hPutStrLn h "Content-Transfer-Encoding: binary"
   IO.hPutStrLn h ""
   BL.readFile video >>= BL.hPut h
   IO.hPutStrLn h ""
   IO.hPutStrLn h $ "--" ++ boundary ++ "--"

runCurl :: String -> String -> FilePath -> FilePath -> IO ()
runCurl developerKey auth xml video = do
   (inp,out,err,pid) <-
      Proc.runInteractiveProcess "curl" [
         "--header", "Authorization: GoogleLogin auth=" ++ auth,
         "--header", "X-GData-Key: key=" ++ developerKey,
         "--header", "GData-Version: 2",
         "--header", "Content-Type: multipart/related; boundary=\"" ++ boundary ++ "\"",
         "--header", "Slug: " ++ video,
         "--data-binary", "@-",
--         "http://localhost:8080/"
         "https://uploads.gdata.youtube.com/feeds/api/users/default/uploads"
         ]
         Nothing Nothing
   term <- MVar.newEmptyMVar
   let transfer from to =
          IO.hGetContents from >>= IO.hPutStr to >> MVar.putMVar term ()
--          BL.hGetContents from >>= BL.hPutStr to >> MVar.putMVar term ()
   _ <- Conc.forkIO $ transfer out IO.stdout
   _ <- Conc.forkIO $ transfer err IO.stderr
   writeBody xml video inp
   IO.hClose inp
   exit <- Proc.waitForProcess pid
   case exit of
      Exit.ExitFailure _ -> Exit.exitWith exit
      _ -> return ()
   MVar.takeMVar term
   MVar.takeMVar term

main :: IO ()
main = do
   args <- Env.getArgs
   case args of
      [keyPath, xml, video] -> do
         auth <- Env.getEnv YouTube.authVar
         key <- readFile keyPath
         case lines key of
            k:_ -> runCurl k auth xml video
            _ -> exitFailureMsg "empty developerKey file"
      _ -> do
         prog <- Env.getProgName
         exitFailureMsg $
            "Usage: " ++ prog ++ " developerKeyPath xmlPath videoPath"
