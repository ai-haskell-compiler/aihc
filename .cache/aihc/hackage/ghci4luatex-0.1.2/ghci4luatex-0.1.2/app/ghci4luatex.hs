{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad
import GHC.Generics
import System.IO
import Data.IORef



import Network.Simple.TCP

import qualified Data.ByteString.Lazy as BL

import System.Console.CmdArgs

import Data.Aeson

import System.Process.Ghci
import qualified Data.Memoizer.Sessions as Memo

data ServerMsg = NewSession String | ContinueSession String
  deriving (Show, Eq, Generic)

instance ToJSON ServerMsg where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerMsg

data Ghci4luatexMsg = GhciMsg String | ServerMsg ServerMsg
  deriving (Show, Eq, Generic)


instance ToJSON Ghci4luatexMsg where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ghci4luatexMsg

data Ghci4luatex = Ghci4luatex
  { command  :: String
  , host :: String
  , port :: String
  }
  deriving (Data,Typeable,Show,Eq)

type GhciMemo =  Memo.SessionMemoizer String String BL.ByteString

cmdArg :: Ghci4luatex
cmdArg =  Ghci4luatex
  { command = "ghci" &= help "Command to run (defaults to ghci)"
  , host = "127.0.0.1" &= help "Host address (defaults to localhost)"
  , port = "54123" &= help "Port (defaults to 54123)"
  }
  &= verbosity
  &= summary "ghci4luatex v0.1, (C) Alice Rixte"

main :: IO ()
main = do
  Ghci4luatex str addr prt <- cmdArgs cmdArg
  case words str of
    [] -> putStrLn "Error : Empty ghci command."
    cmd : ghciArgs -> do
      v <- getVerbosity
      when (v >= Normal) $ do
        putChar '\n'
        putStrLn "(-: Starting GHCi Server :-)"
        putChar '\n'

      ghci <- startGhci v cmd ghciArgs

      when (v >= Normal) $ do
        putChar '\n'
        putStrLn "(-: GHCi server is ready :-)"
        putChar '\n'

      memo <- newIORef (Memo.initSession "main" :: GhciMemo)
      serve (Host addr) prt $ \(sock, remoteAddr) -> do
        when (v > Normal) $ putStrLn $ "New connection of " ++ show remoteAddr
        handleClient v sock ghci memo

printGhciMsg :: String -> IO ()
printGhciMsg str =
  case lines str of
    [] -> return ()
    -- [s] -> when (s /= "") $ putStrLn $ "ghci| " ++ s
    (x:q) -> do
      putStrLn $ "ghci> " ++ x
      mapM_ (putStrLn . ("ghci| " ++)) q

handleClient :: Verbosity -> Socket ->  Ghci -> IORef GhciMemo ->  IO ()
handleClient v sock ghci memo =
    loop
    where
      loop = do
        msg <- recv sock 1024
        case msg of
            Just bs -> do
              case decodeStrict bs :: Maybe Ghci4luatexMsg of
                Nothing ->
                  let json = encode (GhciResult  "ghci4luatex :: Error : Could not parse JSON message." "")
                  in do
                    hPutStr stderr $ "Error : Could not parse JSON message : "
                    hPutStr stderr $ show bs
                    hPutStr stderr "\n"
                    hFlush stderr
                    sendLazy sock json
                Just (GhciMsg s) -> do

                  m <-readIORef memo
                  json <- case Memo.lookup s m of
                    Nothing -> do
                      when (v >= Normal) $ printGhciMsg s
                      res <- execGhciCmd ghci v (s ++ "\n")
                      when (v >= Normal) $ putStrLn ""
                      let json = encode res <> "\n"
                      modifyIORef memo (Memo.storeResult s json)
                      return json
                    Just json -> do
                      when (v >= Loud) $ do
                        printGhciMsg s
                        putStrLn "Memoized !"
                        putStrLn ""
                      modifyIORef memo Memo.nextCmd
                      return json
                  sendLazy sock json
                Just (ServerMsg (NewSession s)) -> do
                  modifyIORef memo (Memo.newSession s)
                  when (v >= Normal) $ do
                    putStrLn $ "--- New session : " ++ show s  ++ "---\n"
                Just (ServerMsg (ContinueSession s)) -> do
                  modifyIORef memo (Memo.continueSession s)
                  when (v >= Normal) $ do
                    putStrLn $ "--- Continue session : " ++ show s  ++ "---\n"

              loop
            Nothing -> when (v >= Loud) $ do
              putChar '\n'
              putStrLn "Connexion was closed"
