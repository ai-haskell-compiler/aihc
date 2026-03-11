{-
    Resolve a bunch of hostnames' A records, then resolve those
    A-record's PTR records and check whether they match. Do it
    all asynchronously. The results are printed in the order the
    answers come in.

    TODO: handle hosts that have more than one A record
-}

module Main ( main ) where

import Control.Monad            ( when, replicateM_ )
import Control.Concurrent       ( forkIO )
import Control.Concurrent.Chan  ( Chan, newChan, writeChan, readChan )
import System.Environment       ( getArgs )
import Network.Socket           ( getNameInfo, NameInfoFlag(..), SockAddr(..) )
import ADNS

data CheckResult
  = OK HostName HostAddress
  | NotOK HostName HostAddress [HostName]
  | DNSError String

printResult :: CheckResult -> IO ()
printResult (OK h a)       = do addr <- ntoa a
                                putStrLn $ "OK: "   ++ h ++ " <-> " ++ addr
printResult (NotOK h a h') = do addr <- ntoa a
                                putStrLn $ "FAIL: " ++ h ++ " -> "  ++ addr ++ " -> " ++ show h'
printResult (DNSError msg) = putStrLn $ "ERR: " ++ msg

main :: IO ()
main = do
  names <- getArgs
  when (null names) (putStrLn "Usage: hostname [hostname ...]")
  initResolver [NoErrPrint, NoServerWarn] $ \resolver -> do
    rrChannel <- newChan :: IO (Chan CheckResult)
    mapM_ (forkIO . ptrCheck resolver rrChannel) names
    replicateM_ (length names) (readChan rrChannel >>= printResult)

ptrCheck :: Resolver -> Chan CheckResult -> HostName -> IO ()
ptrCheck resolver chan host = do
  let returnError t = writeChan chan (DNSError (host ++ ": cannot resolve " ++ t))
  a <- queryA resolver host
  case a of
    Just [addr] -> do
      ptr <- queryPTR resolver addr
      case ptr of
        Just names | host `elem` names -> writeChan chan (OK host addr)
                   | otherwise         -> writeChan chan (NotOK host addr names)
        _                              -> ntoa addr >>= \str -> returnError ("PTR for "++str)
    _           -> returnError "A"

ntoa :: HostAddress -> IO String
ntoa ha = do r <- getNameInfo [NI_NUMERICHOST] True False (SockAddrInet 0 ha)
             case r of
               (Just hname, _) -> return hname
               _               -> fail ("getNameInfo: unexpected failure printing address "++show ha)
