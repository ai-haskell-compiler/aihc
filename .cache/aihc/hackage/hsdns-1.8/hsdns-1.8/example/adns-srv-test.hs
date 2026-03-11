{-
    Resolve a hostnames' SRV records, then show it.

    TODO: Add the ability to perform multiple requests.

    TODO: I am confused. This code does not appear to work. Running
          "adns-srv-test _sip._udp.cryp.to" prints no useful data even though a
          SRV record exists.

-}

module Main ( main ) where

import Control.Monad            ( when )
import System.Environment       ( getArgs )
import ADNS

main :: IO ()
main = do
  names <- getArgs
  when (length names /= 1) (fail "Usage: hostname")
  initResolver [Debug] $ \resolver -> do
    a <- querySRV resolver (head names)
    case a of
        Just addr -> putStrLn $ "RESULT:\n" ++ concatMap (\b -> fst b ++ ":" ++ show (snd b)) addr
        _         -> fail $ "Error in SRV " ++ show (head names)
