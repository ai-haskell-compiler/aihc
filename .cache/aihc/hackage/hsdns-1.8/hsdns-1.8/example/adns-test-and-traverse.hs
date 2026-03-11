module Main ( main ) where

import ADNS
import ADNS.Base
import Control.Concurrent.MVar
import System.Environment

main :: IO ()
main = initResolver [NoErrPrint, NoServerWarn] $ \resolver -> do
  args <- getArgs
  case args of
    [name]   -> traverseDNSSEC resolver name
    [t,name] -> work resolver (read t) name
    _ -> putStrLn "Usage: t [typeid] fqdn"

-- | Test function to see the raw results of a given query type
work :: Resolver -> RRType -> String -> IO ()
work resolver t n = do
  putStrLn $ showString "Querying " . shows t $ showString " for " n
  print =<< takeMVar =<< resolver n t [QuoteOk_Query]

-- | Example implementation to traverse a DNSSEC signed zone.
--
-- This implementation is clearly wrong, because any real zone traversal
-- is done using the NSEC records in the authority section of a NXDOMAIN
-- response.
--
-- Unfortunly the adns library does not provide access to other sections
-- than the answer section, so this walk is done by querying NSEC directly.
--
-- If there are signed subzones, the traversal switches to the subzone
-- and stops if this subzone is traversed. You may continue the traversal
-- by providing the next entry after the subzone.
--
-- You may try this mechanism on "dnssec.iks-jena.de"

traverseDNSSEC :: Resolver -> String -> IO ()
traverseDNSSEC resolver x = do
  putStrLn x
  answer <- takeMVar =<< resolver x NSEC [QuoteOk_Query]
  case rrs answer of
     [RRNSEC y] | not (x `endsWith` ('.':y)) -> traverseDNSSEC resolver y
     _  -> return ()

endsWith :: String -> String -> Bool
endsWith x y = startsWith (reverse x) (reverse y)

startsWith :: String -> String -> Bool
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys
startsWith _      ys     = null ys
