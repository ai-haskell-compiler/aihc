module Main where

import Control.Monad          (forever, void)
import qualified Data.ByteString.Char8 as C8 (putStrLn)
import Data.ByteString.Base16 (encode)

import Bindings.NFC

main :: IO ()
main = do
  ctx <- initialize
  dev <- open ctx "pn532_uart:/dev/ttyUSB0"

  case dev of
    Nothing -> putStrLn "error opening device"
    Just d  -> do
      void $ initiatorInit d
      forever $ do
        let nfcMod = NFCModulation NmtIso14443a Nbr106
        maybeTarget <- initiatorSelectPassiveTarget d nfcMod Nothing
        -- OR: maybeTarget <- initiatorPollTarget d [nfcMod] 7 5
        case maybeTarget of
          Just (NFCTargetISO14443a info) -> C8.putStrLn . encode $ iso14443aAbtUid info
          _ -> return ()
