{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.Tcp
  ( TcpSocket (..)
  , TcpState (..)
  , readProcTcpSockets
  )
  where

import           Control.Error (runExceptT, throwE)
import           Control.Monad (replicateM, void)

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Bits ((.|.), shiftL)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Text as Text

import           System.Linux.Proc.Errors (ProcError (..))
import           System.Linux.Proc.Process (ProcessId (..))
import           System.Linux.Proc.IO (readProcFile)



data TcpState
  = TcpEstablished
  | TcpSynSent
  | TcpSynReceive
  | TcpFinWait1
  | TcpFinWait2
  | TcpTimeWait
  | TcpClose
  | TcpCloseWait
  | TcpLastAck
  | TcpListen
  | TcpClosing
  | TcpNewSynReceive
  deriving (Show, Eq)

-- | TCP socket used by a process according to the `/proc/<pid>/net/tcp`
-- file of the process. Only non-debug fields are parsed and described the socket
-- data structure.
data TcpSocket = TcpSocket
  { tcpLocalAddr :: !(ByteString, Int)
  , tcpRemoteAddr :: !(ByteString, Int)
  , tcpTcpState :: !TcpState
  , tcpUid :: !Int
  , tcpInode :: !Int
  } deriving (Show)


-- | Read and parse the `/proc/<pid>/net/tcp` file. Read and parse errors are caught
-- and returned.
readProcTcpSockets :: ProcessId -> IO (Either ProcError [TcpSocket])
readProcTcpSockets pid =
  runExceptT $ do
    let fpath = mkNetTcpPath pid
    bs <- readProcFile fpath
    case Atto.parseOnly (pTcpSocketList <* Atto.endOfInput) bs of
      Left  e  -> throwE $ ProcParseError fpath (Text.pack e)
      Right ss -> pure ss


-- -----------------------------------------------------------------------------
-- Internals.

mkNetTcpPath :: ProcessId -> FilePath
mkNetTcpPath (ProcessId pid) = "/proc/" ++ show pid ++ "/net/tcp"

-- -----------------------------------------------------------------------------
-- Parsers.

pTcpSocketList :: Parser [TcpSocket]
pTcpSocketList = pHeaders *> Atto.many' pTcpSocket

-- Parse a single pSpace. The net/tcp file does not use tabs. Attoparsec's pSpace
-- includes tab, newline and return feed which captures too much in our case.
pSpace :: Parser Char
pSpace = Atto.char ' '

pMany1Space :: Parser ()
pMany1Space = void $ Atto.many1 pSpace

pStringSpace :: ByteString -> Parser ()
pStringSpace s =
  Atto.string s *> pMany1Space

pHeaders :: Parser ()
pHeaders =
  pMany1Space
    *> pStringSpace "sl"
    *> pStringSpace "local_address"
    *> pStringSpace "rem_address"
    *> pStringSpace "st"
    *> pStringSpace "tx_queue"
    *> pStringSpace "rx_queue"
    *> pStringSpace "tr"
    *> pStringSpace "tm->when"
    *> pStringSpace "retrnsmt"
    *> pStringSpace "uid"
    *> pStringSpace "timeout inode"
    <* Atto.endOfLine

pTcpSocket :: Parser TcpSocket
pTcpSocket = do
  _          <- pMany1Space
  _          <- (Atto.many1 Atto.digit *> Atto.char ':') <* pMany1Space -- Parse kernel slot
  localAddr  <- pAddressPort <* pMany1Space
  remoteAddr <- pAddressPort <* pMany1Space
  tcpState   <- pTcpState <* pMany1Space
  _          <- pInternalData
  uid        <- Atto.decimal <* pMany1Space
  _          <- Atto.hexadecimal <* pMany1Space :: Parser Int -- internal kernel state
  inode      <- Atto.decimal <* pMany1Space :: Parser Int
  _          <- Atto.many1 (Atto.satisfy (/= '\n')) -- remaining internal state
  _          <- Atto.endOfLine
  pure $ TcpSocket localAddr remoteAddr tcpState uid inode

pInternalData :: Parser ()
pInternalData = do
  _ <- Atto.hexadecimal :: Parser Int -- outgoing data queue
  _ <- Atto.char ':'
  _ <- Atto.hexadecimal :: Parser Int -- incoming data queue
  _ <- Atto.many1 pSpace
  _ <- Atto.hexadecimal :: Parser Int -- internal kernel state
  _ <- Atto.char ':'
  _ <- Atto.hexadecimal :: Parser Int -- internal kernel state
  _ <- Atto.many1 pSpace
  _ <- Atto.hexadecimal :: Parser Int -- internal kernel state
  _ <- Atto.many1 pSpace
  pure ()

-- The address parts of the `net/tcp` file is a hexadecimal representation of the IP
-- address and the port. The octets of the IP address have been reversed: 127.0.0.1
-- has been reversed to 1.0.0.127 and then rendered as hex numbers. The port is only
-- rendered as a hex number; it's not been reversed.
pAddressPort :: Parser (ByteString, Int)
pAddressPort = do
  addrParts <- replicateM 4 $ pHexadecimalOfLength 2
  _         <- Atto.char ':'
  port      <- pHexadecimalOfLength 4
  let addr' =
        BS.concat . List.intersperse "." . fmap (BS.pack . show) $ reverse addrParts
  pure (addr', port)

-- See include/net/tcp_states.h of your kernel's source code for all possible states.
pTcpState :: Parser TcpState
pTcpState =
    lookupState <$> (Atto.char '0' *> Atto.anyChar)
  where
    lookupState :: Char -> TcpState
    lookupState '1' = TcpEstablished
    lookupState '2' = TcpSynSent
    lookupState '3' = TcpSynReceive
    lookupState '4' = TcpFinWait1
    lookupState '5' = TcpFinWait2
    lookupState '6' = TcpTimeWait
    lookupState '7' = TcpClose
    lookupState '8' = TcpCloseWait
    lookupState '9' = TcpLastAck
    lookupState 'A' = TcpListen
    lookupState 'B' = TcpClosing
    lookupState 'C' = TcpNewSynReceive
    lookupState c = error $ "System.Linux.Proc.Tcp.pTcpState: " ++ show c

-- Helper parser for hexadecimal strings of a known length. Attoparsec's hexadecimal
-- will keep parsing digits to cover cases like '1', 'AB2', 'deadbeef', etc. In our
-- case we need to parse cases of exact length like port numbers.
pHexadecimalOfLength :: Int -> Parser Int
pHexadecimalOfLength n = do
  ds <- Atto.count n (Atto.satisfy (isHexDigit . fromEnum))
  return $ foldl step 0 (fmap (fromEnum :: Char -> Int) ds)
 where
  isHexDigit :: Int -> Bool
  isHexDigit w =
    (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
  step :: Int -> Int -> Int
  step a w | w >= 48 && w <= 57 = (a `shiftL` 4) .|. (w - 48)
           | w >= 97            = (a `shiftL` 4) .|. (w - 87)
           | otherwise          = (a `shiftL` 4) .|. (w - 55)
