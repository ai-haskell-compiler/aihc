{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module System.IO.Streams.Network.Internal.Address
  ( getSockAddr
  , getSockAddrImpl
  , AddressNotSupportedException(..)
  ) where

------------------------------------------------------------------------------
import           Control.Exception     (Exception, throwIO)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.Typeable         (Typeable)
import           Network.Socket        (AddrInfo (addrAddress, addrFamily, addrFlags), AddrInfoFlag (AI_NUMERICSERV), Family, SockAddr, defaultHints, getAddrInfo)


------------------------------------------------------------------------------
data AddressNotSupportedException = AddressNotSupportedException String
   deriving (Typeable)

instance Show AddressNotSupportedException where
    show (AddressNotSupportedException x) = "Address not supported: " ++ x

instance Exception AddressNotSupportedException

------------------------------------------------------------------------------
getSockAddr :: Int
            -> ByteString
            -> IO (Family, SockAddr)
getSockAddr = getSockAddrImpl getAddrInfo


------------------------------------------------------------------------------
getSockAddrImpl
  :: (Maybe AddrInfo -> Maybe String -> Maybe String -> IO [AddrInfo])
     -> Int -> ByteString -> IO (Family, SockAddr)
getSockAddrImpl !_getAddrInfo p s = do
    ais <- _getAddrInfo (Just hints) (Just $ S.unpack s)
                        (Just $ show p)
    if null ais
      then throwIO $ AddressNotSupportedException $ show s
      else do
        let !ai = head ais
        let !fm = addrFamily ai
        let !sa = addrAddress ai
        return (fm, sa)
  where
    hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
