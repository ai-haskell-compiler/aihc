{-# LANGUAGE OverloadedStrings #-}
-- #hide

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Multipart
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
-- License     :  BSD-style
--
-- Maintainer  :  Anders Kaseorg <andersk@mit.edu>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsing of the multipart format from RFC2046.
-- Partly based on code from WASHMail.
--
-----------------------------------------------------------------------------
module Network.Multipart
    (
     -- * Multi-part messages
     MultiPart(..), BodyPart(..)
    , parseMultipartBody, hGetMultipartBody
    , showMultipartBody
     -- * Headers
    , Headers , HeaderName(..)
    , ContentType(..), ContentTransferEncoding(..)
    , ContentDisposition(..)
    , parseContentType
    , getContentType
    , getContentTransferEncoding
    , getContentDisposition
    ) where

import Control.Monad
import Data.List (intersperse)
import Data.Maybe
import System.IO (Handle)

import Network.Multipart.Header

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Search (breakOn)

--
-- * Multi-part stuff.
--

data MultiPart = MultiPart [BodyPart]
               deriving (Show, Eq, Ord)

data BodyPart = BodyPart Headers ByteString
                deriving (Show, Eq, Ord)

-- | Read a multi-part message from a 'ByteString'.
parseMultipartBody :: String -- ^ Boundary
                   -> ByteString -> MultiPart
parseMultipartBody b =
    MultiPart . mapMaybe parseBodyPart . splitParts (BS.pack b)

-- | Read a multi-part message from a 'Handle'.
--   Fails on parse errors.
hGetMultipartBody :: String -- ^ Boundary
                  -> Handle
                  -> IO MultiPart
hGetMultipartBody b = liftM (parseMultipartBody b) . BS.hGetContents

parseBodyPart :: ByteString -> Maybe BodyPart
parseBodyPart s = do
  let (hdr,bdy) = splitAtEmptyLine s
  hs <- parseM pHeaders "<input>" (BS.unpack hdr)
  return $ BodyPart hs bdy

showMultipartBody :: String -> MultiPart -> ByteString
showMultipartBody b (MultiPart bs) =
    unlinesCRLF $ foldr (\x xs -> d:showBodyPart x:xs) [c,BS.empty] bs
 where d = BS.pack ("--" ++ b)
       c = BS.pack ("--" ++ b ++ "--")

showBodyPart :: BodyPart -> ByteString
showBodyPart (BodyPart hs c) =
    unlinesCRLF $ [BS.pack (n++": "++v) | (HeaderName n,v) <- hs] ++ [BS.empty,c]

--
-- * Splitting into multipart parts.
--

-- | Split a multipart message into the multipart parts.
splitParts :: ByteString -- ^ The boundary, without the initial dashes
           -> ByteString
           -> [ByteString]
splitParts b = spl . dropPreamble b
  where
  spl x = case splitAtBoundary b x of
            Nothing -> []
            Just (s1,d,s2) | isClose b d -> [s1]
                           | otherwise -> s1:spl s2

-- | Drop everything up to and including the first line starting
--   with the boundary.
dropPreamble :: ByteString -- ^ The boundary, without the initial dashes
             -> ByteString
             -> ByteString
dropPreamble b s = case splitAtBoundary b s of
  Nothing -> BS.empty
  Just (_,_,v) -> v

-- | Split a string at the first boundary line.
splitAtBoundary :: ByteString -- ^ The boundary, without the initial dashes
                -> ByteString -- ^ String to split.
                -> Maybe (ByteString,ByteString,ByteString)
                   -- ^ The part before the boundary, the boundary line,
                   --   and the part after the boundary line. The CRLF
                   --   before and the CRLF (if any) after the boundary line
                   --   are not included in any of the strings returned.
                   --   Returns 'Nothing' if there is no boundary.
splitAtBoundary b s =
  let b' = BS.append "--" b
      bcrlf = BS.append "\r\n" b'

      -- check if we are at the beginning of a boundary, if so, we
      -- won’t have a \r\n
      prefix = if BS.isPrefixOf b' s then b'
               else bcrlf

      (before, t) = breakOn (BS.toStrict prefix) s
  in case BS.stripPrefix prefix t of
       Nothing -> Nothing
       Just t' ->
         let after = case BS.stripPrefix "\r\n" t' of
               Nothing -> t'
               Just t'' -> t''
         in  Just (before, prefix, after)

-- | Check whether a string for which 'isBoundary' returns true
--   has two dashes after the boudary string.
isClose :: ByteString -- ^ The boundary, without the initial dashes
        -> ByteString
        -> Bool
isClose b s = BS.isPrefixOf (BS.append "--" (BS.append b "--")) s

--
-- * RFC 2046 CRLF
--

crlf :: ByteString
crlf = BS.pack "\r\n"

unlinesCRLF :: [ByteString] -> ByteString
unlinesCRLF = BS.concat . intersperse crlf

-- | Split a string at the first empty line. The CRLF (if any) before the
--   empty line is included in the first result. The CRLF after the
--   empty line is not included in the result.
--   If there is no empty line, the entire input is returned
--   as the first result.
splitAtEmptyLine :: ByteString -> (ByteString, ByteString)
splitAtEmptyLine s =
  let blank = "\r\n\r\n"
      (before, after) = breakOn (BS.toStrict blank) s
  in case BS.stripPrefix blank after of
       Nothing -> (before, after)
       Just after' -> (BS.append before "\r\n", after')
