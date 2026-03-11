{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.NineP
-- Copyright   : Tim Newsham
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : David Leimbach <leimy2k@gmail.com>
-- Stability   : experimental
-- Portability : Only tested on GHC 6.12.1, uses TypeSynonymInstances
--
-- Module providing Binary serialization of 9P messages to and from lazy 
-- ByteStrings.
-- 
-- This library does not currently provide any networking support or 
-- wrappers for easy to write clients or servers, though that may come 
-- with time as we decide the best way to implement these.
-- 
-- 9P2000 messages are sent in little endian byte order rather than network byte order 
-- (big endian)
--
-- Lightly tested against an Inferno operating
-- system share with no authentication successfully.
-----------------------------------------------------------------------------

module Data.NineP ( 
                  -- * Bin - a little endian encode/decode class for Binary
                   Bin(..)
                  -- * Qid - Server side data type for path tracking (<http://9p.cat-v.org> for details)
                  , Qid(..)

                  -- * Stat - Namespace metadata /(somewhat like a unix fstat)/
                  , Stat(..)

                  -- * Msg - envelope for 9P2000 messages
                  , Msg(..)
                       
                  -- ** Tag - A message payload type
                  , Tag(..)

                  -- ** VarMsg - A data type encapsulating the various 9P messages 
                  , VarMsg(..)
                          
                  -- ** putVarMsg - function that can encode all VarMsg types to a lazy ByteString
                  , putVarMsg

                  -- ** getVarMsg - function to decode all VarMsg types from a lazy ByteString
                  , getVarMsg

                  -- * Example
                  -- $example
                  ) where
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.Word
import qualified Data.ByteString.Lazy as L

{-
import Debug.Trace 
tr msg n = trace (msg ++ show n) n
-- -}

-- Little-endian binary class
class Bin a where
    get :: Get a
    put :: a -> Put

instance Bin Word8 where
    get = getWord8
    put = putWord8

instance Bin Word16 where
    get = getWord16le
    put = putWord16le

instance Bin Word32 where
    get = getWord32le
    put = putWord32le

instance Bin Word64 where
    get = getWord64le
    put = putWord64le

instance Bin Char where
    get = chr . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . ord

instance Bin String where
    get = getWord16le >>= \n -> replicateM (fromIntegral n) get
    put xs = putWord16le (fromIntegral $ length xs) >> mapM_ put xs

-- | A Plan 9 Qid type.  See http://9p.cat-v.org for more information
data Qid = Qid {
    qid_typ :: Word8,
    qid_vers :: Word32,
    qid_path :: Word64 } deriving (Show, Eq)

-- Instnace of Bin for Qid.
instance Bin Qid where
    get = Qid <$> get <*> get <*> get
    put (Qid t v p) = put t >> put v >> put p

-- | Takes a fixed size lazy ByteString @sz@, running @g@ on it, and returning the result if all the input is consumed, uses @Prelude.error@ otherwise. (Throws an exception)
getNest :: Integral n => n -> Get a -> Get a
getNest sz g = do
    b <- getLazyByteString (fromIntegral sz)
    return $ flip runGet b $ do
        x <- g
        e <- isEmpty
        if e
          then return x
          else do
              n <- remaining
              error $ show n ++ " extra bytes in nested structure"

-- | Provides information on a path entry at a 9P2000 server
data Stat = Stat {
    st_typ :: Word16,
    st_dev :: Word32,
    st_qid :: Qid,
    st_mode :: Word32,
    st_atime :: Word32,
    st_mtime :: Word32,
    st_length :: Word64,
    st_name :: String,
    st_uid :: String,
    st_gid :: String,
    st_muid :: String } deriving (Show, Eq)

instance Bin Stat where
    get = do
        n <- getWord16le
        getNest n g
      where g = Stat <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
    put (Stat a b c d e f g h i j k) = do
        let buf = runPut p
        putWord16le $ fromIntegral $ L.length buf
        putLazyByteString buf
      where p  = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j >> put k

-- | A variable message type that encapsulates the valid kinds of messages in a 9P2000 payload
data VarMsg = 
    Tversion {
        tv_msize :: Word32,
        tv_version :: String }
    | Rversion {
        rv_msize :: Word32,
        rv_version :: String }
    | Tauth {
        tau_afid :: Word32,
        tau_uname :: String,
        tau_aname :: String }
    | Rauth { ra_aqid :: Qid }
    | Rerror { re_ename :: String }
    | Tflush { tf_oldtag :: Word16 }
    | Rflush 
    | Tattach {
        tat_fid :: Word32,
        tat_afid :: Word32,
        tat_uname :: String,
        tat_aname :: String }
    | Rattach { rat_qid :: Qid }
    | Twalk {
        tw_fid :: Word32,
        tw_newfid :: Word32,
        tw_wnames :: [String] }
    | Rwalk { rw_wqid :: [Qid] }
    | Topen {
        to_fid :: Word32,
        to_mode :: Word8 }
    | Ropen {
        ro_qid :: Qid,
        ro_iounit :: Word32 }
    | Tcreate { 
        tcr_fid :: Word32,
        tcr_name :: String,
        tcr_perm :: Word32,
        tcr_mode :: Word8 }
    | Rcreate {
        rcr_qid :: Qid,
        rcr_iounit :: Word32 }
    | Tread {
        trd_fid :: Word32,
        trd_offset :: Word64,
        trd_count :: Word32 }
    | Rread { rrd_dat :: L.ByteString }
    | Twrite {
        twr_fid :: Word32,
        twr_offset :: Word64,
        twr_dat :: L.ByteString }
    | Rwrite { rw_count :: Word32 }
    | Tclunk { tcl_fid :: Word32 }
    | Rclunk
    | Tremove { trm_fid :: Word32 }
    | Rremove
    | Tstat { ts_fid :: Word32 }
    | Rstat { rs_stat :: [Stat] }
    | Twstat {
        tws_fid :: Word32,
        tws_stat :: [Stat] }
    | Rwstat
    deriving (Show, Eq)


-- | A type that enumerates all the valid /(and one invalid)/ message types in 9P2000
data Tag = TTversion | TRversion | TTauth | TRauth | TTattach | TRattach
    | XXX_TTerror | TRerror | TTflush | TRflush 
    | TTwalk | TRwalk | TTopen | TRopen 
    | TTcreate | TRcreate | TTread | TRread | TTwrite | TRwrite
    | TTclunk | TRclunk | TTremove | TRremove | TTstat | TRstat 
    | TTwstat | TRwstat
    deriving (Show, Eq, Ord, Enum)

instance Bin Tag where
    get = do
        n <- getWord8
        return $ if n >= 100 && n < 128 && n /= 106 -- 106 == _tTerror
                    then toEnum $ fromEnum (n-100)
                    else error $ "invalid tag: " ++ (show n)
    put = putWord8 . toEnum . (+ 100) . fromEnum

-- | A monadic action in Get that returns all parseable entries as a list, returning [] if none exist
getListAll :: (Bin a) => Get [a]
getListAll = do
    e <- isEmpty 
    if e 
      then return [] 
      else (:) <$> get <*> getListAll

-- | A monadic action in Put that maps all encodable items into the Put monad for serialization
putListAll :: (Bin a) => [a] -> Put
putListAll = mapM_ put

-- | Like 'getNest' but is preceded by a little endian 16bit word.  Useful for retrieving 16bit payload lengths from 9P2000 messages that support it.
getNestList16 :: (Bin a) => Get [a]
getNestList16 = do
    n <- getWord16le
    getNest n getListAll

-- | Runs 'putListAll' over @xs@ followed by computing the 16bit (little endian) length of the list, and prepending it to the final payload.  Useful for automatically computing lengths for 9P2000 messages that require it
putNestList16 :: Bin a => [a] -> Put
putNestList16 xs = do
    let buf = runPut (putListAll xs)
    putWord16le $ fromIntegral $ L.length buf
    putLazyByteString buf

-- | Gets a 16bit value from the stream, then executes @Data.Binary.Get.get@ that many times to produce a list of parsed values.
getList16 :: Bin a => Get [a]
getList16 = getWord16le >>= \n -> replicateM (fromIntegral n) get

-- | Puts @xs@ into the Put stream prepended by its length.
putList16 :: Bin a => [a] -> Put
putList16 xs = putWord16le (fromIntegral $ length xs) >> mapM_ put xs

-- | Gets a 32bit little endian legnth from the stream, then gets a lazy ByteString of that size from the stream, returning only the ByteString
getBytes32 :: Get L.ByteString
getBytes32 = getWord32le >>= getLazyByteString . fromIntegral

-- | Takes length of @xs@ and then prepends that to the lazy ByteString it places in the Stream
putBytes32 :: L.ByteString -> Put
putBytes32 xs = putWord32le (fromIntegral $ L.length xs) >> putLazyByteString xs

-- | Convertss VarMsg types to Tag values
getTag :: VarMsg -> Tag
getTag (Tversion _ _) = TTversion
getTag (Rversion _ _) = TRversion
getTag (Tauth _ _ _) = TTauth
getTag (Rauth _) = TRauth
getTag (Tflush _) = TTflush
getTag (Rflush) = TRflush
getTag (Tattach _ _ _ _) = TTattach
getTag (Rattach _) = TRattach
getTag (Rerror _) = TRerror
getTag (Twalk _ _ _) = TTwalk
getTag (Rwalk _) = TRwalk
getTag (Topen _ _) = TTopen
getTag (Ropen _ _) = TRopen
getTag (Tcreate _ _ _ _) = TTcreate
getTag (Rcreate _ _) = TRcreate
getTag (Tread _ _ _) = TTread
getTag (Rread _) = TRread
getTag (Twrite _ _ _) = TTwrite
getTag (Rwrite _) = TRwrite
getTag (Tclunk _) = TTclunk
getTag (Rclunk) = TRclunk
getTag (Tremove _) = TTremove
getTag (Rremove) = TRremove
getTag (Tstat _) = TTstat
getTag (Rstat _) = TRstat
getTag (Twstat _ _) = TTwstat
getTag (Rwstat) = TRwstat

-- | For every messages type, runs a Get parser to decode that type of payload from the 9P2000 stream
getVarMsg :: Tag -> Get VarMsg
getVarMsg TTversion = Tversion <$> get <*> get
getVarMsg TRversion = Rversion <$> get <*> get
getVarMsg TTauth = Tauth <$> get <*> get <*> get
getVarMsg TRauth = Rauth <$> get
getVarMsg XXX_TTerror = error "there is no Terror"
getVarMsg TRerror = Rerror <$> get
getVarMsg TTflush = Tflush <$> get
getVarMsg TRflush = return Rflush
getVarMsg TTattach = Tattach <$> get <*> get <*> get <*> get
getVarMsg TRattach = Rattach <$> get
getVarMsg TTwalk = Twalk <$> get <*> get <*> getList16
getVarMsg TRwalk = Rwalk <$> getList16
getVarMsg TTopen = Topen <$> get <*> get
getVarMsg TRopen = Ropen <$> get <*> get
getVarMsg TTcreate = Tcreate <$> get <*> get <*> get <*> get
getVarMsg TRcreate = Rcreate <$> get <*> get
getVarMsg TTread = Tread <$> get <*> get <*> get
getVarMsg TRread = Rread <$> getBytes32
getVarMsg TTwrite = Twrite <$> get <*> get <*> getBytes32
getVarMsg TRwrite = Rwrite <$> get
getVarMsg TTclunk = Tclunk <$> get
getVarMsg TRclunk = return Rclunk
getVarMsg TTremove = Tremove <$> get
getVarMsg TRremove = return Rremove
getVarMsg TTstat = Tstat <$> get
getVarMsg TRstat = Rstat <$> getNestList16
getVarMsg TTwstat = Twstat <$> get <*> getNestList16
getVarMsg TRwstat = return Rwstat

-- | For every lower level VarMsg type, encodes a full wrapper around that type for use with 9P2000 streams
putVarMsg :: VarMsg -> Put
putVarMsg (Tversion a b) = put a >> put b
putVarMsg (Rversion a b) = put a >> put b
putVarMsg (Tauth a b c) = put a >> put b >> put c
putVarMsg (Rauth a) = put a
putVarMsg (Rerror a) = put a
putVarMsg (Tflush a) = put a
putVarMsg (Rflush) = return ()
putVarMsg (Tattach a b c d) = put a >> put b >> put c >> put d
putVarMsg (Rattach a) = put a
putVarMsg (Twalk a b c) = put a >> put b >> putList16 c
putVarMsg (Rwalk a) = putList16 a
putVarMsg (Topen a b) = put a >> put b
putVarMsg (Ropen a b) = put a >> put b
putVarMsg (Tcreate a b c d) = put a >> put b >> put c >> put d
putVarMsg (Rcreate a b) = put a >> put b
putVarMsg (Tread a b c) = put a >> put b >> put c
putVarMsg (Rread a) = putBytes32 a
putVarMsg (Twrite a b c) = put a >> put b >> putBytes32 c
putVarMsg (Rwrite a) = put a
putVarMsg (Tclunk a) = put a
putVarMsg (Rclunk) = return ()
putVarMsg (Tremove a) = put a
putVarMsg (Rremove) = return ()
putVarMsg (Tstat a) = put a
putVarMsg (Rstat a) = putNestList16 a
putVarMsg (Twstat a b) = put a >> putNestList16 b
putVarMsg (Rwstat) = return ()

-- | The message envelope type for all 9P2000 messages
data Msg = Msg {
    msg_typ :: Tag,
    msg_tag :: Word16,
    msg_body :: VarMsg } deriving(Show, Eq)

-- | An arbitrarily chosen maxiumum size for any Msg
maxSize :: Word32
maxSize = 1024 * 1024 -- XXX arbitrary, configured?

instance Bin Msg where
    get = do
        sz <- getWord32le
        if sz < 4 || sz > maxSize
          then return $ error $ "Invalid size: " ++ show sz
          else getNest (sz - 4) $ do
              typ <- get
              tag <- getWord16le
              body <- getVarMsg typ
              return $ Msg typ tag body
    put (Msg _ tag body) = do
        let typ = getTag body
            buf = runPut (put typ >> put tag >> putVarMsg body)
        putWord32le $ fromIntegral $ L.length buf + 4
        putLazyByteString buf


-- --------------------------------------------------------------------
-- $example
--
-- Exchanging initial version data with any 9P2000 server
--
-- > module Main where
--
-- > import Data.Maybe
-- > import Control.Monad
-- > import qualified Data.ByteString.Lazy.Char8 as C
-- > import Network.Socket hiding (send, recv)
-- > import Network.Socket.ByteString.Lazy
-- > import Data.Int
-- > import Data.Binary.Get
-- > import Data.Binary.Put
-- > import Debug.Trace
-- > import Data.NineP
-- > 
-- > connector :: IO Socket 
-- > connector = withSocketsDo $
-- >             do
-- >               ainfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6872")
-- >               let a = head ainfo
-- >               sock <- socket AF_INET Stream defaultProtocol
--
-- At this point we've just created our socket to a machine on 127.0.0.1:6872 
-- where we'd expect to see a 9P2000 server.
--
-- >               putStrLn "Trying to connect"
-- >               connect sock (addrAddress (traceShow a a))
-- >               putStrLn "connected!"
--
-- The socket is connected at this point, build up a TVersion message, asking
-- to speak to the server with the 9P2000 protocol. 
--
-- The 1024 tells the server the maximum message size we'd like to support.
--
-- >               let version = Msg TTversion (-1) $ Tversion 1024 "9P2000"
-- >               putStrLn $ "About to send: " ++ show version
--
-- We now need to pack the message into a bytestring.  This is handled by the
-- Bin class instance /Msg/, and the serialization is handled by runPut.
-- We send this data to the socket.
--
-- >               send sock $ runPut (put version) 
-- >               putStrLn "Getting response"
--
-- Now wait for a response from the server, evaluated runGet over it to 
-- de-serialize it, and show it.
--
-- >               msg <- recv sock 50
-- >               let response = runGet get msg ::Msg
-- >               putStrLn $ show response
-- >               return sock