{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE RecordWildCards    #-}

#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#else
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception
import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Maybe
import Data.Typeable
import System.Environment

import Criterion.Main
import GHC.Generics

import "bencode"   Data.BEncode     as A
import "bencoding" Data.BEncode     as C
import "bencoding" Data.BEncode.Internal as C
import "bencoding" Data.BEncode.Types    as C

#ifdef BENCHMARK_ATTOBENCODE
import Data.Attoparsec.ByteString as Atto
import Data.AttoBencode as B
import Data.AttoBencode.Parser as B
#endif

instance NFData A.BEncode where
    rnf (A.BInt    i) = rnf i
    rnf (A.BString s) = rnf s
    rnf (A.BList   l) = rnf l
    rnf (A.BDict   m) = rnf m

#ifdef BENCHMARK_ATTOBENCODE
instance NFData B.BValue where
    rnf (B.BInt    i) = rnf i
    rnf (B.BString s) = rnf s
    rnf (B.BList   l) = rnf l
    rnf (B.BDict   d) = rnf d
#endif

getRight :: Either String a -> a
getRight = either error id

data List a = Cons a (List a) | Nil
              deriving Generic

instance C.BEncode a => C.BEncode (List a)

instance NFData a => NFData (List a) where
  rnf  Nil        = ()
  rnf (Cons x xs) = rnf (x, xs)

replicate' :: Int -> a -> List a
replicate' c x
    |   c >= 0  = go c
    | otherwise = Nil
  where
    go 0 = Nil
    go n = Cons x $ go (pred n)

{-----------------------------------------------------------------------
--  Big dicts
-----------------------------------------------------------------------}

data Torrent = Torrent {
    tAnnounce     :: !ByteString
  , tAnnounceList :: !(Maybe ByteString)
  , tComment      :: !(Maybe ByteString)
  , tCreatedBy    :: !(Maybe ByteString)
  , tCreationDate :: !(Maybe ByteString)
  , tEncoding     :: !(Maybe ByteString)
  , tInfo         :: !BDict
  , tPublisher    :: !(Maybe ByteString)
  , tPublisherURL :: !(Maybe ByteString)
  , tSignature    :: !(Maybe ByteString)
  } deriving (Show, Eq, Typeable)

instance NFData Torrent where
  rnf Torrent {} = ()

instance C.BEncode Torrent where
  toBEncode Torrent {..} = toDict $
         "announce"      .=! tAnnounce
    C..: "announce-list" .=? tAnnounceList
    C..: "comment"       .=? tComment
    C..: "created by"    .=? tCreatedBy
    C..: "creation date" .=? tCreationDate
    C..: "encoding"      .=? tEncoding
    C..: "info"          .=!  tInfo
    C..: "publisher"     .=? tPublisher
    C..: "publisher-url" .=? tPublisherURL
    C..: "signature"     .=? tSignature
    C..: endDict



  fromBEncode = fromDict $ do
    Torrent <$>! "announce"
            <*>? "announce-list"
            <*>? "comment"
            <*>? "created by"
            <*>? "creation date"
            <*>? "encoding"
            <*>! "info"
            <*>? "publisher"
            <*>? "publisher-url"
            <*>? "signature"

{-----------------------------------------------------------------------
--  Main
-----------------------------------------------------------------------}

main :: IO ()
main = do
  (path : args) <- getArgs
  torrentFile   <- BS.readFile path
  let lazyTorrentFile = BL.fromChunks [torrentFile]

  evaluate $ rnf (torrentFile, lazyTorrentFile)

  withArgs args $
       defaultMain
       [ bench "decode/bencode"     $
           nf A.bRead                            lazyTorrentFile
#ifdef BENCHMARK_ATTOBENCODE
       , bench "decode/AttoBencode" $
           nf (getRight . Atto.parseOnly bValue) torrentFile
#endif
       , bench "decode/bencoding"   $
           nf (getRight . C.parse)              torrentFile

       , let Just v = A.bRead lazyTorrentFile in
         bench "encode/bencode"     $ nf A.bPack v
#ifdef BENCHMARK_ATTOBENCODE
       , let Right v = Atto.parseOnly bValue torrentFile in
         bench "encode/AttoBencode" $ nf B.encode v
#endif
       , let Right v = C.parse torrentFile in
         bench "encode/bencoding"   $ nf C.build v

       , bench "decode+encode/bencode"     $
           nf (A.bPack  . fromJust . A.bRead) lazyTorrentFile
#ifdef BENCHMARK_ATTOBENCODE
       , bench "decode+encode/AttoBencode" $
           nf (B.encode . getRight . Atto.parseOnly bValue) torrentFile
#endif
       , bench "decode+encode/bencoding"   $
           nf (C.build . getRight . C.parse) torrentFile

       , bench "list10000int/bencode/encode" $
           nf (A.bPack . A.BList . L.map (A.BInt . fromIntegral))
              [0..10000 :: Int]

#ifdef BENCHMARK_ATTOBENCODE
       , bench "list10000int/attobencode/encode" $
           nf B.encode [1..20000 :: Int]
#endif
       , bench "list10000int/bencoding/encode" $
           nf C.encode [1..20000 :: Int]


       , let d = A.bPack $ A.BList $
                 L.map A.BInt (L.replicate 1000 (0 :: Integer))
         in d `seq` (bench "list1000int/bencode/decode" $ nf
            (fromJust . A.bRead :: BL.ByteString -> A.BEncode) d)

       , let d = BL.toStrict (C.encode (L.replicate 10000 ()))
         in d `seq` (bench "list10000unit/bencoding/decode" $ nf
            (C.decode :: BS.ByteString -> Either String [()]) d)

       , let d = BL.toStrict $ C.encode $ L.replicate 10000 (0 :: Int)
         in d `seq` (bench "list10000int/bencoding/decode" $ nf
            (C.decode :: BS.ByteString -> Either String [Int]) d)

       , let d = L.replicate 10000 0
         in bench "list10000int/bencoding/encode>>decode" $
            nf  (getRight . C.decode . BL.toStrict . C.encode
                 :: [Int] ->  [Int] )
                d

       , let d = replicate' 10000 0
         in bench "list10000int/bencoding/encode>>decode/generic" $
            nf (getRight . C.decode . BL.toStrict . C.encode
                :: List Int -> List Int)
               d

       , let Right !be = C.parse torrentFile
             id'   x  = let t = either error id (fromBEncode x)
                        in toBEncode (t :: Torrent)
             !_unused@True =
                     let Right t = C.decode torrentFile
                     in if C.decode (BL.toStrict (C.encode t))
                           /= Right (t :: Torrent)
                        then error "invalid instance: BEncode Torrent"
                        else True

             replFn m f = go m
               where go 0 = id
                     go n = f . go (pred n)

         in bench "bigdict" $ nf (replFn (1000 :: Int) id') be

       , let fn x = let Right t = C.decode x in t :: Torrent
         in bench "torrent/decode" $ nf fn torrentFile
       ]
