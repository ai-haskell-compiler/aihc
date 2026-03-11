-- | BitTorrent metainfo files
--
-- <http://www.bittorrent.org/beps/bep_0003.html>

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Torrent
	( Torrent(..)
	, TorrentInfo(..)
	, TorrentFile(..)
	, readTorrent
	, serializeTorrent
	, torrentSize
	, showTorrent
	) where

import Data.BEncode
import Data.BEncode.Reader
import Data.Binary
import Data.Generics
import Data.Maybe
import Control.Applicative

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.Map as Map

data Torrent
	= Torrent
		{ tAnnounce     :: Maybe ByteString
		, tAnnounceList :: [ByteString]
		, tComment      :: ByteString
		, tCreatedBy    :: Maybe ByteString
		, tInfo         :: TorrentInfo
		}
	deriving (Show, Read, Typeable, Data)

data TorrentInfo
	= SingleFile
		{ tLength      :: Integer
		, tName        :: ByteString
		, tPieceLength :: Integer
		, tPieces      :: ByteString }
	| MultiFile
		{ tFiles       :: [TorrentFile]
		, tName        :: ByteString
		, tPieceLength :: Integer
		, tPieces      :: ByteString
		}
	deriving (Show, Read, Typeable, Data)

data TorrentFile
	= TorrentFile
		{ fileLength :: Integer
		, filePath   :: [ByteString]
		}
	deriving (Show, Read, Typeable, Data)

instance Binary Torrent where
	put = put . serializeTorrent
	get = do
		e <- get
		case readTorrent e of
			Left err -> fail $ "Failed to parse torrent: " ++ err
			Right t  -> return t

-- | Size of the files in the torrent.
torrentSize :: Torrent -> Integer
torrentSize torrent = case tInfo torrent of
	s@SingleFile{} -> tLength s
	MultiFile{tFiles=files} -> sum (map fileLength files)

readTorrent :: ByteString -> Either String Torrent
readTorrent inp = case bRead inp of
	Nothing -> Left "Not BEncoded"
	Just be -> runBReader parseTorrent be

parseTorrent :: BReader Torrent
parseTorrent = do
	announce <- optional $ dict "announce" bbytestring
	creator <- optional $ dict "created by" bbytestring
	dict "info" $ do
		name <- dict "name" bbytestring
		pLen <- dict "piece length" bint
		pieces <- dict "pieces" bbytestring
		torrentInfo <- parseTorrentInfo name pLen pieces
		return $ Torrent announce [] BS.empty creator torrentInfo

parseTorrentInfo :: ByteString -> Integer -> ByteString -> BReader TorrentInfo
parseTorrentInfo name pLen pieces = single <|> multi
  where
	single = do
		len <- dict "length" bint
		return $ SingleFile len name pLen pieces
	multi = do
		files <- dict "files" $ list $ do
			len <- dict "length" bint
			filePaths <- dict "path" (list bbytestring)
			return $ TorrentFile len filePaths
		return $ MultiFile files name pLen pieces

serializeTorrent :: Torrent -> BEncode
serializeTorrent torrent = BDict $ Map.fromList $ catMaybes
	[ fmap (\b -> ("announce", BString b)) (tAnnounce torrent)
	, Just ("comment", BString $ tComment torrent)
	, Just ("info", info)
	]
  where
	info = BDict $ Map.fromList $
		[ ("name", BString $ tName (tInfo torrent))
		, ("pieces", BString $ tPieces (tInfo torrent))
		, ("piece length", BInt $ tPieceLength (tInfo torrent))
		] ++ case tInfo torrent of
			SingleFile len _ _ _ ->
				[ ("length", BInt len) ]
			MultiFile files _ _ _ ->
				[ ("files", BList $ map serfile files) ]

	serfile file = BDict $ Map.fromList
		[ ("length", BInt (fileLength file))
		, ("path", BList (map BString $ filePath file))
		]

-- | generates a torrent file
--
-- Due to lexographical ordering requirements of BEncoded data, this
-- should generate the same ByteString that readTorrent read to generate
-- the Torrent. However, torrent files may contain extensions and
-- nonstandard fields that prevent that from holding for all torrent files.
showTorrent :: Torrent -> ByteString
showTorrent = bPack . serializeTorrent
