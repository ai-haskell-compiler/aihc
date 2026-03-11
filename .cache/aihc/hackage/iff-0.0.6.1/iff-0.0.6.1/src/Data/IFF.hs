{- |
ToDo:

Lazy read and write.

Problem on writing:
If the length of data is computed lazily,
then you must seek back to the file position
where the size is stored.
That is for writing of lazily generated data
we need a seekable file device.
-}
module Data.IFF
   (T(..), Chunk(..),
    ChunkId, chunkIdFromString, chunkIdToString,
    fromByteString, toByteString,
    ) where

import Control.Monad (guard, liftM, liftM2, liftM4, )
import Data.Maybe (fromMaybe)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Data.Word (Word32)

import qualified Data.Binary.Get as Get
import qualified Data.Binary as Bin
import Data.Binary.Get (Get, getByteString, getWord32be, getWord8, runGetOrFail)
import Data.Binary.Put (Put, putByteString, putWord32be, runPut)
import Data.Binary (Binary)

import Prelude hiding (getChar)

data T = Cons
   {chunkId :: ChunkId
   ,chunk   :: Chunk
   }

data ChunkId = ChunkId Char Char Char Char
   deriving Eq

data Chunk =
     Form  {parts :: [T]
           }
   | List  {props :: [T]
           ,parts :: [T]
           }
   | Cat   {parts :: [T]
           }
   | Prop  {parts :: [T]
           }
   | Chunk {content :: ByteString
           }

chunkIdFromString :: String -> ChunkId
chunkIdFromString (c0:c1:c2:c3:[]) = ChunkId c0 c1 c2 c3
chunkIdFromString _ = error "chunkId must have four characters"

chunkIdToString :: ChunkId -> String
chunkIdToString (ChunkId c0 c1 c2 c3) = c0:c1:c2:c3:[]


formId, listId, catId, propId :: ChunkId
formId = chunkIdFromString "FORM"
listId = chunkIdFromString "LIST"
catId  = chunkIdFromString "CAT "
propId = chunkIdFromString "PROP"


instance Show T where
  show (Cons { chunkId = name, chunk = chk }) =
    "chunk "++show name++" "++show chk

instance Show ChunkId where
  show cid = show (chunkIdToString cid)

instance Show Chunk where
  show (Form { parts = p }) =
    "IFF.Form {parts="++show p++"}"
  show (List { props = ps, parts = p }) =
    "IFF.List {props"++show ps++",parts="++show p++"}"
  show (Cat { parts = p }) =
    "IFF.Cat {parts="++show p++"}"
  show (Prop { parts = p }) =
    "IFF.Prop {parts="++show p++"}"
  show (Chunk { content = cont }) =
    "IFF.Chunk { binary content, size "++show (B.length cont)++" }"

instance Binary T where
  get = parser
  put = putT


isProp :: T -> Bool
isProp (Cons _ (Prop _)) = True
isProp _ = False

getChar :: Get Char
getChar = liftM (toEnum . fromIntegral) getWord8

getChunkId :: Get ChunkId
getChunkId = liftM4 ChunkId getChar getChar getChar getChar

fromByteString :: ByteString -> Maybe T
fromByteString = runParser parser

runParser :: Get a -> ByteString -> Maybe a
runParser parse bs = runParserLazy parse $ L.fromChunks [bs]

runParserLazy :: Get a -> L.ByteString -> Maybe a
runParserLazy parse bs =
   case runGetOrFail parse bs of
      Right (rest, _, x) -> guard (L.null rest) >> Just x
      Left  _            -> Nothing

parseMany :: Get [T]
parseMany =
   do mt <- Get.isEmpty
      if mt
        then return []
        else liftM2 (:) parser parseMany

parser :: Get T
parser =
   do cid        <- getChunkId
      size       <- fmap fromIntegral getWord32be
      rawContent <- getByteString size
      Get.skip $ mod (-size) 2 -- skip padding byte
      maybe (fail "parser: failed") return $
        fromMaybe (Just (Cons cid
                     (Chunk {content = rawContent}))) $
        lookup cid $
          (formId, runParseStruct Form rawContent) :
          (listId, runParseStruct (uncurry List . span isProp) rawContent) :
          (catId,  runParseStruct Cat  rawContent) :
          (propId, runParseStruct Prop rawContent) :
          []

runParseStruct :: ([T] -> Chunk) -> ByteString -> Maybe T
runParseStruct cons bs =
   do (format, subChunks) <- runParser parseStruct bs
      return (Cons format (cons subChunks))

parseStruct :: Get (ChunkId, [T])
parseStruct =
   liftM2 (,) getChunkId parseMany


packLengthBE :: ByteString -> Put
packLengthBE dat =
   putWord32be (fromIntegral (B.length dat) :: Word32)

s2b :: String -> ByteString
s2b = B8.pack

toByteString :: T -> ByteString
toByteString = B.concat . L.toChunks . runPut . putT

putT :: T -> Put
putT (Cons {chunkId = name, chunk = chk}) =
   let bid = s2b (chunkIdToString name)
   in  case chk of
          Form subChunks -> makeStructureChunk formId bid subChunks
          List ps
               subChunks -> makeStructureChunk listId bid (ps++subChunks)
          Cat  subChunks -> makeStructureChunk catId  bid subChunks
          Prop subChunks -> makeStructureChunk propId bid subChunks
          Chunk chunkData -> do
            putByteString bid
            packLengthBE chunkData
            putByteString $ padData chunkData

makeStructureChunk :: ChunkId -> ByteString -> [T] -> Put
makeStructureChunk name formatName chunks =
   let cont = B.concat (formatName : map toByteString chunks)
   in  do
          putByteString $ s2b $ chunkIdToString name
          packLengthBE cont
          putByteString cont

padData :: ByteString -> ByteString
padData str =
   if even (B.length str)
     then str
     else B.snoc str 0
