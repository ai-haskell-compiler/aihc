module Sound.Audacity.Project.Track.Wave where

import qualified Sound.Audacity.Project.Track.Wave.Summary as Summary

import qualified Sound.Audacity.XML.Attribute as Attr
import qualified Sound.Audacity.XML as XML

import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.XML.Basic.Name.MixedCase as Name

import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)

import qualified System.IO as IO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.StorableVector.Lazy as SVL

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.NonEmpty ((!:))
import Data.Tuple.HT (mapSnd)

import qualified Control.Monad.Trans.Reader as MR
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (liftM)


import Prelude hiding (sequence_)


data T =
   Cons {
      name_ :: String,
      channel_ :: Channel,
      linked_, mute_, solo_, minimized_ :: Bool,
      height_ :: Int,
      rate_ :: Int,
      gain_, pan_ :: Double,
      clips_ :: [Clip]
   }
   deriving Show

deflt :: T
deflt =
   Cons {
      name_ = "",
      channel_ = Mono,
      linked_ = False, mute_ = False, solo_ = False,
      minimized_ = False,
      height_ = 150,
      rate_ = 44100,
      gain_ = 1.0,
      pan_ = 0.0,
      clips_ = []
   }

data Channel = Left | Right | Mono
   deriving (Eq, Ord, Enum, Show)


data Clip =
   Clip {
      offset_ :: Double,
      sequence_ :: Sequence
      -- envelope_ :: Envelope
   }
   deriving Show


data Sequence =
   Sequence {
      maxSamples_ :: Int,
      -- the sampleformat attribute seems to have no effect
      sampleFormat_ :: SampleFormat,
      numSamples_ :: Int,
      blocks_ :: [Block]
   }
   deriving Show

-- cf. audacity:SampleFormat.h
data SampleFormat = Int16Sample | Int24Sample | FloatSample
   deriving (Eq, Ord, Enum, Bounded, Show)

intFromSampleFormat :: SampleFormat -> Int
intFromSampleFormat fmt =
   case fmt of
      Int16Sample -> 0x00020001
      Int24Sample -> 0x00040001
      FloatSample -> 0x0004000F


data Block =
   Block {
      blockStart_, blockLength_ :: Int,
      blockFile_ :: BlockFile
   }
   deriving Show

data BlockFile =
   PCMAliasBlockFile {
      summaryFile_, aliasFile_ :: FilePath,
      aliasStart_ :: Int,
      -- aliasLength_ :: Int,   replaced by blockLength
      aliasChannel_ :: Int,
      limits_ :: Summary.Limits
   }
   deriving Show


toXML :: T -> [[Tag.T Name.T String]]
toXML x =
   XML.tag "wavetrack" x
      (Attr.string "name" name_ :
       Attr.enum "channel" channel_ :
       Attr.bool "linked" linked_ :
       Attr.bool "mute" mute_ :
       Attr.bool "solo" solo_ :
       Attr.int "height" height_ :
       Attr.bool "minimized" minimized_ :
       Attr.int "rate" rate_ :
       Attr.double "gain" gain_ :
       Attr.double "pan" pan_ :
       [])
      (concatMap clipToXML (clips_ x))

clipToXML :: Clip -> [[Tag.T Name.T String]]
clipToXML x =
   XML.tag "waveclip" x
      (Attr.string "offset" (printf "%.8f" . offset_) :
       [])
      (sequenceToXML (sequence_ x))

sequenceToXML :: Sequence -> [[Tag.T Name.T String]]
sequenceToXML x =
   XML.tag "sequence" x
      (Attr.int "maxsamples" maxSamples_ :
       Attr.int "numsamples" numSamples_ :
       Attr.int "sampleformat" (intFromSampleFormat . sampleFormat_) :
       [])
      (concatMap blockToXML (blocks_ x))

blockToXML :: Block -> [[Tag.T Name.T String]]
blockToXML x =
   XML.tag "waveblock" x
      (Attr.int "start" blockStart_ :
       [])
   $
   XML.tag "pcmaliasblockfile" x
      (Attr.string "summaryfile" (summaryFile_ . blockFile_) :
       Attr.string "aliasfile" (aliasFile_ . blockFile_) :
       Attr.int "aliasstart" (aliasStart_ . blockFile_) :
       Attr.int "aliaslen" blockLength_ :
       Attr.int "aliaschannel" (aliasChannel_ . blockFile_) :
       Attr.float "min" (Summary.min_ . limits_ . blockFile_) :
       Attr.float "max" (Summary.max_ . limits_ . blockFile_) :
       Attr.float "rms" (Summary.rms_ . limits_ . blockFile_) :
       [])
      []


{- |
@maxSamples_@ must be at least 1024,
otherwise you get an error about clip values
if you load the project to Audacity.
However, 1024 is not necessarily a good value.
Audacity uses 524288 by default.
-}
{-
Alternatively we could omit the @maxsamples@ attribute
when writing the XML file.
The DTD says that the @maxsamples@ attribute is required,
but Audacity accepts when it is missing.
-}
pcmAliasSequence ::
   (Monad m) =>
   SampleFormat -> Int -> Int -> FilePath -> Int -> Summary.Monad m Sequence
pcmAliasSequence fmt blockSize totalSize path channel =
   liftM
      (\bs ->
         Sequence {
            maxSamples_ = blockSize,
            numSamples_ = totalSize,
            sampleFormat_ = fmt,
            blocks_ = bs
         }) $
   mapM
      (\start -> do
         (Summary.State n) <- Summary.reserve
         return $
            Block {
               blockStart_ = start,
               blockLength_ = min totalSize (start+blockSize) - start,
               blockFile_ =
                  PCMAliasBlockFile {
                     summaryFile_ = printf "e%05x.auf" n,
                     aliasFile_ = path,
                     aliasStart_ = start,
                     aliasChannel_ = channel,
                     limits_ = Summary.defltLimits
                  }
            }) $
   takeWhile (<totalSize) $
   iterate (blockSize+) 0

pcmAliasSequenceFromStorableVector ::
   (MonadIO m) =>
   Int -> FilePath -> Int -> SVL.Vector Float -> Summary.Monad m Sequence
pcmAliasSequenceFromStorableVector blockSize aliasFile channel =
   liftM (sequenceFromBlocksSize blockSize) .
   mapM (uncurry $ storeSummary aliasFile channel) .
   Summary.attachStarts .
   Summary.sequenceFromStorableVector blockSize

pcmAliasSequenceFromSummary ::
   (MonadIO m) =>
   FilePath -> Int -> [Summary.T] -> Summary.Monad m Sequence
pcmAliasSequenceFromSummary aliasFile channel =
   liftM sequenceFromBlocks .
   mapM (uncurry $ storeSummary aliasFile channel) .
   Summary.attachStarts


{- |
This type lets you specify how to order blocks of multi-channel sounds.
Both orders always work but Haskell's garbage collector works best,
if the order matches the order of the data production.
-}
data BlockOrder =
     Serial
       {- ^ All blocks of a channel are stored adjacently. -}
   | Interleaved
       {- ^ Blocks of channels are interleaved. -}
   deriving (Eq, Ord, Show, Enum, Bounded)


{- |
It is an unchecked error if StorableVectors have different lengths.
-}
pcmAliasSequencesFromStorableVectorChannels ::
   (MonadIO m) =>
   BlockOrder ->
   Int -> FilePath -> [SVL.Vector Float] -> Summary.Monad m [Sequence]
pcmAliasSequencesFromStorableVectorChannels order blockSize aliasFile =
   liftM (map (sequenceFromBlocksSize blockSize)) .
   blocksFromChannelSummaries order aliasFile .
   map (Summary.sequenceFromStorableVector blockSize)


pcmAliasSequencesFromChannelSummaries ::
   (MonadIO m) =>
   BlockOrder -> FilePath -> [[Summary.T]] -> Summary.Monad m [Sequence]
pcmAliasSequencesFromChannelSummaries order aliasFile =
   liftM (map sequenceFromBlocks) .
   blocksFromChannelSummaries order aliasFile

blocksFromChannelSummaries ::
   (MonadIO m) =>
   BlockOrder -> FilePath -> [[Summary.T]] -> Summary.Monad m [[Block]]
blocksFromChannelSummaries order aliasFile =
   let applyOrder f =
         case order of
            Serial -> f
            Interleaved -> liftM List.transpose . f . List.transpose
   in  applyOrder
         (mapM
            (mapM
               (\(channel, startBlock) ->
                  uncurry (storeSummary aliasFile channel) startBlock))) .
       zipWith (\channel -> map ((,) channel)) (iterate (1+) 0) .
       map Summary.attachStarts


sequenceFromBlocks :: [Block] -> Sequence
sequenceFromBlocks bs =
   let lens = map blockLength_ bs
   in  Sequence {
          maxSamples_ = NonEmpty.maximum $ 1024 !: lens,
          numSamples_ = sum lens,
          sampleFormat_ = FloatSample,
          blocks_ = bs
       }

sequenceFromBlocksSize :: Int -> [Block] -> Sequence
sequenceFromBlocksSize blockSize bs =
   Sequence {
      maxSamples_ = blockSize,
      numSamples_ = sum $ map blockLength_ bs,
      sampleFormat_ = FloatSample,
      blocks_ = bs
   }


storeSummary ::
   MonadIO m =>
   FilePath -> Int -> Int -> Summary.T -> Summary.Monad m Block
storeSummary aliasFile channel start
   (Summary.Cons {
      Summary.length_ = len, Summary.limits_ = limits,
      Summary.content_ = cont}) = do
   (Summary.State n) <- Summary.reserve
   summaryDir <- MR.ask
   let fileName = printf "e%07x.auf" n
   let dirName =
         case mapSnd (take 2) $ splitAt 3 fileName of
            (e, d) -> summaryDir </> e </> 'd':d
   liftIO $ do
      createDirectoryIfMissing True dirName
      IO.withBinaryFile (dirName </> fileName) IO.WriteMode $ \h -> do
         BS.hPut h $ BS.pack "AudacityBlockFile112"
         SVL.hPut h cont
   return $
      Block {
         blockStart_ = start,
         blockLength_ = len,
         blockFile_ =
            PCMAliasBlockFile {
               summaryFile_ = fileName,
               aliasFile_ = aliasFile,
               aliasStart_ = start,
               aliasChannel_ = channel,
               limits_ = limits
            }
      }
