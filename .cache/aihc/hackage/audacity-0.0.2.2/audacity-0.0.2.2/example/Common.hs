module Common where

import qualified Sound.Audacity.Project.Track.Wave.Summary as ProjectWaveSummary
import qualified Sound.Audacity.Project.Track.Wave as ProjectWaveTrack
import qualified Sound.SoxLib as SoxLib

import qualified Data.StorableVector.Lazy as SVL
import Foreign.Storable (peek, )

import qualified Options.Applicative.Types as OPInt
import qualified Options.Applicative as OP
import Control.Applicative ((<*>), )

import qualified System.FilePath as FilePath
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Data.NonEmpty as NonEmpty
import Data.NonEmpty ((!:), )
import Data.Maybe (fromMaybe, )
import Data.Monoid ((<>), )

import Data.Int (Int32, )


summaryName :: FilePath -> String
summaryName path = FilePath.dropExtension path ++ "_data"

waveSummary ::
   ProjectWaveSummary.Handle ->
   Int -> FilePath -> SVL.Vector Int32 ->
   IO [ProjectWaveTrack.Sequence]
waveSummary sh numChan input sig = do
   cwd <- Dir.getCurrentDirectory
   ProjectWaveSummary.usingHandle sh .
      ProjectWaveTrack.pcmAliasSequencesFromStorableVectorChannels
         ProjectWaveTrack.Interleaved 262144 (cwd </> input) .
      SVL.deinterleave numChan .
      SVL.map (\x -> fromIntegral x / 2^(31::Int))
         $ sig


defaultSampleRate :: SoxLib.Rate
defaultSampleRate = 44100


withSound ::
   FilePath ->
   (SoxLib.Format SoxLib.ReadMode ->
    Maybe SoxLib.Rate -> Int -> SVL.Vector Int32 -> IO a) ->
   IO a
withSound path act =
   SoxLib.withRead SoxLib.defaultReaderInfo path $ \fmtPtr -> do
      fmt <- peek fmtPtr
      let sigInfo = SoxLib.signalInfo fmt
          numChan = fromMaybe 1 $ SoxLib.channels sigInfo
          rate = SoxLib.rate sigInfo
      act fmt rate numChan =<<
         SoxLib.readStorableVectorLazy fmtPtr
            (case SVL.defaultChunkSize of
               SVL.ChunkSize size -> SVL.ChunkSize $ numChan * size)


writerInfoFromFormat ::
   SoxLib.Format mode -> SoxLib.WriterInfo
writerInfoFromFormat fmtIn =
   SoxLib.defaultWriterInfo {
      SoxLib.writerSignalInfo = Just $ SoxLib.signalInfo fmtIn
   }


some :: OP.Parser a -> OP.Parser (NonEmpty.T [] a)
some p = OPInt.fromM $ OP.liftA2 (!:) (OPInt.oneM p) (OPInt.manyM p)

parseArgs :: OP.Parser (NonEmpty.T [] FilePath, FilePath)
parseArgs =
   OP.liftA2 (,)
      (some (OP.strArgument (OP.metavar "SRC")))
      (OP.strOption (OP.long "output" <> OP.metavar "DST"))

info :: String -> OP.Parser a -> OP.ParserInfo a
info desc parser =
   OP.info
      (OP.helper <*> parser)
      (OP.fullDesc <> OP.progDesc desc)

multiArgs :: String -> IO (NonEmpty.T [] FilePath, FilePath)
multiArgs desc = OP.execParser $ info desc parseArgs
