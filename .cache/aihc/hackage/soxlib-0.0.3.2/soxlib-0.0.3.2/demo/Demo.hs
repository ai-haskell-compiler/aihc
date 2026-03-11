module Main where

import qualified Sound.SoxLib as SoxLib

import qualified Data.StorableVector as SV
import Foreign.Storable (peek, )


writerInfo :: Int -> SoxLib.WriterInfo
writerInfo numChans =
   SoxLib.defaultWriterInfo {
      SoxLib.writerSignalInfo = Just $
         SoxLib.defaultSignalInfo {
            SoxLib.rate = Just 44100,
            SoxLib.channels = Just numChans,
            SoxLib.precision = Just 16
         }
   }

writeMono :: FilePath -> IO ()
writeMono filename =
   SoxLib.withWrite (writerInfo 1) filename $ \fmt ->
      let chunk = SV.iterateN 256 (128*256*256+) 0
      in  do SoxLib.writeStorableVector fmt chunk
             SoxLib.writeStorableVector fmt (SV.reverse chunk)

writeStereo :: FilePath -> IO ()
writeStereo filename =
   SoxLib.withWrite (writerInfo 2) filename $ \fmt ->
      let chunk = SV.iterateN 256 (128*256*256+) 0
      in  SoxLib.writeStorableVector fmt $
          SV.interleave [chunk, SV.reverse chunk]

readInfo :: FilePath -> IO ()
readInfo filename =
   SoxLib.withRead SoxLib.defaultReaderInfo filename $ \fmtPtr -> do
      fmt <- peek fmtPtr
      print $ SoxLib.encodingInfo fmt
      let si = SoxLib.signalInfo fmt
      print si
      case SoxLib.length si of
         Nothing -> putStrLn "unknown length"
         Just n ->
            print . SV.map (flip div (256*256*256)) =<<
               SoxLib.readStorableVector fmtPtr n

main :: IO ()
main = SoxLib.formatWith $ do
   let monoName = "mono.wav"
   let stereoName = "stereo.wav"
   writeMono monoName
   writeStereo stereoName
   readInfo monoName
   readInfo stereoName
