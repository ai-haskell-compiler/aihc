{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.File (
   render,
   renderToInt16,
   renderMonoToInt16,
   renderStereoToInt16,
   write,
   writeToInt16,
   writeMonoToInt16,
   writeStereoToInt16,
   writeRaw,
   writeRawCompressed,
   rawToAIFF,
   compress,
   readAIFFMono,
   readMonoFromInt16,
   -- will no longer be exported
   getInt16List,
   ) where

import qualified Sound.Sox.Convert as Convert
import qualified Sound.Sox.Frame as Frame
import qualified Sound.Sox.Frame.Stereo as Stereo
import qualified Sound.Sox.Option.Format as SoxOpt
import qualified Sound.Sox.Write as Write
import qualified Sound.Sox.Read as Read
import qualified Sound.Sox.Signal.List as SoxList

import qualified Synthesizer.Plain.IO as FileL
import qualified Synthesizer.Plain.Builder as Builder

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as Get
import qualified Synthesizer.Basic.Binary as BinSmp
import Foreign.Storable (Storable, )
import Data.Int (Int16, )

import qualified System.FilePath as FilePath
import System.Process (rawSystem, )
import System.Exit (ExitCode, )

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM2, )

import Data.Monoid (mconcat, )

import qualified Algebra.ToInteger          as ToInteger
import qualified Algebra.RealRing           as RealRing
import qualified Algebra.Field              as Field

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
See 'write'.
-}
render ::
   (Storable int, Frame.C int, ToInteger.C int, Bounded int,
    RealRing.C a, BinSmp.C v) =>
   Builder.Put int -> FilePath -> a -> (a -> [v]) -> IO ExitCode
render put fileName sampleRate renderer =
   write put fileName sampleRate (renderer sampleRate)

renderToInt16 :: (RealRing.C a, BinSmp.C v) =>
   FilePath -> a -> (a -> [v]) -> IO ExitCode
renderToInt16 fileName sampleRate renderer =
   writeToInt16 fileName sampleRate (renderer sampleRate)

renderMonoToInt16 :: (RealRing.C a) =>
   FilePath -> a -> (a -> [a]) -> IO ExitCode
renderMonoToInt16 fileName sampleRate renderer =
   writeMonoToInt16 fileName sampleRate (renderer sampleRate)

renderStereoToInt16 :: (RealRing.C a) =>
   FilePath -> a -> (a -> [(a,a)]) -> IO ExitCode
renderStereoToInt16 fileName sampleRate renderer =
   writeStereoToInt16 fileName sampleRate (renderer sampleRate)


{- |
The output format is determined by SoX by the file name extension.
The sample precision is determined by the provided 'Builder.Put' function.

Example:

> import qualified Synthesizer.Plain.Builder as Builder
>
> write (Builder.put :: Builder.Put Int16) "test.aiff" 44100 sound
-}
write ::
   (Storable int, Frame.C int, ToInteger.C int, Bounded int,
    RealRing.C a, BinSmp.C v) =>
   Builder.Put int -> FilePath -> a -> [v] -> IO ExitCode
write put fileName sampleRate signal =
   writeRaw
      (SoxOpt.numberOfChannels (BinSmp.numberOfSignalChannels signal))
      fileName
      sampleRate
      (Builder.run . mconcat . map (BinSmp.outputFromCanonical put) $
       signal)

writeToInt16 :: (RealRing.C a, BinSmp.C v) =>
   FilePath -> a -> [v] -> IO ExitCode
writeToInt16 =
   write (Builder.put :: Builder.Put Int16)

writeMonoToInt16 :: (RealRing.C a) =>
   FilePath -> a -> [a] -> IO ExitCode
writeMonoToInt16 fileName sampleRate signal =
   writeRaw SoxOpt.none fileName sampleRate
      (map BinSmp.int16FromCanonical signal)

writeStereoToInt16 :: (RealRing.C a) =>
   FilePath -> a -> [(a,a)] -> IO ExitCode
writeStereoToInt16 fileName sampleRate signal =
   writeRaw SoxOpt.none fileName sampleRate
      (map (fmap BinSmp.int16FromCanonical . uncurry Stereo.cons) signal)

writeRaw :: (RealRing.C a, Frame.C v, Storable v) =>
   SoxOpt.T -> FilePath -> a -> [v] -> IO ExitCode
writeRaw opts fileName sampleRate signal =
   Write.extended SoxList.put opts SoxOpt.none fileName (round sampleRate) signal

{- |
You hardly need this routine
since you can use a filename with @.mp3@ or @.ogg@
extension for 'writeRaw'
and SoX will do the corresponding compression for you.
-}
writeRawCompressed :: (RealRing.C a, Frame.C v, Storable v) =>
   SoxOpt.T -> FilePath -> a -> [v] -> IO ExitCode
writeRawCompressed opts fileName sampleRate signal =
   Exc.toExitCodeT $
   do Exc.fromExitCodeT $ writeRaw opts fileName sampleRate signal
      Exc.fromExitCodeT $ compress fileName


{-# DEPRECATED rawToAIFF "If you want to generate AIFF, then just write to files with .aiff filename extension. If you want to convert files to AIFF, use Sound.Sox.Convert." #-}
rawToAIFF :: (RealRing.C a) =>
   FilePath -> SoxOpt.T -> a -> Int -> IO ExitCode
rawToAIFF fileName soxOptions sampleRate numChannels =
   let fileNameRaw  = fileName ++ ".sw"
       fileNameAIFF = fileName ++ ".aiff"
   in  Convert.simple
          (mconcat $
           soxOptions :
           SoxOpt.sampleRate (round sampleRate) :
           SoxOpt.numberOfChannels numChannels :
           [])
          fileNameRaw
          SoxOpt.none fileNameAIFF

compress :: FilePath -> IO ExitCode
compress fileName = Exc.toExitCodeT $
   do Exc.fromExitCodeT $ rawSystem "oggenc" ["--quality", "5", fileName]
      Exc.fromExitCodeT $ rawSystem "lame"
         ["-h", fileName, FilePath.replaceExtension fileName "mp3"]


{-# DEPRECATED readAIFFMono "Use readMonoFromInt16 instead" #-}
{-
This implementation doesn't work properly.
It seems like readFile is run
after all system calls to Sox are performed.
Aren't the calls serialized?

readAIFFMono :: (RealRing.C a, Floating a) => FilePath -> IO [a]
readAIFFMono file =
   do putStrLn ("sox "++file++" /tmp/sample.sw")
      system ("sox "++file++" /tmp/sample.sw")
      str <- readFile "/tmp/sample.sw"
      return (binaryToSignalMono str)
-}
readAIFFMono :: (Field.C a) => FilePath -> IO [a]
readAIFFMono file =
   let tmp = FilePath.replaceExtension file "s16"
   in  Exc.resolveT (const $ return []) $ do
          -- lift $ putStrLn ("sox "++file++" "++tmp)
          Exc.fromExitCodeT $ Convert.simple SoxOpt.none file SoxOpt.none tmp
          fmap (map BinSmp.int16ToCanonical) $
             lift $ FileL.readInt16StreamStrict tmp


{- |
I suspect we cannot handle file closing properly.
-}
readMonoFromInt16 :: (Field.C a) => FilePath -> IO [a]
readMonoFromInt16 fileName =
   Read.open SoxOpt.none fileName >>=
   Read.withHandle1 (fmap (Get.runGet getInt16ListPrivate) . B.hGetContents) >>=
   return . map BinSmp.int16ToCanonical

{-# DEPRECATED getInt16List "This function will no longer be exported" #-}
getInt16List, getInt16ListPrivate :: Get.Get [Int16]
getInt16List = getInt16ListPrivate
getInt16ListPrivate =
   do b <- Get.isEmpty
      if b
        then return []
        else liftM2 (:) (fmap fromIntegral Get.getWord16host) getInt16ListPrivate
