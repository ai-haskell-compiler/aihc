import Sound.ALSA.PCM
         (SoundFmt(SoundFmt), copySound, sampleFreq,
          fileSoundSink, fileSoundSource,
          alsaSoundSink, alsaSoundSource, )

import Control.Concurrent (forkOS, threadDelay, )
import System.Environment (getArgs, )
import System.Exit (exitFailure, )
import System.IO (hPutStrLn, stderr, )
import Data.Int (Int16, )

bufSize :: Int
bufSize = 4096

soundFormat :: SoundFmt Int16
soundFormat = SoundFmt { sampleFreq = 8000 }

main :: IO ()
main = do args <- getArgs
          case args of
            [infile,outfile] -> duplex infile outfile
            _                ->
                do hPutStrLn stderr "Usage: duplex <input.pcm> <output.pcm>"
                   exitFailure

duplex :: FilePath -> FilePath -> IO ()
duplex infile outfile =
    do _ <- forkOS (play infile)
       _ <- forkOS (record outfile)
       threadDelay 5000000


play :: FilePath -> IO ()
play file =
    do let source = fileSoundSource file
           sink   = alsaSoundSink "default" soundFormat
       copySound source sink bufSize

record :: FilePath -> IO ()
record file =
    do let source = alsaSoundSource "default" soundFormat
           sink   = fileSoundSink file
       copySound source sink bufSize
