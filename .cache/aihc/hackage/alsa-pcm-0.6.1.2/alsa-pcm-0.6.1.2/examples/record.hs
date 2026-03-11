import Sound.ALSA.PCM
         (SoundFmt(SoundFmt), copySound, sampleFreq,
          fileSoundSink, alsaSoundSource, )

import System.Environment (getArgs, )
import System.Exit (exitFailure, )
import System.IO (hPutStrLn, stderr, )
import Data.Int (Int16, )

bufSize :: Int
bufSize = 8192

soundFormat :: SoundFmt Int16
soundFormat = SoundFmt { sampleFreq = 8000 }

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> record file
            _      -> do hPutStrLn stderr "Usage: record <file.pcm>"
                         exitFailure

record :: FilePath -> IO ()
record file =
    do let source = alsaSoundSource "default" soundFormat
           sink   = fileSoundSink file
       copySound source sink bufSize
