import Sound.ALSA.PCM
         (SoundFmt(SoundFmt), copySound, sampleFreq,
          fileSoundSource, alsaSoundSink, )

import System.Environment (getArgs, )

import Data.Int (Int16, )

bufSize :: Int
bufSize = 8192

soundFormat :: SoundFmt Int16
soundFormat = SoundFmt { sampleFreq  = 8000 }

main :: IO ()
main =
   mapM_ play =<< getArgs

play :: FilePath -> IO ()
play file =
   copySound
      (fileSoundSource file)
      -- "default" allows mixing with other ALSA programs
      (alsaSoundSink "default" soundFormat)
      bufSize
