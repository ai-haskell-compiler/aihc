import Sound.ALSA.PCM
         (SoundFmt(SoundFmt), sampleFreq, soundSourceRead,
          SoundSource, alsaSoundSource, withSoundSource, )

import Foreign
         (allocaArray, peekArray,
          Storable, Ptr, castPtr, )
import Data.Int (Int16, )

bufSize :: Int
bufSize = 1000

inputFormat :: SoundFmt Int16
inputFormat = SoundFmt { sampleFreq = 8000 }


main :: IO ()
main = let source = alsaSoundSource "default" inputFormat
        in allocaArray     bufSize $ \buf  ->
           withSoundSource source  $ \from ->
               loop source from bufSize buf

-- | assumes that the file contains numbers in the host's byte order
loop :: SoundSource h Int16 -> h Int16 -> Int -> Ptr Int16 -> IO ()
loop src h n buf =
    do n' <- soundSourceRead src h (castPtr buf) n
       avg <- avgBuf buf n'
       putStrLn (replicate (avg `div` 20) '*')
       loop src h n buf

avgBuf :: (Storable a, Integral a) => Ptr a -> Int -> IO Int
avgBuf buf n = do xs <- peekArray n buf
                  let xs' = map (fromIntegral . abs) xs :: [Int]
                  return $ sum xs' `div` fromIntegral n
