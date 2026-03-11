{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Data.FilePlow
    ( PlowHandle(..), Handle, SeekMode(..)
    , seekUntil, seekUntilRev
    , MultiHandle, withMultiHandle
    )
where

import Control.Monad
import Data.IORef
import GHC.IO.Exception
import GHC.IO.Handle
import System.IO
import qualified Data.ByteString as BS
import qualified Data.Vector as V

class PlowHandle hdl where
    pSeek :: hdl -> SeekMode -> Integer -> IO ()
    pTell :: hdl -> IO Integer
    pGetChar :: hdl -> IO Char
    pGetLine :: hdl -> IO BS.ByteString
    pIsEOF :: hdl -> IO Bool
    pFileSize :: hdl -> IO Integer

instance PlowHandle Handle where
    pSeek = hSeek
    pTell = hTell
    pGetChar = hGetChar
    pGetLine = BS.hGetLine
    pIsEOF = hIsEOF
    pFileSize = hFileSize

data MultiHandle h
    = MultiHandle
    { mh_handles :: !(V.Vector h)
    , mh_currentIndex :: !(IORef Int)
    }

-- | Seek until a certain charater is reached in reverse
seekUntilRev :: PlowHandle hdl => hdl -> (Char -> Bool) -> IO Bool
seekUntilRev hdl pp =
    do let getLoop =
               do p <- pTell hdl
                  if p == 0
                      then pure False
                      else do x <- pGetChar hdl
                              if pp x
                                  then pure True
                                  else do pSeek hdl RelativeSeek (-2)
                                          getLoop
       getLoop

-- | Seek until a certain charater is reached
seekUntil :: PlowHandle hdl => hdl -> (Char -> Bool) -> IO Bool
seekUntil hdl pp =
    do let getLoop =
               do eof <- pIsEOF hdl
                  if eof
                      then pure False
                      else do x <- pGetChar hdl
                              if pp x then pure True else getLoop
       getLoop

instance (PlowHandle h) => PlowHandle (MultiHandle h) where
    pFileSize mh =
        V.foldM' (\total hdl -> (+ total) <$> pFileSize hdl) 0 (mh_handles mh)
    {-# INLINE pFileSize #-}

    pIsEOF mh =
        do idx <- readIORef (mh_currentIndex mh)
           let ct = V.length (mh_handles mh)
           if idx >= ct - 1
              then pIsEOF $ mh_handles mh V.! (ct - 1)
              else pure False
    {-# INLINE pIsEOF #-}

    pGetChar mh =
        do h <- getCurrentHandle "pGetChar" mh
           pGetChar h
    {-# INLINE pGetChar #-}

    pGetLine mh =
        do h <- getCurrentHandle "pGetLine" mh
           pGetLine h
    {-# INLINE pGetLine #-}

    pSeek mh sm val =
        case sm of
          AbsoluteSeek -> absoluteSeek mh val
          RelativeSeek -> relativeSeek mh val
          SeekFromEnd -> seekFromEnd mh val
    {-# INLINE pSeek #-}

    pTell mh =
        do idx <- readIORef (mh_currentIndex mh)
           let h = mh_handles mh V.! idx
           localPos <- pTell h
           foldM
               (\total i -> (+ total) <$> pFileSize (mh_handles mh V.! i))
               localPos [0 .. (idx - 1)]
    {-# INLINE pTell #-}

seekFromEnd :: (PlowHandle h) => MultiHandle h -> Integer -> IO ()
seekFromEnd mh tval =
    let ct = V.length (mh_handles mh)
        seekLoop !tgtVal !idx
            | idx >= ct || idx < 0 =
              ioException (IOError Nothing EOF "seekFromEnd" "No more file handles" Nothing Nothing)
            | otherwise =
              do let h = mh_handles mh V.! idx
                 size <- pFileSize h
                 if abs tgtVal >= size
                     then seekLoop ((-1) * (abs tgtVal - size)) (idx - 1)
                     else do writeIORef (mh_currentIndex mh) idx
                             pSeek h SeekFromEnd tgtVal
    in seekLoop tval (ct - 1)

relativeSeek :: (PlowHandle h) => MultiHandle h -> Integer -> IO ()
relativeSeek mh tval =
    do myIdx <- readIORef (mh_currentIndex mh)
       let localH = mh_handles mh V.! myIdx
       localPos <- pTell localH
       pSeek localH AbsoluteSeek 0
       let seekVal = tval + localPos
       seekHelp mh seekVal myIdx

absoluteSeek :: PlowHandle h => MultiHandle h -> Integer -> IO ()
absoluteSeek mh tval =
    seekHelp mh tval 0

seekHelp :: PlowHandle h => MultiHandle h -> Integer -> Int -> IO ()
seekHelp mh t i =
    let ct = V.length (mh_handles mh)
        seekLoop !tgtVal !idx
            | idx >= ct =
              let msg = "No more file handles, desired pos: " ++ show t
              in ioException (IOError Nothing EOF "seekHelp" msg Nothing Nothing)
            | tgtVal < 0 && idx > 0 =
              do let h = mh_handles mh V.! (idx - 1)
                 size <- pFileSize h
                 pSeek h AbsoluteSeek 0
                 seekLoop (tgtVal + size) (idx - 1)
            | otherwise =
              do let h = mh_handles mh V.! idx
                 size <- pFileSize h
                 if tgtVal >= size
                     then seekLoop (tgtVal - size) (idx + 1)
                     else do writeIORef (mh_currentIndex mh) idx
                             pSeek h AbsoluteSeek tgtVal
    in seekLoop t i

getCurrentHandle :: PlowHandle h => String -> MultiHandle h -> IO h
getCurrentHandle helpTxt mh =
    do startIdx <- readIORef (mh_currentIndex mh)
       let ct = V.length (mh_handles mh)
           gotoUsefulHdl !i !nh
               | i >= ct =
                     ioException (IOError Nothing EOF ("currentHandle/" ++ helpTxt) "No more file handles" Nothing Nothing)
               | otherwise =
                     do let h = mh_handles mh V.! i
                        when nh $
                            pSeek h AbsoluteSeek 0
                        eof <- pIsEOF h
                        if eof
                            then gotoUsefulHdl (i + 1) True
                            else do writeIORef (mh_currentIndex mh) i
                                    pure h
       gotoUsefulHdl startIdx False


withMultiHandle :: [FilePath] -> (MultiHandle Handle -> IO a) -> IO a
withMultiHandle files go =
    loop [] files
    where
      loop accum xs =
          case xs of
            [] ->
                do r <- newIORef 0
                   go (MultiHandle (V.fromList (reverse accum)) r)
            (fp : more) ->
                withFile fp ReadMode $ \hdl ->
                loop (hdl : accum) more
