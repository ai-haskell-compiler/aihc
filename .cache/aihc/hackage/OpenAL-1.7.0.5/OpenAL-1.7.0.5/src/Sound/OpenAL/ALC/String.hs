{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.String
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.String (
   withALCString, peekALCString, peekALCStrings
) where

import Foreign.C.String ( withCString, peekCString )
import Foreign.Marshal.Array ( lengthArray0 )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )

import Sound.OpenAL.ALC.BasicTypes

--------------------------------------------------------------------------------

-- AL uses "Ptr ALCchar" instead of "CString" for strings, so some wrappers
-- are quite handy.

withALCString :: String -> (Ptr ALCchar -> IO a) -> IO a
withALCString str action = withCString str (action . castPtr)

peekALCString :: Ptr ALCchar -> IO String
peekALCString = peekCString . castPtr

peekALCStrings :: Ptr ALCchar -> IO [String]
peekALCStrings ptr = loop ptr []
   where loop p strs = do
            str <- peekALCString p
            if str == ""
               then return (reverse strs)
               else do
                  len <- lengthArray0 0 p
                  loop (p `plusPtr` (len + 1)) (str : strs)
