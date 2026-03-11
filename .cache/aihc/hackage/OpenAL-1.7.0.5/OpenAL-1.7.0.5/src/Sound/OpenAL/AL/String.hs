{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.String
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.String (
   withALString, peekALString
) where

import Foreign.C.String ( withCString, peekCString )
import Foreign.Ptr ( Ptr, castPtr )

import Sound.OpenAL.AL.BasicTypes

--------------------------------------------------------------------------------

-- AL uses "Ptr ALchar" instead of "CString" for strings, so some wrappers
-- are quite handy.

withALString :: String -> (Ptr ALchar -> IO a) -> IO a
withALString str action = withCString str (action . castPtr)

peekALString :: Ptr ALchar -> IO String
peekALString = peekCString . castPtr
